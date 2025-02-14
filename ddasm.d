/*
 * Copyright © 2021-2023, David Priver
 */
import core.stdc.string: strlen, strerror, memcpy, memset;
import core.stdc.stdio: fprintf, stdout, stderr, fread, stdin, FILE, fwrite, fflush, fopen, fputs, fgets, fclose;
import core.stdc.stdlib: calloc, malloc, free, atoi;

import dlib.stringbuilder: StringBuilder;
import dlib.argparse;
import dlib.zstring: ZString;
import dlib.get_input: get_input_line, LineHistory;
import dlib.term_util: get_cols, stdin_is_interactive;
import dlib.file_util: read_file, FileFlags, FileResult;
import dlib.allocator;
import dlib.box: Box;
import dlib.str_util: endswith;
import dlib.table;
import dlib.aliases;


import dvm.dvm_defs: Fuzzing, uintptr_t;
import dvm.dvm_linked: LinkedModule, Function, FunctionType, FunctionTable, FunctionInfo;
import dvm.dvm_unlinked: UnlinkedModule;
import dvm.dvm_machine: Machine, RunFlags;
import dvm.dvm_linker: link_module;
import dvm.dvm_regs: RegisterNames;

import dasm.dasm_parser: parse_asm_string;
import dvm_modules.builtins;
import c;



static if(Fuzzing){
    __gshared RecordingAllocator!Mallocator recorder;
    extern(C)
    int LLVMFuzzerTestOneInput(const ubyte *Data, size_t Size){
        const char* d = cast(const char*)Data;
        const char[] data = d[0 .. Size];

        UnlinkedModule prog;
        VAllocator va = VAllocator.from!(GlobalAllocator!recorder);
        int err = parse_asm_string(&va, data, &prog);
        va.free_all;
        return 0;
    }
}
else {

extern(C)
int main(int argc, char** argv){
    bool disassemble = false;
    bool early_exit = false;
    bool force_interactive = false;
    bool no_interactive = false;
    bool debugger = false;
    bool highlevel = false;
    bool no_run = false;
    uintptr_t[RegisterNames.RARGMAX-RegisterNames.RARG1] rargs;
    ZString[rargs.length] rargs_s;
    ZString sourcefile;
    with(ArgParseFlags) with(ArgToParseFlags) {
    ArgToParse[1] pos_args = [
        {
            name: "source",
            help: "Source file (.dasm file) to read from.
            If not given, will read from stdin.",
            dest: ARGDEST(&sourcefile),
        },
    ];
    ArgToParse[8] kw_args = [
        {
            name: "--force-interactive", altname: "-i",
            help: "Force interactive mode when reading from stdin.",
            dest: ARGDEST(&force_interactive),
        },
        {
            name: "--no-interactive",
            help: "Force non-interactive mode when reading from stdin.",
            dest: ARGDEST(&no_interactive),
        },
        {
            name: "--dev-null",
            help: "Builtin funcs don't print anymore",
            dest: ARGDEST(&dvm_modules.builtins.devnull),
        },
        {
            name: "--disassemble-every-op", altname: "--dis",
            help: "Print out the disassembly before executing each op",
            dest: ARGDEST(&disassemble),
        },
        {
            name: "--debug", altname: "-g",
            help: "Executes in debug mode",
            dest: ARGDEST(&debugger),
        },
        {
            name: "--ds", altname: "--davescript",
            help: "Force interpretation of the source as
            davescript instead of dasm",
            dest: ARGDEST(&highlevel),
        },
        {
            name: "-a", altname: "--args",
            help: "Set rarg1 to ... to the following values (coerced to integers if possible)",
            dest: ARGDEST(&rargs_s[0]),
            num: NumRequired(0, rargs_s.length),
            flags: SHOW_DEFAULT,
        },
        {
            name: "-y", altname: "--dry-run",
            help: "Compile and link, but don't run the script/dasm",
            dest: ARGDEST(&no_run),
        }

    ];
    enum {HELP=0, VERSION=1}
    ArgToParse[2] early_args = [
        {
            name: "-h", altname: "--help",
            help: "Print this help and exit.",
        },
        {
            name: "-v", altname: "--version",
            help: "Print the version and exit.",
        },
    ];
    int columns = get_cols();
    ArgParser parser = {
        name: argc?argv[0][0..strlen(argv[0])]:"ddasm",
        description: "A dasm interpreter",
        early_out: early_args,
        positional: pos_args,
        keyword: kw_args,
    };
    switch(check_for_early_out_args(&parser, argc?argv[1..argc]:null)){
        case HELP:
            print_argparse_help(&parser, columns);
            return 0;
        case VERSION:
            if(!Fuzzing)fprintf(stdout, "ddasm V1337\n");
            return 0;
        default:
            break;
    }
    auto error = parser.parse_args(argc?argv[1..argc]:null);
    if(error) {
        print_argparse_error(&parser, error);
        fprintf(stderr, "Use --help to see usage.\n");
        return error;
    }
    }
    foreach(size_t i, ZString arg; rargs_s[]){
        if(arg.ptr){
            import dlib.parse_numbers: parse_unsigned_human, IntegerResult;
            IntegerResult!ulong ir = parse_unsigned_human(arg[]);
            if(ir.errored)
                rargs[i] = cast(uintptr_t)arg.ptr;
            else
                rargs[i] = ir.value;
        }
        else
            break;
    }
    Box!(str) btext;
    if(sourcefile.length){
        FileResult fe = read_file(sourcefile.ptr);
        if(fe.errored){
            version(Posix)
                fprintf(stderr, "Unable to read from '%s': %s\n", sourcefile.ptr, strerror(fe.errored));
            // TODO: get error message from windows
            version(Windows)
                fprintf(stderr, "Unable to read from '%s'\n", sourcefile.ptr);
            return fe.errored;
        }
        btext = fe.value.as!str;
    }
    else if(!no_interactive && (force_interactive || stdin_is_interactive())){
        StringBuilder sb = {allocator: MALLOCATOR};
        LineHistory history;
        history.allocator = MALLOCATOR;
        const char* HISTORYFILE = "ddasm.history";
        history.load_history(HISTORYFILE);
        scope(exit) {
            history.dump(HISTORYFILE);
            history.cleanup;
        }
        char[4096] buff = void;
        for(;;){
            ptrdiff_t len = get_input_line(&history, "> ", buff[]);
            if(len < 0) break;
            if(len == 1 && buff[0] == 'q') break;
            if(len){
                history.add_line(buff[0..len]);
                sb.write(buff[0..len]);
            }
            sb.write('\n');
        }
        btext = sb.detach.as!str;
    }
    else {
        StringBuilder sb = {allocator: MALLOCATOR};
        for(;;){
            enum N = 4096;
            sb.ensure_additional(N);
            char* buff= sb.data + sb.cursor;
            size_t numread = fread(buff, 1, N, stdin);
            sb.cursor += numread;
            if(numread != N)
                break;
        }
        if(!sb.cursor)
            sb.write(' ');
        btext = sb.detach.as!str;
    }
    if(highlevel || sourcefile[].endswith(".ds")){
        static import dscript.dscript;
        static import dscript_to_dasm;
        dscript.dscript.powerup;
        Box!(char[]) dasmtext = {allocator:MALLOCATOR};
        ubyte[] data = btext.as!(ubyte[]).data;
        int err = dscript_to_dasm.compile_to_dasm(data, &dasmtext);
        if(err) return err;
        btext.dealloc();
        btext = Box!str(btext.allocator, dasmtext.data);
        dscript.dscript.powerdown;
    }
    UnlinkedModule prog;
    int err = parse_asm_string(MALLOCATOR, btext.data, &prog);
    if(err){
        fprintf(stderr, "Parsing failed\n");
        return err;
    }

    LinkedModule linked_prog = {source_text: btext, name: prog.name};
    {
        void find_loc(const char* first_char, out str fn, out int line, out int column){
            import dasm.dasm_tokenizer: Tokenizer;
            import dasm.dasm_token: Token, TokenType;
            str text = btext.data;
            Tokenizer tokenizer = Tokenizer.from(text);
            Token tok = tokenizer.current_token_and_advance;
            while(tok._text != first_char){
                tok = tokenizer.current_token_and_advance;
                if(tok.type == TokenType.EOF){
                    if(!Fuzzing)fprintf(stderr, "Unable to find: %p: %s\n", first_char, first_char);
                    fn = "unknown";
                    line = -1;
                    column = -1;
                    return;
                }
            }
            fn = sourcefile[];
            if(!fn.length)
                fn = "(stdin)";
            line = tok.line;
            column = tok.column;
        }
        ArenaAllocator arena = ArenaAllocator(MALLOCATOR);
        scope(exit) arena.free_all;
        Allocator temp = arena.allocator();
        Table!(str, LinkedModule*) loaded = {data:{allocator: temp}};
        foreach(imp; prog.imports[]){
            import dvm_modules.builtins;
            switch(imp){
                case "io":
                    loaded["io"] = get_io_module();
                    break;
                case "mem":
                    loaded["mem"] = get_mem_module();
                    break;
                case "misc":
                    loaded["misc"] = get_misc_module();
                    break;
                case "SDL":
                    static import dvm_modules.sdl;
                    LinkedModule* sdl_module = dvm_modules.sdl.get_module();
                    if(!sdl_module) goto default;
                    loaded["SDL"] = sdl_module;
                    break;
                default:
                    fprintf(stderr, "Unknown module: '%.*s'\n", cast(int)imp.length, imp.ptr);
                    return 1;
            }
        }
        err = link_module(MALLOCATOR, temp, BUILTINS, &prog, &linked_prog, &find_loc, &loaded);
    }
    if(err){
        fprintf(stderr, "Linking failed\n");
        return err;
    }
    if(!linked_prog.start){
        fprintf(stderr, "Program needs a 'start' function as an entry point\n");
        return 1;
    }
    LinkedAllocator recorder = {base_allocator:MALLOCATOR};
    // RecordingAllocator!Mallocator recorder;
    Machine machine = {allocator: recorder.allocator()};
    machine.registers[RegisterNames.RARG1 .. RegisterNames.RARGMAX] = rargs[];
    if(no_run)
        return 0;
    if(debugger && disassemble)
        err = machine.run!(RunFlags.DEBUG|RunFlags.DISASSEMBLE_EACH)(&linked_prog, 1024*1024);
    else if(debugger)
        err = machine.run!(RunFlags.DEBUG)(&linked_prog, 1024*1024);
    else if(disassemble)
        err = machine.run!(RunFlags.DISASSEMBLE_EACH)(&linked_prog, 1024*1024);
    else
        err = machine.run!(RunFlags.NONE)(&linked_prog, 1024*1024);

    if(err){
        fprintf(stderr, "Running failed\n");
        return err;
    }
    return 0;
}
}


FunctionTable*
BUILTINS(){
    __gshared bool initialized = false;
    __gshared FunctionTable table;
    if(!initialized){
        initialized = true;
        table.data.allocator = MALLOCATOR;
    }
    return &table;
}
