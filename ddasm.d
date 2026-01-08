/*
 * Copyright © 2021-2023, David Priver
 */
import core.stdc.string: strlen, strerror, memcpy, memset;
import core.stdc.stdio: fprintf, stdout, stderr, fread, stdin, FILE, fwrite, fflush, fopen, fputs, fgets, fclose, getchar;
import core.stdc.stdlib: calloc, malloc, free, atoi;

import dlib.stringbuilder: StringBuilder;
import dlib.argparse;
import dlib.zstring: ZString;
import dlib.get_input: get_input_line, LineHistory;
import dlib.term_util: get_cols, stdin_is_interactive;
import dlib.allocator;
import dlib.barray: Barray, make_barray;
import dlib.box: Box, boxed;
import dlib.str_util: endswith;
import dlib.table: Table;
import dlib.aliases;
import dlib.logger: Logger;

import dvm.dvm_defs: Fuzzing, uintptr_t;
import dvm.dvm_linked: LinkedModule, Function, FunctionType, FunctionTable, FunctionInfo;
import dvm.dvm_unlinked: UnlinkedModule;
import dvm.dvm_machine: Machine, RunFlags;
import dvm.dvm_linker: link_module;
import dlib.file_cache: FileCache, FileError;
import dvm.dvm_regs: RegisterNames;

import dasm.dasm_parser: parse_asm_string;
import dvm_modules.builtins;

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

enum Language {
    UNSET,
    DASM,
    DS,
    C,
}
extern(C)
int main(int argc, char** argv){
    FileCache file_cache = FileCache(MALLOCATOR);
    Logger logger;
    bool disassemble = false;
    bool early_exit = false;
    bool force_interactive = false;
    bool no_interactive = false;
    bool debugger = false;
    bool no_run = false;
    bool codegen_only = false;
    bool parse_only = false;
    bool preprocess = false;
    Language lang = Language.UNSET;
    uintptr_t[RegisterNames.RARGMAX-RegisterNames.RARG1] rargs;
    ZString[rargs.length] rargs_s;
    ZString sourcefile;
    Barray!str include_paths = make_barray!str(MALLOCATOR);
    scope(exit) include_paths.cleanup();
    Barray!str framework_paths = make_barray!str(MALLOCATOR);
    scope(exit) framework_paths.cleanup();
    Barray!str lib_paths = make_barray!str(MALLOCATOR);
    scope(exit) lib_paths.cleanup();
    Barray!str force_includes = make_barray!str(MALLOCATOR);
    scope(exit) force_includes.cleanup();

    with(ArgParseFlags) with(ArgToParseFlags) {
    ArgToParse[1] pos_args = [
        {
            name: "source",
            help: "Source file to read from.
            If not given, will read from stdin.
            Will be interpreted based on file extension (default is C).",
            dest: ARGDEST(&sourcefile),
        },
    ];
    version(OSX)
        bool hide_framework = false;
    else
        bool hide_framework = true;
    ArgToParse[16] kw_args = [
        {
            name: "-I",
            help: "Add directory to include search path (for C files). Can be specified multiple times.",
            dest: ArgUser((str path) { include_paths ~= path; return 0; }, "path"),
            num: NumRequired(0, int.max),
        },
        {
            name: "-F",
            help: "Add directory to framework search path (for C files). Can be specified multiple times.",
            dest: ArgUser((str path) { framework_paths ~= path; return 0; }, "path"),
            num: NumRequired(0, int.max),
            flags: hide_framework?ArgToParseFlags.HIDDEN:ArgToParseFlags.ARG_TO_PARSE_NONE,
        },
        {
            name: "-L",
            help: "Add directory to library search path (for dlopen). Can be specified multiple times.",
            dest: ArgUser((str path) { lib_paths ~= path; return 0; }, "path"),
            num: NumRequired(0, int.max),
        },
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
            name: "--lang",
            help: "Force interpretation of the source
                   as this language.",
            dest: ArgUser((str l) { 
                switch(l){
                    case "c":
                    case "C":
                        lang = Language.C;
                        break;
                    case "dasm":
                    case "DASM":
                        lang = Language.DASM;
                        break;
                    case "ds":
                    case "davescript":
                        lang = Language.DS;
                        break;
                    default:
                        return 1;
                }
                return 0; 
            }, "language"),
        },
        {
            name: "--ds", altname: "--davescript",
            help: "Force interpretation of the source as
            davescript instead of dasm",
            dest: ArgAction((){ lang = Language.DS; return 0;}),
        },
        {
            name: "--dasm",
            help: "Force interpretation of the source as dasm",
            dest: ArgAction((){ lang = Language.DASM; return 0;}),
        },
        {
            name: "-c",
            help: "Force interpretation of the source as C",
            dest: ArgAction((){ lang = Language.C; return 0;}),
        },
        {
            name: "-a", altname: "--args",
            // The integer coercion is honestly pretty janky, we'll remove it
            // after we fix the examples.
            help: "Set rarg1 to ... to the following values (coerced to integers if possible)",
            dest: ARGDEST(&rargs_s[0]),
            num: NumRequired(0, rargs_s.length),
            flags: SHOW_DEFAULT,
        },
        {
            name: "-y", altname: "--dry-run",
            help: "Compile and link, but don't run the script/dasm",
            dest: ARGDEST(&no_run),
        },
        {
            name: "-S", altname: "--codegen-only",
            help: "Compile C to DASM text and output, don't link or run",
            dest: ARGDEST(&codegen_only),
        },
        {
            name: "--parse-code-only",
            help: "Parse DASM but don't link or run",
            dest: ARGDEST(&parse_only),
        },
        {
            name: "-include",
            help: "Include file(s) before processing source (for C files).",
            dest: ArgUser((str path) { force_includes ~= path; return 0; }, "file"),
            num: NumRequired(0, int.max),
        },
    ];
    // Only have -F arg on macos
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
    str source_text;
    if(sourcefile.length){
        const(ubyte)[] file_data;
        if(file_cache.read_file(sourcefile[], file_data) != FileError.OK){
            fprintf(stderr, "Unable to read from '%s'\n", sourcefile.ptr);
            return 1;
        }
        source_text = cast(str)file_data;
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
        source_text = sb.detach()[]; // leak it
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
        file_cache.insert("(stdin)", cast(const(ubyte)[])sb.borrow);
        source_text = sb.borrow;
    }
    if(!lang){
        if(sourcefile[].endswith(".ds"))
            lang = Language.DS;
        else if(sourcefile[].endswith(".c") || sourcefile[].endswith(".h"))
            lang = Language.C;
        else if(sourcefile[].endswith(".dasm"))
            lang = Language.DASM;
        else
            lang = Language.C;
    }
    if(lang == Language.DS){
        static import dscript.dscript;
        static import dscript_to_dasm;
        dscript.dscript.powerup;
        Box!(char[]) dasmtext = {allocator:MALLOCATOR};
        int err = dscript_to_dasm.compile_to_dasm(cast(ubyte[])source_text, &dasmtext);
        if(err) return err;
        source_text = dasmtext.data; // leak
        dscript.dscript.powerdown;
    }
    else if(lang == Language.C){
        import cfront.cfront : DEFAULT_INCLUDE_PATHS, DEFAULT_FRAMEWORK_PATHS, DEFAULT_LIBRARY_PATHS;
        static import cfront.cfront;
        // Add default paths
        include_paths.extend(DEFAULT_INCLUDE_PATHS);
        framework_paths.extend(DEFAULT_FRAMEWORK_PATHS);
        Box!(char[]) dasmtext = {allocator:MALLOCATOR};
        import cfront.cfront : CompileFlags;
        int err = cfront.cfront.compile_c_to_dasm(cast(ubyte[])source_text, &dasmtext, sourcefile[], include_paths[], framework_paths[], CompileFlags.init, force_includes[], &logger, &file_cache);
        if(err) return err;
        // If codegen_only, print DASM text and exit
        source_text = dasmtext.data; // leak
    }
    if(codegen_only){
        logger.sink(source_text);
        return 0;
    }
    UnlinkedModule prog;
    int err = parse_asm_string(MALLOCATOR, source_text, &prog);
    if(err){
        fprintf(stderr, "Parsing failed\n");
        return err;
    }
    // If parse_only, exit after successful parsing
    if(parse_only){
        fprintf(stdout, "Parsing successful\n");
        return 0;
    }

    LinkedModule linked_prog = {source_text: source_text, name: prog.name};
    {
        void find_loc(const char* first_char, out str fn, out int line, out int column){
            import dasm.dasm_tokenizer: Tokenizer;
            import dasm.dasm_token: Token, TokenType;
            str text = source_text;
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
                default:
                    fprintf(stderr, "Unknown module: '%.*s'\n", cast(int)imp.length, imp.ptr);
                    return 1;
            }
        }
        // Load dynamic libraries from dlimport declarations
        import dvm.dvm_dynload;
        import cfront.cfront : DEFAULT_LIBRARY_PATHS, DEFAULT_FRAMEWORK_PATHS;
        lib_paths.extend(DEFAULT_LIBRARY_PATHS);
        framework_paths.extend(DEFAULT_FRAMEWORK_PATHS);
        foreach(ref dlimport; prog.dlimports[]){
            LinkedModule* dyn_mod = boxed!LinkedModule(MALLOCATOR).pointer;
            DynLoadError dl_err = load_dynamic_module(MALLOCATOR, dlimport, dyn_mod, lib_paths[], framework_paths[]);
            if(dl_err.errored){
                fprintf(stderr, "Failed to load '%.*s': %.*s\n",
                    cast(int)dlimport.alias_name.length, dlimport.alias_name.ptr,
                    cast(int)dl_err.message.length, dl_err.message.ptr);
                return 1;
            }
            loaded[dlimport.alias_name] = dyn_mod;
        }
        err = link_module(MALLOCATOR, temp, BUILTINS, &prog, &linked_prog, &find_loc, &loaded, &file_cache);
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
    do {
        machine.paused = 0;
        if(debugger && disassemble)
            err = machine.run!(RunFlags.DEBUG|RunFlags.DISASSEMBLE_EACH)(&linked_prog, 1024*1024);
        else if(debugger)
            err = machine.run!(RunFlags.DEBUG)(&linked_prog, 1024*1024);
        else if(disassemble)
            err = machine.run!(RunFlags.DISASSEMBLE_EACH)(&linked_prog, 1024*1024);
        else
            err = machine.run!(RunFlags.NONE)(&linked_prog, 1024*1024);
        if(machine.paused){
            fprintf(stderr, "Running paused\n");
            getchar();
        }
    }while(machine.paused);

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
