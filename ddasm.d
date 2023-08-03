/*
 * Copyright © 2021-2022, David Priver
 */
import core.stdc.string: strlen, strerror, memcpy, memset;
import core.stdc.stdio: fprintf, stdout, stderr, fread, stdin, FILE, fwrite, fflush, fopen, fputs, fgets;
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
import dlib.btable;
import dlib.aliases;


import dvm.dvm_defs: Fuzzing, uintptr_t;
import dvm.dvm_linked: LinkedModule, Function, FunctionType, FunctionTable, FunctionInfo;
import dvm.dvm_unlinked: UnlinkedModule;
import dvm.dvm_machine: Machine, RunFlags;
import dvm.dvm_linker: link_module;
import dvm.dvm_regs: RegisterNames;

import dasm.dasm_parser: parse_asm_string;


__gshared bool devnull = false;

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
    uintptr_t[RegisterNames.RARGMAX-RegisterNames.RARG1] rargs;
    ZString sourcefile;
    with(ArgParseFlags) with(ArgToParseFlags) {
    ArgToParse[1] pos_args = [
        {
            "source", null,
            "Source file (.dasm file) to read from.
            If not given, will read from stdin.",
            ARGDEST(&sourcefile),
        },
    ];
    ArgToParse[7] kw_args = [
        {
            "--force-interactive", "-i",
            "Force interactive mode when reading from stdin.",
            ARGDEST(&force_interactive),
        },
        {
            "--no-interactive", null,
            "Force non-interactive mode when reading from stdin.",
            ARGDEST(&no_interactive),
        },
        {
            "--dev-null", null,
            "Builtin funcs don't print anymore",
            ARGDEST(&devnull),
        },
        {
            "--disassemble-every-op", "--dis",
            "Print out the disassembly before executing each op",
            ARGDEST(&disassemble),
        },
        {
            "--debug", "-g",
            "Executes in debug mode",
            ARGDEST(&debugger),
        },
        {
            "--ds", "--davescript",
            "Force interpretation of the source as
            davescript instead of dasm",
            ARGDEST(&highlevel),
        },
        {
            "-a", "--args",
            "Set rarg1 to ... to the following integer values",
            ARGDEST(&rargs[0]),
            NumRequired(0, rargs.length),
            SHOW_DEFAULT,
        },

    ];
    enum {HELP=0, VERSION=1}
    ArgToParse[2] early_args = [
        {
            "-h", "--help",
            "Print this help and exit.",
        },
        {
            "-v", "--version",
            "Print the version and exit.",
        },
    ];
    int columns = get_cols();
    ArgParser parser = {
        argc?argv[0][0..strlen(argv[0])]:"ddasm",
        "A dasm interpreter",
        early_args,
        pos_args,
        kw_args,
        null,
        null,
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
    auto error = parse_args(&parser, argc?argv[1..argc]:null, NONE);
    if(error) {
        print_argparse_error(&parser, error);
        fprintf(stderr, "Use --help to see usage.\n");
        return error;
    }
    }
    VAllocator va = VAllocator.from!(Mallocator);
    Box!(str, VAllocator*) btext;
    if(sourcefile.length){
        FileResult!(VAllocator*) fe = read_file(sourcefile.ptr, &va);
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
        StringBuilder!(VAllocator*) sb;
        sb.allocator = &va;
        LineHistory!() history;
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
        btext = sb.detach.as!(str);
    }
    else {
        StringBuilder!(VAllocator*) sb;
        sb.allocator = &va;
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
        btext = sb.detach.as!(str);
    }
    if(highlevel || sourcefile[].endswith(".ds")){
        static import dscript.dscript;
        static import dscript_to_dasm;
        dscript.dscript.powerup;
        Box!(char[], Mallocator) dasmtext;
        ubyte[] data = btext.as!(ubyte[]).data;
        int err = dscript_to_dasm.compile_to_dasm(data, &dasmtext);
        if(err) return err;
        btext.dealloc();
        btext = btext.from(btext.allocator, dasmtext.data);
        dscript.dscript.powerdown;
    }
    UnlinkedModule prog;
    int err = parse_asm_string(&va, btext.data, &prog);
    if(err){
        fprintf(stderr, "Parsing failed\n");
        return err;
    }
    expose_builtins;
    LinkedModule io_module;
    io_module.functions.allocator = &va;
    {
        void reg(str key, str f){
            io_module.functions[key] = (*BUILTINS)[f];
        }
        reg("puts", "Puts");
        reg("printf1", "Printf1");
        reg("printf2", "Printf2");
        reg("printf3", "Printf3");
        reg("printf4", "Printf4");
        reg("fread", "Fread");
        reg("fwrite", "Fwrite");
        reg("fputs", "Fputs");
        reg("fgets", "Fgets");
        reg("fflush", "Fflush");
        reg("stdin", "GetStdIn");
        reg("stdout", "GetStdOut");
        reg("getline", "GetLine");
    }

    LinkedModule mem_module;
    mem_module.functions.allocator = &va;
    {
        void reg(str key, str f){
            mem_module.functions[key] = (*BUILTINS)[f];
        }
        reg("malloc", "Malloc");
        reg("free",   "Free");
        reg("calloc", "Calloc");
        reg("cpy",    "Memcpy");
        reg("set",    "Memset");
    }

    LinkedModule misc_module;
    misc_module.functions.allocator = &va;
    {
        void reg(str key, str f){
            misc_module.functions[key] = (*BUILTINS)[f];
        }
        reg("atoi", "Atoi");
        version(Posix) reg("clock", "Clock");
    }


    LinkedModule linked_prog;
    linked_prog.source_text = btext;
    linked_prog.name = prog.name;
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
        ArenaAllocator!(Mallocator) arena;
        scope(exit) arena.free_all;
        VAllocator temp_va = VAllocator.from(&arena);
        BTable!(str, LinkedModule*, VAllocator*) loaded;
        loaded.allocator = &temp_va;
        foreach(imp; prog.imports[]){
            switch(imp){
                case "io":
                    loaded["io"] = &io_module;
                    break;
                case "mem":
                    loaded["mem"] = &mem_module;
                    break;
                case "misc":
                    loaded["misc"] = &misc_module;
                    break;
                default:
                    fprintf(stderr, "Unknown module: '%.*s'\n", cast(int)imp.length, imp.ptr);
                    return 1;
            }
        }
        err = link_module(&va, &temp_va, BUILTINS, &prog, &linked_prog, &find_loc, &loaded);
    }
    if(err){
        fprintf(stderr, "Linking failed\n");
        return err;
    }
    if(!linked_prog.start){
        fprintf(stderr, "Program needs a 'start' function as an entry point\n");
        return 1;
    }
    Machine machine;
    machine.registers[RegisterNames.RARG1 .. RegisterNames.RARGMAX] = rargs[];
    RecordingAllocator!Mallocator recorder;
    machine.allocator = VAllocator.from(&recorder);
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

version(Posix){
struct timespec {
	long tv_sec;
	long tv_nsec;
}
extern(C)
int clock_gettime(int __clock_id, timespec *__tp);
}

Function*
expose_function(F)(F fun){
    Function* f = cast(Function*)Mallocator.alloc(Function.sizeof).ptr;
    f.type = FunctionType.NATIVE;
    static if(is(F : uintptr_t function())){
        f.native_function_r = fun;
        f.n_ret = 1;
        f.n_args = 0;
        return f;
    }
    else static if(is(F : uintptr_t function(uintptr_t))){
        f.native_function_ra = fun;
        f.n_ret = 1;
        f.n_args = 1;
        return f;
    }
    else static if(is(F : uintptr_t function(uintptr_t, uintptr_t))){
        f.native_function_raa = fun;
        f.n_ret = 1;
        f.n_args = 2;
        return f;
    }
    else static if(is(F : uintptr_t function(uintptr_t, uintptr_t, uintptr_t))){
        f.native_function_raaa = fun;
        f.n_ret = 1;
        f.n_args = 3;
        return f;
    }
    else static if(is(F : uintptr_t function(uintptr_t, uintptr_t, uintptr_t, uintptr_t))){
        f.native_function_raaaa = fun;
        f.n_ret = 1;
        f.n_args = 4;
        return f;
    }
    else static if(is(F : void function())){
        f.native_function_ = fun;
        f.n_ret = 0;
        f.n_args = 0;
        return f;
    }
    else static if(is(F : void function(uintptr_t))){
        f.native_function_a = fun;
        f.n_ret = 0;
        f.n_args = 1;
        return f;
    }
    else static if(is(F : void function(uintptr_t, uintptr_t))){
        f.native_function_aa = fun;
        f.n_ret = 0;
        f.n_args = 2;
        return f;
    }
    else static if(is(F : void function(uintptr_t, uintptr_t, uintptr_t))){
        f.native_function_aaa = fun;
        f.n_ret = 0;
        f.n_args = 3;
        return f;
    }
    else static if(is(F : void function(uintptr_t, uintptr_t, uintptr_t, uintptr_t))){
        f.native_function_aaaa = fun;
        f.n_ret = 0;
        f.n_args = 4;
        return f;
    }
    else static if(is(F : void function(uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t))){
        f.native_function_aaaaa = fun;
        f.n_ret = 0;
        f.n_args = 5;
        return f;
    }
    else {
    pragma(msg, F.stringof);
    pragma(msg, typeof(fun));
    static assert(0);
    }
}

void
register_function(F)(string name, F fun){
    (*BUILTINS)[name] = FunctionInfo(name, expose_function(fun));
}

FunctionTable*
BUILTINS(){
    __gshared bool initialized = false;
    __gshared FunctionTable table;
    __gshared VAllocator va = VAllocator.from!(Mallocator);
    if(!initialized){
        initialized = true;
        table.allocator = &va;
    }
    return &table;
}

void
expose_builtins(){
    register_function("Printf1",
        (uintptr_t fmt, uintptr_t arg){
            if(devnull) return;
            if(!Fuzzing)fprintf(stdout, cast(char*)fmt, arg);
        }
    );
    register_function("Printf2",
        (uintptr_t fmt, uintptr_t arg, uintptr_t arg2){
            if(devnull) return;
            if(!Fuzzing)fprintf(stdout, cast(char*)fmt, arg, arg2);
        }
    );
    register_function("Printf3",
        (uintptr_t fmt, uintptr_t arg, uintptr_t arg2, uintptr_t arg3){
            if(devnull) return;
            if(!Fuzzing)fprintf(stdout, cast(char*)fmt, arg, arg2, arg3);
        }
    );
    register_function("Printf4",
        (uintptr_t fmt, uintptr_t arg, uintptr_t arg2, uintptr_t arg3, uintptr_t arg4){
            if(devnull) return;
            if(!Fuzzing)fprintf(stdout, cast(char*)fmt, arg, arg2, arg3, arg4);
        }
    );
    register_function("Puts",
        (uintptr_t arg){
            if(devnull) return;
            if(!Fuzzing)fprintf(stdout, "%s\n", cast(char*)arg);
        }
    );
    version(Posix){
    register_function("Clock",
        (){
            timespec tv;
            clock_gettime(6, &tv);
            uintptr_t result =  tv.tv_sec * 1000*1000*1000 + tv.tv_nsec;
            return result;
        }
    );
    }
    register_function("Fread",
        (uintptr_t ptr, uintptr_t size, uintptr_t nitems, uintptr_t stream){
            return cast(uintptr_t)fread(cast(void*)ptr, size, nitems, cast(FILE*)stream);
        }
    );
    register_function("Fwrite",
        (uintptr_t ptr, uintptr_t size, uintptr_t nitems, uintptr_t stream){
            return cast(uintptr_t)fwrite(cast(void*)ptr, size, nitems, cast(FILE*)stream);
        }
    );
    register_function("Fputs",
        (uintptr_t ptr, uintptr_t stream){
            return cast(uintptr_t)fputs(cast(const char*)ptr, cast(FILE*)stream);
        }
    );
    register_function("Fgets",
        (uintptr_t ptr, uintptr_t size, uintptr_t stream){
            return cast(uintptr_t)fgets(cast(char*)ptr, cast(int)size, cast(FILE*)stream);
        }
    );
    register_function("Fflush",
        (uintptr_t stream){
            return cast(uintptr_t)fflush(cast(FILE*)stream);
        }
    );
    register_function("GetStdIn",
        (){
            // return cast(uintptr_t)fopen("hello.txt", "r");
            return cast(uintptr_t)stdin;
        }
    );
    register_function("GetStdOut",
        (){
            return cast(uintptr_t)stdout;
        }
    );
    register_function("Malloc",
        (uintptr_t size){
            return cast(uintptr_t)malloc(size);
        }
    );
    register_function("Free",
        (uintptr_t ptr){
            free(cast(void*)ptr);
        }
    );
    register_function("Memcpy",
        (uintptr_t dst, uintptr_t src, uintptr_t len){
            return cast(uintptr_t)memcpy(cast(void*)dst, cast(void*)src, len);
        }
    );
    register_function("GetLine",
        (uintptr_t buff, uintptr_t buflen, uintptr_t prompt_){
            __gshared LineHistory!() history;
            char* buff_ = cast(char*)buff;
            char* prompt = cast(char*)prompt_;
            str promptbuff;
            if(!prompt)
                promptbuff = "dasm> ";
            else
                promptbuff = prompt[0..strlen(prompt)];
            ptrdiff_t len = get_input_line(&history, promptbuff, buff_[0..buflen]);
            if(len >= 0 && len < buflen)
                buff_[len] = 0;
            else
                buff_[buflen-1] = 0;
            return cast(uintptr_t)len;
        }
    );
    register_function("Atoi",
        (uintptr_t p){
            return cast(uintptr_t)atoi(cast(char*)p);
        }
    );
    register_function("Calloc", (uintptr_t nitems, uintptr_t size){
        return cast(uintptr_t)calloc(nitems, size);
    });
    register_function("Memset",
            (uintptr_t dst, uintptr_t c, uintptr_t sz){
                void* buff = cast(void*)dst;
                memset(buff, cast(int)c, sz);
            }
    );
}



