import dlib.stringbuilder: StringBuilder, Q, H, E;
import dlib.argparse;
import dlib.zstring;
import dlib.get_input;
import dlib.term_util: get_cols, stdin_is_interactive;
import dlib.file_util;
import dlib.allocator;
import dlib.barray;
import dlib.parse_numbers: parse_hex_inner, parse_unsigned_human;
import dlib.btable;
import dlib.box: Box;
import dlib.str_util: endswith, split, stripped;


import core.stdc.string: strlen, strerror, memcpy;
import core.stdc.stdio: fprintf, stdout, stderr, fread, stdin, FILE, fwrite, fflush, fopen, fputs, fgets;
import core.stdc.stdlib: calloc, malloc, free, atoi;

import dvm_defs;
import dvm_regs;
import dvm_args;
import dvm_instructions;
import dvm_linked;
import dvm_unlinked;
import dvm_machine;
import dasm_token;
import dasm_tokenizer;


__gshared devnull = false;

static if(Fuzzing){
    __gshared RecordingAllocator!Mallocator recorder;
    extern(C)
    int LLVMFuzzerTestOneInput(const ubyte *Data, size_t Size){
        const char* d = cast(const char*)Data;
        const char[] data = d[0 .. Size];

        UnlinkedModule prog;
        auto va = VAllocator.from!(GlobalAllocator!recorder);
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
    ArgToParse[6] kw_args = [
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
        if(!Fuzzing)fprintf(stderr, "Use --help to see usage.\n");
        return error;
    }
    }
    auto va = VAllocator.from!(Mallocator);
    // fprintf(stdout, "va.vtable.name: %.*s\n", cast(int)va.vtable.name.length, va.vtable.name.ptr);
    Box!(const(char)[], VAllocator) btext;
    if(sourcefile.length){
        auto fe = read_file!VAllocator(sourcefile.ptr, &va);
        if(fe.errored){
            version(Posix)
                if(!Fuzzing)fprintf(stderr, "Unable to read from '%s': %s\n", sourcefile.ptr, strerror(fe.errored));
            // TODO: get error message from windows
            version(Windows)
                if(!Fuzzing)fprintf(stderr, "Unable to read from '%s'\n", sourcefile.ptr);
            return fe.errored;
        }
        btext = cast(typeof(btext))fe.value;
    }
    else if(!no_interactive && (force_interactive || stdin_is_interactive())){
        StringBuilder!VAllocator sb;
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
            auto len = get_input_line(&history, "> ", buff[]);
            if(len < 0) break;
            if(len == 1 && buff[0] == 'q') break;
            if(len){
                history.add_line(buff[0..len]);
                sb.write(buff[0..len]);
            }
            sb.write('\n');
        }
        btext = sb.detach.as!(const(char)[]);
    }
    else {
        StringBuilder!VAllocator sb;
        sb.allocator = &va;
        for(;;){
            enum N = 4096;
            sb.ensure_additional(N);
            char* buff= sb.data + sb.cursor;
            auto numread = fread(buff, 1, N, stdin);
            sb.cursor += numread;
            if(numread != N)
                break;
        }
        if(!sb.cursor)
            sb.write(' ');
        btext = sb.detach.as!(const(char)[]);
    }
    if(highlevel || sourcefile[].endswith(".ds")){
        static import dscript_to_dasm;
        dscript_to_dasm.powerup;
        Box!(char[], Mallocator) dasmtext;
        auto data = btext.data;
        auto d = (cast(const(ubyte)*)data.ptr)[0 .. data.length];
        int err = dscript_to_dasm.compile_to_dasm(d, &dasmtext);
        if(err) return err;
        btext.dealloc();
        btext = btext.from(btext.allocator, dasmtext.data);
        dscript_to_dasm.powerdown;
    }
    UnlinkedModule prog;
    int err = parse_asm_string(&va, btext.data, &prog);
    if(err){
        if(!Fuzzing)fprintf(stderr, "Parsing failed\n");
        return err;
    }
    expose_builtins;
    LinkedModule linked_prog;
    linked_prog.source_text = btext;
    {
        ArenaAllocator!(Mallocator) arena;
        scope(exit) arena.free_all;
        auto temp_va = VAllocator.from(&arena);
        err = link_asm(&va, &temp_va, BUILTINS, &prog, &linked_prog);
    }
    if(err){
        if(!Fuzzing)fprintf(stderr, "Linking failed\n");
        return err;
    }
    if(!linked_prog.start){
        if(!Fuzzing)fprintf(stderr, "Program needs a 'start' function as an entry point\n");
        return 1;
    }
    Machine machine;
    auto recorder = RecordingAllocator!(Mallocator)();
    machine.allocator = VAllocator.from(&recorder);
    if(debugger){
        err = machine.run!(RunFlags.DEBUG)(&linked_prog, 1024*1024);
    }
    else if(!disassemble)
        err = machine.run!(RunFlags.NONE)(&linked_prog, 1024*1024);
    else
        err = machine.run!(RunFlags.DISASSEMBLE_EACH)(&linked_prog, 1024*1024);
    if(err){
        if(!Fuzzing)fprintf(stderr, "Running failed\n");
        return err;
    }
    return 0;
}
}

struct timespec {
	long tv_sec;
	long tv_nsec;
}
extern(C)
int clock_gettime(int __clock_id, timespec *__tp);

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
    __gshared initialized = false;
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
    register_function("Clock",
        (){
            timespec tv;
            clock_gettime(6, &tv);
            uintptr_t result =  tv.tv_sec * 1000*1000*1000 + tv.tv_nsec;
            return result;
        }
    );
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
        (uintptr_t buff, uintptr_t buflen){
            __gshared LineHistory!() history;
            char* buff_ = cast(char*)buff;
            ptrdiff_t len = get_input_line(&history, "dasm> ", buff_[0..buflen]);
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
}


int
parse_asm_string(VAllocator* allocator, const(char)[] text, UnlinkedModule* prog){
    ParseContext ctx;
    ctx.allocator = allocator;
    ctx.tokenizer = Tokenizer.from(text);
    ctx.prog.functions.bdata.allocator = ctx.allocator;
    ctx.prog.variables.bdata.allocator = ctx.allocator;
    ctx.prog.arrays.bdata.allocator = ctx.allocator;
    auto err = ctx.parse_asm();
    if(err){
        if(!Fuzzing)fprintf(stderr, "%.*s\n", cast(int)ctx.errmess.length, ctx.errmess.ptr);
        Mallocator.free(ctx.errmess.ptr, ctx.errmess.mem_size);
        ctx.prog.functions.cleanup;
        ctx.prog.variables.cleanup;
        foreach(ref arr; ctx.prog.arrays[]){
            arr.array.cleanup;
        }
        ctx.prog.arrays.cleanup;
        return err;
    }
    *prog = ctx.prog;
    return 0;
}

enum AsmError: int {
    NO_ERROR = 0,
    PARSE_ERROR,
    LINK_ERROR,
}

struct ParseContext{
    Tokenizer tokenizer;
    VAllocator* allocator;
    UnlinkedModule prog;
    ZString errmess;
    void
    err_print(A...)(Token tok, A args){
        StringBuilder!Mallocator sb;
        sb.FORMAT(tok.line, ':', tok.column, ": ParseError: ");
        foreach(a; args)
            sb.write(a);
        errmess = sb.zdetach;
    }
    int
    parse_asm(){
        with(TokenType) with(AsmError) with(ArgumentKind){
            Token tok;
            for(;;){
                tok = tokenizer.current_token_and_advance();
                while(tok.type == SPACE || tok.type == NEWLINE || tok.type == TAB){
                    tok = tokenizer.current_token_and_advance;
                }
                tok = tokenizer.skip_comment(tok);
                if(tok.type == NEWLINE)
                    continue;
                if(tok.type == EOF)
                    break;
                if(tok.type == IDENTIFIER){
                    if(tok.text == "function"){
                        AbstractFunction func;
                        func.first_char = tok._text;
                        func.instructions.bdata.allocator = allocator;
                        tok = tokenizer.current_token_and_advance;
                        if(tok.type != SPACE){
                            err_print(tok, "function header must be followed by the function's name");
                            return PARSE_ERROR;
                        }
                        tok = tokenizer.current_token_and_advance;
                        if(tok.type != IDENTIFIER){
                            err_print(tok, "expected a function name");
                            return PARSE_ERROR;
                        }
                        func.name = tok.text;
                        tok = tokenizer.current_token_and_advance;
                        while(tok.type == SPACE || tok.type == TAB)
                            tok = tokenizer.current_token_and_advance;
                        auto peek = tokenizer.current_token;
                        if(peek.type == NUMBER){
                            tok = tokenizer.current_token_and_advance;
                            auto err = parse_unsigned_human(tok.text);
                            if(err.errored){
                                err_print(tok, "Unable to parse a number from ", Q(tok.text));
                                return PARSE_ERROR;
                            }
                            if(err.value > int.max){
                                err_print(tok, "number (", tok.text, ") exceeds ", int.max);
                                return PARSE_ERROR;
                            }
                            func.n_args = cast(int)err.value;
                        }
                        else {
                        }
                        int err = parse_function(&func);
                        if(err) return err;
                        prog.functions.push(func);
                    }
                    else if(tok.text == "var"){
                        tok = tokenizer.current_token_and_advance;
                        if(tok.type != SPACE){
                            err_print(tok, "var must be followed by a space: ", Q(tok.text));
                            return PARSE_ERROR;
                        }
                        tok = tokenizer.current_token_and_advance;
                        if(tok.type != IDENTIFIER){
                            err_print(tok, "expected a variable name");
                            return PARSE_ERROR;
                        }
                        AbstractVariable var;
                        var.tok = tok;
                        var.name = tok.text;
                        tok = tokenizer.current_token_and_advance;
                        if(tok.type != SPACE){
                            err_print(tok, "var name must be followed by a space");
                            return PARSE_ERROR;
                        }
                        tok = tokenizer.current_token_and_advance;
                        while(tok.type == SPACE && tok.type == TAB)
                            tok = tokenizer.current_token_and_advance;
                        auto arg = parse_one_argument(tok);
                        switch(arg.kind){
                            case UNSET:
                            case REGISTER:
                            case LABEL:
                                if(!errmess.length) err_print(tok, "Invalid variable dealio");
                                return PARSE_ERROR;
                            default:
                                var.value = arg;
                                break;
                        }
                        prog.variables.push(var);
                    }
                    else {
                        err_print(tok, "1. Only function or variable declarations are legal at global scope, not ", Q(tok.text));
                        return PARSE_ERROR;
                    }
                }
                else {
                    err_print(tok, "2. Only function or variable declarations are legal at global scope, not ", Q(tok.text));
                    return PARSE_ERROR;
                }
            }
        }
        return 0;
    }
    int
    parse_function(AbstractFunction* func){ with(TokenType) with(AsmError) with(Instruction){
        Token tok;
        for(;;){
            tok = tokenizer.current_token_and_advance;
            while(tok.type == SPACE || tok.type == NEWLINE || tok.type == TAB)
                tok = tokenizer.current_token_and_advance;
            if(tok.type == EOF){
                err_print(tok, "Unexpected end of file");
                return PARSE_ERROR;
            }

            if(tok.type == POUND){
                tok = tokenizer.skip_comment(tok);
                continue;
            }
            if(tok.type != IDENTIFIER){
                err_print(tok, "Expected an identifier, got ", Q(tok.text), " instead");
                return PARSE_ERROR;
            }
            if(tok.text == "end"){
                if(!func.instructions.count){
                    err_print(tok, "function should have instructions");
                    return PARSE_ERROR;
                }
                auto end = func.instructions[$-1].instruction;
                if(end != HALT && end != RET && end != ABORT && end != TAIL_CALL_I && end != TAIL_CALL_R){
                    err_print(tok, "Last instruction of a function should be a halt, ret, tail_call or abort.");
                    return PARSE_ERROR;
                }
                break;
            }
            if(tok.text == "function"){
                err_print(tok, "Can't define a function inside a function");
                return PARSE_ERROR;
            }
            if(tok.text == "label"){
                AbstractInstruction inst;
                inst.first_char = tok._text;
                tok = tokenizer.current_token_and_advance;
                if(tok.type != SPACE && tok.type != TAB){
                    err_print(tok, "Expected space between label and label name");
                    return PARSE_ERROR;
                }
                while(tok.type == SPACE || tok.type == TAB)
                    tok = tokenizer.current_token_and_advance;
                if(tok.type != IDENTIFIER){
                    err_print(tok, "Expected an identifier as the label name");
                    return PARSE_ERROR;
                }
                inst.instruction = NOP;
                inst.label = tok.text;
                func.instructions.push(inst);
                continue;
            }
            AbstractInstruction inst;
            inst.first_char = tok._text;
            int err = decode_instruction(tok, &inst);
            if(err) return err;
            func.instructions.push(inst);
        }
        return NO_ERROR;
    }
    }

    int
    decode_instruction(Token tok, AbstractInstruction* inst){ with(AsmError) with(TokenType) with(ArgumentKind){
        auto first_tok = tok;
        auto infos = InstructionTable.get(cast(string)tok.text);
        if(!infos){
            err_print(tok, Q(tok.text), " does not match any known instruction");
            return PARSE_ERROR;
        }
        for(;;){
            tok = tokenizer.current_token_and_advance;
            while(tok.type == SPACE || tok.type == TAB)
                tok = tokenizer.current_token_and_advance;
            if(tok.type == NEWLINE)
                break;
            if(tok.type == POUND){
                tok = tokenizer.skip_comment(tok);
                break;
            }
            auto arg = parse_one_argument(tok);
            if(arg.kind == UNSET){
                if(!errmess.length)
                    err_print(tok, "Unable to decode an argument from ", Q(tok.text));
                return PARSE_ERROR;
            }
            if(inst.n_args >= inst.MAX_ARGS){
                err_print(tok, "Too many arguments");
                return PARSE_ERROR;
            }
            inst.args[inst.n_args++] = arg;
            }
        foreach(info; (*infos)[]){
            if(info.args.length == inst.n_args){
                foreach(i, kind; info.args){
                    if(!(kind & inst.args[i].kind)){
                        goto skip;
                    }
                }
                inst.instruction = info.instruction;
                // kind of dumb to do this here, but whatever
                return NO_ERROR;
                skip:{}
            }
        }
        // This error message sucks. If there is only one candidate, we should say exactly where it fails.
        // If there is more than one, we should see if it fails in all the same places, then report that.
        err_print(first_tok, "Unable to match against instruction ", Q(first_tok.text), " . Wrong number or wrong types of arguments");
        return PARSE_ERROR;
    }
    }

    Argument
    parse_one_argument(Token tok){with(ArgumentKind) with(TokenType){
        Argument result;
        while(tok.type == SPACE || tok.type == TAB)
            tok = tokenizer.current_token_and_advance;
        result.first_char = tok._text;
        switch(tok.type){
            case EOF:
                err_print(tok, "Unexpected end of file");
                return result;
            case POUND: case NEWLINE:
                err_print(tok, "NEWLINE"); // lol what
                return result;
            // match arrays
            case LEFTSQUAREBRACKET:{
                AbstractArray array;
                array.array.bdata.allocator = allocator;
                for(tok = tokenizer.current_token_and_advance; tok.type != RIGHTSQUAREBRACKET; tok = tokenizer.current_token_and_advance){
                    switch(tok.type){
                        case NEWLINE:
                            continue;
                        case POUND:{
                            Token peek = tokenizer.current_token;
                            tok = tokenizer.current_token_and_advance;
                            while(peek.type != EOF && peek.type != NEWLINE){
                                tokenizer.advance;
                                peek = tokenizer.current_token;
                            }
                            if(peek.type == EOF){
                                err_print(peek, "Unexpected EOF");
                                return result;
                            }
                            continue;
                        }
                        case EOF:
                            err_print(tok, "Unexpected EOF");
                            return result;
                        case COMMA:
                        case SPACE:
                        case TAB:
                        case CARRIAGERETURN:
                            continue;
                        default:{
                            auto arg = parse_one_argument(tok);
                            if(arg.kind == UNSET){
                                err_print(tok, "array"); // lol what
                                array.array.cleanup;
                                return result;
                            }
                            array.array.push(arg);
                        }continue;
                    }
                }
                array.id = prog.arrays.count; // FIXME?
                prog.arrays.push(array);
                result.array = array.id;
                result.kind = ARRAY;
                return result;
            }
            case QUOTATION:{
                const char * before = tok.text.ptr;
                bool backslash = false;
                tok = tokenizer.current_token_and_advance;
                for(;;){
                    if(tok.type == QUOTATION && !backslash)
                        break;
                    if(tok.type == BACKSLASH)
                        backslash = !backslash;
                    if(tok.type == NEWLINE || tok.type == EOF){
                        err_print(tok, "bad quotation, no terminating '\"'");
                        return result;
                    }
                    if(tok.type != BACKSLASH)
                        backslash = false;
                    tok = tokenizer.current_token_and_advance;
                }
                ptrdiff_t length = tok.text.ptr - before;
                result.text = before[1..length];
                result.kind = STRING;
                return result;
            }
            case DASH: case PLUS:{
                int minuses = tok.type == DASH;
                number:
                for(;;){
                    tok = tokenizer.current_token_and_advance;
                    switch(tok.type){
                        case DASH:
                            minuses++;
                            continue;
                        case SPACE: case TAB: case PLUS:
                            continue;
                        case NUMBER:
                            break number;
                        default:
                            return result;
                    }
                }
                auto e = parse_unsigned_human(tok.text);
                if(e.errored){
                    err_print(tok, "Unable to parse a number from ", Q(tok.text));
                    return result;
                }
                auto value = e.value;
                if(minuses & 1)
                    value = -value;
                result.kind = IMMEDIATE;
                result.immediate = value;
                return result;
            }
            case NUMBER:{
                auto e = parse_unsigned_human(tok.text);
                if(e.errored){
                    err_print(tok, "Unable to parse a number from ", Q(tok.text));
                    return result;
                }
                result.immediate = e.value;
                result.kind = IMMEDIATE;
                return result;
            }
            case IDENTIFIER:{
                auto text = tok.text;
                if(auto val = ConstantsTable.get(cast(string)text)){
                    result.immediate = *val;
                    result.kind = IMMEDIATE;
                    return result;
                }
                // this is lazy and dumb, but whatever.
                foreach(mode; CmpModes)
                    if(mode.name == text){
                        result.cmp_mode = mode.mode;
                        result.kind = CMPMODE;
                        return result;
                    }
                if(auto ri = get_register_info(text)){
                    result.reg = ri.register;
                    result.kind = REGISTER;
                    return result;
                }
                void parse_namespaced(string label, ArgumentKind kind){
                    tok = tokenizer.current_token_and_advance;
                    while(tok.type == SPACE || tok.type == TAB)
                        tok = tokenizer.current_token_and_advance;
                    if(tok.type == POUND || tok.type == EOF || tok.type == NEWLINE){
                        err_print(tok, "Unexpected end of line");
                        return;
                    }
                    if(tok.type != IDENTIFIER){
                        err_print(tok, "Expected an identifier as a ", label, " name, got ", Q(tok.text));
                        return;
                    }
                    result.kind = kind;
                    // these all pun, so whatever
                    result.function_name = tok.text;
                    return;
                }
                switch(text){
                    case "function":
                        parse_namespaced("function", FUNCTION);
                        return result;
                    case "label":
                        parse_namespaced("label", LABEL);
                        return result;
                    case "var":
                        parse_namespaced("var", VARIABLE);
                        return result;
                    case "constant":
                        parse_namespaced("constant", CONSTANT);
                        return result;
                    default: break;
                }
                return result;
            }
            default:
                err_print(tok, "Unable to match ", Q(tok.text), " to any valid argument type.");
                return result;
        }
    }
    }
}

Table!(string, uintptr_t)*
ConstantsTable(){
    static __gshared bool initialized;
    alias TableT = Table!(string, uintptr_t);
    static __gshared TableT constants_table;
    if(!initialized){
        initialized = true;
        foreach(ii; INSTRUCTION_INFOS){
            constants_table[ii.NAME] = ii.instruction;
        }
        foreach(ri; registerinfos){
            constants_table[ri.NAME] = ri.register;
        }
        constants_table["PTRSIZE"] = (void*).sizeof;
        constants_table["USIZE"] = (uintptr_t).sizeof;
        constants_table["FUNCSIZE"] = (Function).sizeof;
    }
    return &constants_table;
}

struct LinkContext {
    VAllocator* allocator;
    VAllocator* temp_allocator;
    FunctionTable* builtins;
    UnlinkedModule* unlinked;
    LinkedModule prog;
    Box!(char[], Mallocator) errmess;

    void
    err_print(A...)(Token tok, A args){
        StringBuilder!Mallocator sb;
        sb.FORMAT(tok.line, ':', tok.column, ": LinkError: ");
        foreach(a; args)
            sb.write(a);
        errmess = sb.detach;
    }

    AsmError
    allocate_arrays(){
        foreach(abstract_array; unlinked.arrays[]){
            IntegerArray array;
            array.bdata.allocator = allocator;
            size_t count = abstract_array.array.count?abstract_array.array.count:1;// we need a valid allocation.
            array.bdata.resize(count);
            array.count = count;
            prog.arrays.push(array);
        }
        return AsmError.NO_ERROR;
    }

    AsmError
    allocate_variables(){
        if(unlinked.variables.count)
            prog.variables.resize(unlinked.variables.count);
        foreach(i, var; unlinked.variables[]){
            if(var.name in prog.variable_table){
                err_print(var.tok, "Duplicate variable: ", Q(var.tok.text));
                return AsmError.LINK_ERROR;
            }
            prog.variable_table[var.name] = &prog.variables.data[i];
        }
        return AsmError.NO_ERROR;
    }

    AsmError
    allocate_functions(){
        size_t total_size = 0;
        enum CODE_PAD = 4;
        foreach(ref func; unlinked.functions[]){
            auto size = calculate_function_size(&func) + CODE_PAD;
            total_size += size;
        }
        prog.bytecode.resize(total_size);
        size_t code_off = 0;
        prog.function_store.resize(unlinked.functions.count);
        foreach(i, ref func; unlinked.functions[]){
            auto size = calculate_function_size(&func);
            code_off += CODE_PAD/2;
            if(func.name in prog.functions){
                err_print(prog.find_token(func.first_char), "Duplicate function: ", Q(func.name));
                return AsmError.LINK_ERROR;
            }
            auto info = prog.functions.set(func.name);
            info.name = func.name;
            info.func = &prog.function_store.data[i];
            info.func.type = FunctionType.INTERPRETED;
            info.func.instructions_ = &prog.bytecode.data[code_off];
            info.func.length = cast(uint)size;
            if(func.name == "start")
                prog.start = info.func;
            code_off += CODE_PAD - CODE_PAD/2 + size;
        }
        return AsmError.NO_ERROR;
    }

    //
    // Convert the un-escaped raw text into a ZString.
    // Records the ZString in the string table.
    ZString
    make_string(const char[] text){
        StringBuilder!VAllocator sb;
        sb.allocator = allocator;
        // This is slow and we should use simd to scan for
        // escape codes.
        for(size_t i = 0; i < text.length; i++){
            char c = text[i];
            if(c == '\\'){
                if(i < text.length - 1){
                    char next = text[i+1];
                    switch(next){
                        case 'n':  sb.write('\n'); i++; continue;
                        case 't':  sb.write('\t'); i++; continue;
                        case '\\': sb.write('\\'); i++; continue;
                        case 'r':  sb.write('\r'); i++; continue;
                        case 'e':  sb.write('\033'); i++; continue;
                        case '0':  sb.write('\0'); i++; continue;
                        case 'b':  sb.write('\b'); i++; continue;
                        case 'x':  case 'X':
                            if(i < text.length - 3){
                                auto v = parse_hex_inner(text[i+2 .. i+4]);
                                if(v.errored)
                                    break;
                                sb.write(cast(char)v.value);
                                i+=3;
                                continue;
                            }
                            goto default;
                        default: break;
                    }
                }
                // invalid backslash escape. Could error here.
                sb.write('\\');
                continue;
            }
            sb.write(c);
        }
        ZString result = sb.zdetach;
        prog.strings.push(result);
        return result;
    }
    ZString
    make_message(Token tok, const char[] text){
        StringBuilder!VAllocator sb;
        sb.allocator = allocator;
        sb.FORMAT(tok.line, ':', tok.column, ": ");
        // This is slow and we should use simd to scan for
        // escape codes.
        for(size_t i = 0; i < text.length; i++){
            char c = text[i];
            if(c == '\\'){
                if(i < text.length - 1){
                    char next = text[i+1];
                    switch(next){
                        case 'n': sb.write('\n'); i++; continue;
                        case 't': sb.write('\t'); i++; continue;
                        case '\\': sb.write('\\'); i++; continue;
                        case 'r': sb.write('\r'); i++; continue;
                        case 'e': sb.write('\033'); i++; continue;
                        case '0': sb.write('\0'); i++; continue;
                        case 'b': sb.write('\b'); i++; continue;
                        case 'x': case 'X':
                            if(i < text.length - 3){
                                auto v = parse_hex_inner(text[i+2 .. i+4]);
                                if(v.errored){
                                    if(!Fuzzing)fprintf(stderr, "parse_hex_inner failed: '%.*s'\n", 2, text.ptr+i+2);
                                    break;
                                }
                                sb.write(cast(char)v.value);
                                i+=3;
                                continue;
                            }
                            goto default;
                        default: break;
                    }
                }
                // invalid backslash escape. Could error here.
                sb.write('\\');
                continue;
            }
            sb.write(c);
        }
        ZString result = sb.zdetach;
        prog.strings.push(result);
        return result;
    }

    AsmError
    link_arrays(){
        foreach(i, abstract_array; unlinked.arrays[]){with(ArgumentKind){
            auto actual = &prog.arrays[i];
            foreach(j, v; abstract_array.array[]){
                switch(v.kind){
                    default:
                    case LABEL:
                    case UNSET:
                        err_print(prog.find_token(v.first_char), "BUG");
                        return AsmError.LINK_ERROR;
                    case STRING:{
                        ZString s = make_string(v.text);
                        (*actual)[j] = cast(uintptr_t)s.ptr;
                    }break;
                    case IMMEDIATE:
                        (*actual)[j] = v.immediate;
                        break;
                    case REGISTER:
                        (*actual)[j] = v.reg;
                        break;
                    case CMPMODE:
                        (*actual)[j] = v.cmp_mode;
                        break;
                    case FUNCTION:
                        if(auto func = v.function_name in  prog.functions)
                            (*actual)[j] = cast(uintptr_t)func.func;
                        else{
                            err_print(prog.find_token(v.first_char), "Reference to unknown function: ", Q(v.function_name));
                            return AsmError.LINK_ERROR;
                        }
                        break;
                    case ARRAY:
                        (*actual)[j] = cast(uintptr_t)prog.arrays[i].bdata.data.ptr;
                        break;
                    case VARIABLE:
                        if(auto var = v.variable in prog.variable_table)
                            (*actual)[j] = cast(uintptr_t)*var;
                        else {
                            err_print(prog.find_token(v.first_char), "Reference to unknown variable: ", Q(v.variable));
                            return AsmError.LINK_ERROR;
                        }
                        break;

                    case CONSTANT:
                        err_print(prog.find_token(v.first_char), "TODO");
                        return AsmError.LINK_ERROR;
                }
            }
        }
        }
        return AsmError.NO_ERROR;
    }

    AsmError
    link_variables(){with(ArgumentKind){
        foreach(i, var; unlinked.variables[]){
            uintptr_t* dest = &prog.variables.data[i];
            switch(var.value.kind){
                default:
                case LABEL:
                case UNSET:
                    err_print(prog.find_token(var.value.first_char), "BUG");
                    return AsmError.LINK_ERROR;
                case STRING:{
                    ZString s = make_string(var.value.text);
                    *dest = cast(uintptr_t)s.ptr;
                }break;
                case IMMEDIATE:
                    *dest = var.value.immediate;
                    break;
                case REGISTER:
                    *dest = var.value.reg;
                    break;
                case CMPMODE:
                    *dest = var.value.cmp_mode;
                    break;
                case FUNCTION:
                    if(auto func = var.value.function_name in  prog.functions)
                        *dest = cast(uintptr_t)func.func;
                    else{
                        err_print(prog.find_token(var.value.first_char), "Reference to unknown function: ", Q(var.value.function_name));
                        return AsmError.LINK_ERROR;
                    }
                    break;
                case ARRAY:
                    *dest = cast(uintptr_t)prog.arrays[var.value.array].bdata.data.ptr;
                    break;
                case VARIABLE:
                    if(auto variable = var.value.variable in prog.variable_table)
                        (*dest) = cast(uintptr_t)*variable;
                    else {
                        err_print(prog.find_token(var.value.first_char), "Reference to unknown variable: ", Q(var.value.variable));
                        return AsmError.LINK_ERROR;
                    }
                    break;
                case CONSTANT:
                    err_print(prog.find_token(var.value.first_char), "TODO");
                    return AsmError.LINK_ERROR;
            }
        }
        return AsmError.NO_ERROR;
    }
    }

    AsmError
    link_functions(){
        foreach(i, ref func; prog.function_store.data){
            auto afunc = &unlinked.functions[i];
            if(auto err = link_function(afunc, &func))
                return err;
        }
        return AsmError.NO_ERROR;
    }

    AsmError
    link_function(AbstractFunction* afunc, Function* func){
        BTable!(const(char)[], uintptr_t, VAllocator) labels;
        labels.allocator = temp_allocator;
        scope(exit) labels.cleanup;
        // look for labels
        {
            uintptr_t* ip = func.instructions_;
            foreach(inst; afunc.instructions[]){
                if(inst.label.length){
                    if(inst.label in labels){
                        err_print(prog.find_token(inst.first_char), "Duplicate label ", Q(inst.label));
                        return AsmError.LINK_ERROR;
                    }
                    labels[inst.label] = cast(uintptr_t)ip;
                }
                ip += instruction_size(inst.instruction);
            }
        }
        uintptr_t* ip = func.instructions_;
        foreach(inst; afunc.instructions[]){
            *(ip++) = inst.instruction;
            foreach(arg;inst.args[0..inst.n_args]){
                switch(arg.kind)with(ArgumentKind){
                    default:
                    case UNSET:
                        err_print(prog.find_token(afunc.first_char), "BUG: link_function");
                        return AsmError.LINK_ERROR;
                    case STRING:{
                        ZString s = inst.instruction == Instruction.MSG?
                            make_message(prog.find_token(inst.first_char), arg.text)
                            :
                            make_string(arg.text);
                        *(ip++) = cast(uintptr_t)s.ptr;
                    }break;
                    case IMMEDIATE:
                        *(ip++) = arg.immediate;
                        break;
                    case REGISTER:
                        *(ip++) = arg.reg;
                        break;
                    case CMPMODE:
                        *(ip++) = arg.cmp_mode;
                        break;
                    case FUNCTION:{
                        auto f = prog.functions.get(arg.function_name);
                        if(!f){
                            err_print(prog.find_token(arg.first_char), "Reference to unknown function: ", Q(arg.function_name));
                            return AsmError.LINK_ERROR;
                        }
                        *(ip++) = cast(uintptr_t)f.func;
                    }break;
                    case LABEL:{
                        if(auto label = arg.label_name in labels){
                            *(ip++) = *label;
                        }
                        else {
                            err_print(prog.find_token(arg.first_char), "Reference to unknown label: ", Q(arg.label_name));
                            return AsmError.LINK_ERROR;
                        }
                    }break;
                    case ARRAY:
                        *(ip++) = cast(uintptr_t)prog.arrays[arg.array].bdata.data.ptr;
                        break;
                    case VARIABLE:
                        if(auto var = arg.variable in prog.variable_table){
                            *(ip++) = cast(uintptr_t)*var;
                        }
                        else {
                            err_print(prog.find_token(arg.first_char), "Reference to unknown variable: ", Q(arg.variable));
                            return AsmError.LINK_ERROR;
                        }
                        break;

                    case CONSTANT:
                        err_print(prog.find_token(arg.first_char), "TODO");
                        return AsmError.LINK_ERROR;
                }
            }
        }
        return AsmError.NO_ERROR;
    }


    AsmError
    link(){
        if(auto err = allocate_arrays)
            return err;
        if(auto err = allocate_variables)
            return err;
        if(auto err = allocate_functions)
            return err;
        if(auto err = link_functions)
            return err;
        if(auto err = link_arrays)
            return err;
        if(auto err = link_variables)
            return err;
        return AsmError.NO_ERROR;
    }
}

size_t
calculate_function_size(AbstractFunction* func){
    size_t size = 0;
    foreach(ref inst; func.instructions[]){
        size += instruction_size(inst.instruction);
    }
    return size;
}

AsmError
link_asm(VAllocator* allocator, VAllocator* temp_allocator, FunctionTable* builtins, UnlinkedModule* unlinked, LinkedModule* prog){
    LinkContext ctx;
    ctx.allocator = allocator;
    ctx.temp_allocator = temp_allocator;
    ctx.builtins  = builtins;
    ctx.unlinked  = unlinked;
    ctx.prog.bytecode.allocator = allocator;
    ctx.prog.strings.bdata.allocator = allocator;
    ctx.prog.arrays.bdata.allocator = allocator;
    ctx.prog.functions.allocator = allocator;
    ctx.prog.functions.extend(builtins.items);
    ctx.prog.function_store.allocator = allocator;
    ctx.prog.variables.allocator = allocator;
    ctx.prog.variable_table.allocator = allocator;
    ctx.prog.source_text = prog.source_text;
    AsmError err = ctx.link();
    if(err){
        auto mess = ctx.errmess.data;
        if(!Fuzzing)fprintf(stderr, "%.*s\n", cast(int)mess.length, mess.ptr);
        ctx.errmess.dealloc;
        // TODO: cleanup ctx.prog
        return err;
    }
    *prog = ctx.prog;
    return AsmError.NO_ERROR;
}

Token
find_token(ref LinkedModule mod, const(char)* first_char){
    with(mod){
        const(char)[] text = source_text.data;
        assert(source_text.data.ptr);
        auto tokenizer = Tokenizer.from(text);
        Token tok = tokenizer.current_token_and_advance;
        while(tok._text != first_char){
            tok = tokenizer.current_token_and_advance;
            if(tok.type == TokenType.EOF){
                if(!Fuzzing)fprintf(stderr, "Unable to find: %p: %s\n", first_char, first_char);
                return tok;
            }
        }
        return tok;
    }
}


