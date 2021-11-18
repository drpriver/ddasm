import stringbuilder: StringBuilder, Q, H, E;
import argparse;
import zstring;
import get_input;
import term_util: get_cols, stdin_is_interactive;
import file_util;
import core.stdc.string: strlen, strerror, memcpy;
import core.stdc.stdio: fprintf, stdout, stderr, fread, stdin;
import allocator;
import barray;
import parse_numbers: parse_hex_inner, parse_unsigned_human;
import btable;
import box: Box;
import str_util: endswith;

static import dscript_to_dasm;

alias uintptr_t = size_t;

bool devnull = false;

version(Fuzz){
    enum Fuzzing = true;
    __gshared RecordingAllocator!Mallocator recorder;
    extern(C)
    int LLVMFuzzerTestOneInput(const ubyte *Data, size_t Size){
        const char* d = cast(const char*)Data;
        const char[] data = d[0 .. Size];

        UnlinkedProgram prog;
        auto va = VAllocator.from!(GlobalAllocator!recorder);
        int err = parse_asm_string(&va, data, &prog);
        va.free_all;
        return 0;
    }
}
else {
    enum Fuzzing = false;

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
        btext = btext.from(&va, sb.detach()[]);
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
        btext = btext.from(&va, sb.detach()[]);
    }
    if(highlevel || sourcefile[].endswith(".ds")){
        dscript_to_dasm.powerup();
        Box!(char[], Mallocator) dasmtext;
        auto data = btext.data;
        auto d = (cast(const(ubyte)*)data.ptr)[0 .. data.length];
        int err = dscript_to_dasm.compile_to_dasm(d, &dasmtext);
        if(err) return err;
        btext.dealloc();
        btext = btext.from(btext.allocator, dasmtext.data);
    }
    UnlinkedProgram prog;
    int err = parse_asm_string(&va, btext.data, &prog);
    if(err){
        // if(!Fuzzing)fprintf(stderr, "Parsing failed\n");
        return err;
    }
    LinkedProgram linked_prog;
    linked_prog.source_text = btext;
    {
        ArenaAllocator!(Mallocator) arena;
        scope(exit) arena.free_all;
        auto temp_va = VAllocator.from(&arena);
        err = link_asm(&va, &temp_va, BUILTINS, &prog, &linked_prog);
    }
    if(err){
        // if(!Fuzzing)fprintf(stderr, "Linking failed\n");
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
FunctionTable*
BUILTINS(){
    __gshared initialized = false;
    __gshared FunctionTable table;
    __gshared VAllocator va = VAllocator.from!(Mallocator);
    if(!initialized){
        initialized = true;
        table.allocator = &va;
        __gshared Function Printf1 = {
            native_function_aa: (uintptr_t fmt, uintptr_t arg){

                if(devnull) return;
                if(!Fuzzing)fprintf(stdout, cast(char*)fmt, arg);
                },
            type: FunctionType.NATIVE,
            n_args: 2,
            n_ret: 0,
        };
        __gshared Function Printf2 = {
            native_function_aaa: (uintptr_t fmt, uintptr_t arg, uintptr_t arg2){
                if(devnull) return;
                if(!Fuzzing)fprintf(stdout, cast(char*)fmt, arg, arg2);
                },
            type: FunctionType.NATIVE,
            n_args: 3,
            n_ret: 0,
        };
        __gshared Function Puts = {
            native_function_a: (uintptr_t arg){
                if(devnull) return;
                if(!Fuzzing)fprintf(stdout, "%s\n", cast(char*)arg);
                },
            type: FunctionType.NATIVE,
            n_args: 1,
            n_ret: 0,
        };
        __gshared Function Clock = {
            native_function_r: (){
                timespec tv;
                clock_gettime(6, &tv);
                auto result =  tv.tv_sec * 1000*1000*1000 + tv.tv_nsec;
                return result;
                },
            type: FunctionType.NATIVE,
            n_args: 0,
            n_ret: 1,
        };
        table["Printf1"] = FunctionInfo("Printf1", &Printf1);
        table["Printf2"] = FunctionInfo("Printf2", &Printf2);
        table["Puts"] = FunctionInfo("Puts", &Puts);
        table["Clock"] = FunctionInfo("Clock", &Clock);
    }
    return &table;
}

enum RunFlags: uint {
    NONE = 0,
    DEBUG = 1 << 0,
    DISASSEMBLE_EACH = 1 << 1,
}

struct Machine {
    enum CmpFlags: uintptr_t {
        ZERO = 1 << 0,
        CARRY = 1 << 1,
    }
    VAllocator allocator;
    Box!(void[], VAllocator) stack;
    uintptr_t[RegisterNames.max+1] registers;
    LinkedProgram* current_program;
    uintptr_t[256] call_stack;
    uintptr_t call_depth;
    bool halted;
    bool paused;
    bool badend;
    bool debugging;
    LineHistory!VAllocator debugger_history;
    int
    run(RunFlags flags = RunFlags.NONE)(LinkedProgram* prog, size_t stack_size, const char* debug_history_file = null){
        if(!prog.start || !prog.start.instructions_)
            return 1;
        halted = false;
        paused = false;
        badend = false;
        debugging = false;
        stack.allocator = &allocator;
        stack.resize(stack_size);
        current_program = prog;
        registers[RegisterNames.RIP] = cast(uintptr_t)current_program.start.instructions_;
        registers[RegisterNames.RSP] = cast(uintptr_t)stack.data.ptr;
        call_depth = 0;
        debugger_history.allocator = &allocator;
        if(debug_history_file){
            debugger_history.load_history(debug_history_file);
        }
        int result = run_interpreter!flags;
        static if(flags & RunFlags.DEBUG){
            if(result == BEGIN_CONTINUE){
                registers[RegisterNames.RIP] -= uintptr_t.sizeof;
                result = run_interpreter!(RunFlags.NONE);
            }
        }
        if(debug_history_file){
            debugger_history.dump(debug_history_file);
        }
        debugger_history.cleanup;
        return result;
    }

    int
    call_function(Function* func){with(RegisterNames){
        switch(func.type) with(FunctionType){
            default:
                return 1;
            case INTERPRETED:
                if(call_depth >= call_stack.length)
                    return 1;
                call_stack.ptr[call_depth++] = registers[RIP];
                registers[RIP] = cast(uintptr_t)func.instructions_;
                return 0;
            case NATIVE:{
                return call_native(func);
            }
        }
    }}

    int
    call_native(Function* func){with(RegisterNames){
        if(func.n_ret){
            switch(func.n_args){
                case 0:
                    registers[ROUT1] = func.native_function_r();
                    return 0;
                case 1:
                    registers[ROUT1] = func.native_function_ra(registers[RARG1]);
                    return 0;
                case 2:
                    registers[ROUT1] = func.native_function_raa(registers[RARG1], registers[RARG2]);
                    return 0;
                case 3:
                    registers[ROUT1] = func.native_function_raaa(registers[RARG1], registers[RARG2], registers[RARG3]);
                    return 0;
                default:
                    return 1;
            }
        }
        else {
            switch(func.n_args){
                case 0:
                    func.native_function_r();
                    return 0;
                case 1:
                    func.native_function_ra(registers[RARG1]);
                    return 0;
                case 2:
                    func.native_function_raa(registers[RARG1], registers[RARG2]);
                    return 0;
                case 3:
                    func.native_function_raaa(registers[RARG1], registers[RARG2], registers[RARG3]);
                    return 0;
                default:
                    return 1;
            }
        }
    }}

    int
    tail_call_function(Function* func){with(RegisterNames){
        switch(func.type) with(FunctionType){
            default:
                return 1;
            case INTERPRETED:
                registers[RIP] = cast(uintptr_t)func.instructions_;
                return 0;
            case NATIVE:{
                return call_native(func);
            }
        }
    }}

    void
    dump(){
        foreach(i, reg; registers){
            foreach(ri; registerinfos){
                if(ri.register == i){
                    if(!Fuzzing)fprintf(stderr, "[%s] = %#zx\n", ri.NAME.ptr, reg);
                    break;
                }
            }
        }
        for(size_t i = 0; i < 4; i++){
            if(!Fuzzing)fprintf(stderr, "ip[%zu] = %#zx\n", i, (cast(uintptr_t*)registers[RegisterNames.RIP])[i]);
        }
        StringBuilder!VAllocator temp;
        temp.allocator = &allocator;
        scope(exit) temp.cleanup;
        auto ip = cast(uintptr_t*)registers[RegisterNames.RIP];
        for(int i = 0; i < 4; i++){
            ip += disassemble_one_instruction(current_program, &temp, ip);
            temp.write("\n     ");
        }
        auto text = temp.borrow;
        if(!Fuzzing)fprintf(stderr, "Dis: %.*s\n", cast(int)text.length, text.ptr);
    }

    int
    debugger(){
        if(debugging) return 0;
        debugging = true;
        int result = run_interpreter!(RunFlags.DEBUG);
        debugging = false;
        if(result == 2){
            registers[RegisterNames.RIP] -= uintptr_t.sizeof;
            return 0;
        }
        return result;
    }

    void
    backtrace(){
        auto current = current_program.addr_to_function(cast(uintptr_t*)registers[RegisterNames.RIP]);
        if(!Fuzzing)fprintf(stderr, "[%zu] %.*s\n", call_depth, cast(int)current.name.length, current.name.ptr);
        for(ptrdiff_t i = call_depth-1; i >= 0; i--){
            auto fi = current_program.addr_to_function(cast(uintptr_t*)call_stack[i]);
            if(!Fuzzing)fprintf(stderr, "[%zu] %.*s\n", i, cast(int)fi.name.length, fi.name.ptr);
        }
    }

    void
    print_current_function(uintptr_t* ip){
        auto func = current_program.addr_to_function(ip);
        size_t i = 0;
        auto insts = func.func.instructions;
        StringBuilder!Mallocator sb;
        scope(exit) sb.cleanup;
        sb.FORMAT("function ", func.name, "\n");
        while(insts.length){
            sb.FORMAT(' ', insts.ptr==ip?"-> ":"   ");
            int num = disassemble_one_instruction(current_program, &sb, insts.ptr);
            sb.write('\n');
            insts = insts[num.. $];
            i++;
        }
        sb.write("end");
        auto text = sb.borrow;
        if(!Fuzzing)fprintf(stderr, "\n%.*s\n", cast(int)text.length, text.ptr);
    }
    enum {
        BEGIN_OK = 0,
        BEGIN_BAD = 1,
        BEGIN_CONTINUE = 2,
        BEGIN_END = 3,
    }

    void
    print_context(uintptr_t* ip){
        auto func = current_program.addr_to_function(ip);
        ptrdiff_t current = 0;
        ptrdiff_t i = 0;
        auto insts = func.func.instructions;
        while(insts.length){
            auto size = instruction_size(cast(Instruction)insts[0]);
            if(insts.ptr == ip)
                current = i;
            insts = insts[size .. $];
            i++;
        }
        insts = func.func.instructions;
        ptrdiff_t n_lines = i;
        i = 0;
        StringBuilder!Mallocator sb;
        scope(exit) sb.cleanup;
        sb.FORMAT("function ", func.name, "\n");
        if(current > 6){
            sb.write("    ...\n");
        }
        while(insts.length){
            if(i >= current -5 && i <= current+5){
                sb.FORMAT(' ', insts.ptr==ip?"-> ":"   ");
                disassemble_one_instruction(current_program, &sb, insts.ptr);
                sb.write('\n');
            }
            auto size = instruction_size(cast(Instruction)insts[0]);
            insts = insts[size .. $];
            i++;
        }
        if(n_lines <= current+5){
            }
        else
            sb.write("    ...\n");
        sb.write("end");
        auto text = sb.borrow;
        if(!Fuzzing)fprintf(stderr, "\n%.*s\n", cast(int)text.length, text.ptr);
    }

    int
    run_interpreter(RunFlags flags = RunFlags.NONE)(){ with(RegisterNames) with(Instruction){
        static if(uintptr_t.sizeof == 4)
            alias float_t = float;
        else
            alias float_t = double;

        Instruction
        next_instruction(){
            Instruction result = *cast(Instruction*)registers[RIP];
            registers[RIP]+= uintptr_t.sizeof;
            return result;
        }

        uintptr_t
        get_unsigned(){
            uintptr_t result = *cast(uintptr_t*)registers[RIP];
            registers[RIP]+= uintptr_t.sizeof;
            return result;
        }
        ptrdiff_t
        get_signed(){
            ptrdiff_t result = *cast(ptrdiff_t*)registers[RIP];
            registers[RIP]+= uintptr_t.sizeof;
            return result;
        }
        float_t
        get_float(){
            auto result = *cast(float_t*)registers[RIP];
            registers[RIP]+= uintptr_t.sizeof;
            return result;
        }

        uintptr_t*
        get_reg(){
            return &registers[get_unsigned()];
        }
        uintptr_t
        read_reg(){
            return registers[get_unsigned()];
        }
        float_t
        float_read_reg(){
            return *cast(float_t*)get_reg();
        }
        int
        check_cmp(uintptr_t cmp){
            switch(cmp) with(CmpFlags) with(CmpMode){
                case EQ:
                    return !!(registers[RFLAGS] & ZERO);
                case NE:
                    return !(registers[RFLAGS] & ZERO);
                case LT:
                    return (registers[RFLAGS] & (CARRY|ZERO)) == CARRY;
                case GT:
                    return (registers[RFLAGS] & (CARRY|ZERO)) == 0;
                case LE:
                    return !!(registers[RFLAGS] & (CARRY|ZERO));
                case GE:
                    return (registers[RFLAGS] & CARRY) != CARRY;
                case TRUE:
                    return 1;
                case FALSE:
                    return 0;
                default: 
                    return -1;
            }
        }

        static if(flags & RunFlags.DEBUG){
            int
            begin(Instruction inst){
                print_context((cast(uintptr_t*)registers[RIP])-1);
                char[1024] buff = void;
                for(;;){
                    auto len = get_input_line(&debugger_history, "> ", buff[]);
                    if(len < 0){
                        halted = true;
                        return BEGIN_BAD;
                    }
                    if(!len){
                        return BEGIN_OK;
                    }
                    switch(buff[0..len]){
                        case "next": case "n":
                            debugger_history.add_line(buff[0..len]);
                            return BEGIN_OK;
                        case "c": case "continue":
                            debugger_history.add_line(buff[0..len]);
                            return BEGIN_CONTINUE;
                        case "q": case "quit":
                            debugger_history.add_line(buff[0..len]);
                            halted = true;
                            return BEGIN_END;
                        case "l": case "list":
                            debugger_history.add_line(buff[0..len]);
                            print_current_function((cast(uintptr_t*)registers[RIP])-1);
                            continue;
                        case "d": case "dump":
                            debugger_history.add_line(buff[0..len]);
                            dump;
                            continue;
                        case "bt": case "backtrace": case "where":
                            debugger_history.add_line(buff[0..len]);
                            backtrace;
                            continue;

                        default: continue;
                    }
                }
            }
        }
        else static if(flags & RunFlags.DISASSEMBLE_EACH){
            pragma(inline, true)
            int
            begin(Instruction inst){
                auto ip = cast(uintptr_t*)registers[RIP];
                ip--;
                StringBuilder!Mallocator sb;
                scope(exit) sb.cleanup;
                disassemble_one_instruction(current_program, &sb, ip);
                auto text = sb.borrow;
                if(!Fuzzing)fprintf(stderr, "%.*s\n", cast(int)text.length, text.ptr);
                return BEGIN_OK;
            }
        }
        else {
            pragma(inline, true)
            int
            begin(Instruction inst){
                return BEGIN_OK;
            }
        }

        for(;;){
            auto inst = *cast(Instruction*)registers[RIP];
            registers[RIP] += uintptr_t.sizeof;
            final switch(inst){
                case HALT:
                    if(auto b = begin(HALT)) return b;
                    halted = true;
                    return 0;
                case ABORT:
                    if(auto b = begin(ABORT)) return b;
                    backtrace;
                    badend = true;
                    return 1;
                case NOP:
                    if(auto b = begin(NOP)) return b;
                    break;
                case READ:{
                    if(auto b = begin(READ)) return b;
                    auto dst = get_reg();
                    auto src = cast(uintptr_t*)read_reg();
                    *dst = *src;
                }break;
                case READ_I:{
                    if(auto b = begin(READ_I)) return b;
                    auto dst = get_reg();
                    auto src = cast(uintptr_t*)get_unsigned();
                    *dst = *src;
                }break;
                case MOVE_R:{
                    if(auto b = begin(MOVE_R)) return b;
                    auto dst = get_reg();
                    auto src = get_reg();
                    *dst = *src;
                }break;
                case LOCAL_READ:{
                    if(auto b = begin(LOCAL_READ)) return b;
                    auto dst = get_reg();
                    auto offset = get_unsigned();
                    auto src = cast(ubyte*)registers[RBP];
                    src += offset;
                    *dst = *cast(uintptr_t*)src;
                }break;
                case LOCAL_WRITE:{
                    if(auto b = begin(LOCAL_WRITE)) return b;
                    auto dst = cast(ubyte*)registers[RBP];
                    auto offset = get_unsigned();
                    dst += offset;
                    auto src = get_reg();
                    *cast(uintptr_t*)dst = *src;
                }break;
                case LOCAL_WRITE_I:{
                    if(auto b = begin(LOCAL_WRITE_I)) return b;
                    auto dst = cast(ubyte*)registers[RBP];
                    auto offset = get_unsigned();
                    dst += offset;
                    auto src = get_unsigned();
                    *cast(uintptr_t*)dst = src;
                }break;
                case MOVE_I:{
                    if(auto b = begin(MOVE_I)) return b;
                    auto dst = get_reg();
                    auto val = get_unsigned();
                    *dst = val;
                }break;
                case CMOVE_R:{
                    if(auto b = begin(CMOVE_R)) return b;
                    auto cmp_mode = get_unsigned();
                    auto dst = get_reg();
                    auto src = get_reg();
                    auto should_move = check_cmp(cmp_mode);
                    if(should_move == -1)
                        return 1;
                    if(should_move)
                        *dst = *src;
                }break;
                case CMOVE_I:{
                    if(auto b = begin(CMOVE_I)) return b;
                    auto cmp_mode = get_unsigned();
                    auto dst = get_reg();
                    auto val = get_unsigned();
                    auto should_move = check_cmp(cmp_mode);
                    if(should_move == -1)
                        return 1;
                    if(should_move)
                        *dst = val;
                }break;
                case WRITE_R:{
                    if(auto b = begin(WRITE_R)) return b;
                    auto dst = cast(uintptr_t*)read_reg();
                    auto val = read_reg();
                    *dst = val;
                }break;
                case WRITE_I:{
                    if(auto b = begin(WRITE_I)) return b;
                    auto dst = cast(uintptr_t*)read_reg();
                    auto val = get_unsigned();
                    *dst = val;
                }break;
                case ITOF:{
                    if(auto b = begin(ITOF)) return b;
                    auto dst = cast(float_t*)get_reg;
                    auto rhs = get_reg;
                    *dst = cast(typeof(*dst))*rhs;
                }break;
                case FTOI:{
                    if(auto b = begin(FTOI)) return b;
                    auto dst = get_reg;
                    auto rhs = cast(float_t*)get_reg;
                    *dst = cast(typeof(*dst))*rhs;
                }break;
                case NOT:{
                    if(auto b = begin(NOT)) return b;
                    auto dst = get_reg;
                    auto rhs = get_reg;
                    *dst = !*rhs;
                }break;
                case NEG:{
                    if(auto b = begin(NEG)) return b;
                    auto dst = get_reg;
                    auto rhs = get_reg;
                    *dst = ~*rhs;
                }break;
                case FADD_I:{
                    if(auto b = begin(FADD_I)) return b;
                    auto dst = cast(float_t*)get_reg;
                    auto lhs = cast(float_t*)get_reg;
                    auto rhs = get_float;
                    *dst = *lhs + rhs;
                }break;
                case FADD_R:{
                    if(auto b = begin(FADD_R)) return b;
                    auto dst = cast(float_t*)get_reg;
                    auto lhs = cast(float_t*)get_reg;
                    auto rhs = cast(float_t*)get_reg;
                    *dst = *lhs + *rhs;
                }break;
                case ADD_I:{
                    if(auto b = begin(ADD_I)) return b;
                    auto dst = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_unsigned;
                    *dst = (*lhs) + rhs;
                }break;
                case ADD_R:{
                    if(auto b = begin(ADD_R)) return b;
                    auto dst = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_reg;
                    *dst = (*lhs) + (*rhs);
                }break;
                case AND_I:{
                    if(auto b = begin(AND_I)) return b;
                    auto dst = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_unsigned;
                    *dst = (*lhs) & rhs;
                }break;
                case AND_R:{
                    if(auto b = begin(AND_R)) return b;
                    auto dst = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_reg;
                    *dst = (*lhs) & (*rhs);
                }break;
                case LOGICAL_AND_I:{
                    if(auto b = begin(LOGICAL_AND_I)) return b;
                    auto dst = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_unsigned;
                    *dst = (*lhs) && rhs;
                }break;
                case LOGICAL_AND_R:{
                    if(auto b = begin(LOGICAL_AND_R)) return b;
                    auto dst = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_reg;
                    *dst = (*lhs) && (*rhs);
                }break;
                case OR_I:{
                    if(auto b = begin(OR_I)) return b;
                    auto dst = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_unsigned;
                    *dst = (*lhs) | rhs;
                }break;
                case OR_R:{
                    if(auto b = begin(OR_R)) return b;
                    auto dst = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_reg;
                    *dst = (*lhs) | (*rhs);
                }break;
                case LOGICAL_OR_I:{
                    if(auto b = begin(LOGICAL_OR_I)) return b;
                    auto dst = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_unsigned;
                    *dst = (*lhs) || rhs;
                }break;
                case LOGICAL_OR_R:{
                    if(auto b = begin(LOGICAL_OR_R)) return b;
                    auto dst = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_reg;
                    *dst = (*lhs) || (*rhs);
                }break;
                case XOR_I:{
                    if(auto b = begin(XOR_I)) return b;
                    auto dst = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_unsigned;
                    *dst = (*lhs) ^ rhs;
                }break;
                case XOR_R:{
                    if(auto b = begin(XOR_R)) return b;
                    auto dst = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_reg;
                    *dst = (*lhs) ^ (*rhs);
                }break;
                case SUB_I:{
                    if(auto b = begin(SUB_I)) return b;
                    auto dst = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_unsigned;
                    *dst = (*lhs) - rhs;
                }break;
                case SUB_R:{
                    if(auto b = begin(SUB_R)) return b;
                    auto dst = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_reg;
                    *dst = (*lhs) - (*rhs);
                }break;
                case MUL_I:{
                    if(auto b = begin(MUL_I)) return b;
                    auto dst = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_unsigned;
                    *dst = (*lhs) * rhs;
                }break;
                case MUL_R:{
                    if(auto b = begin(MUL_R)) return b;
                    auto dst = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_reg;
                    *dst = (*lhs) * (*rhs);
                }break;
                case SHIFTL_I:{
                    if(auto b = begin(SHIFTL_I)) return b;
                    auto dst = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_unsigned;
                    *dst = (*lhs) << rhs;
                }break;
                case SHIFTL_R:{
                    if(auto b = begin(SHIFTL_R)) return b;
                    auto dst = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_reg;
                    *dst = (*lhs) << (*rhs);
                }break;
                case SHIFTR_I:{
                    if(auto b = begin(SHIFTR_I)) return b;
                    auto dst = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_unsigned;
                    *dst = (*lhs) >> rhs;
                }break;
                case SHIFTR_R:{
                    if(auto b = begin(SHIFTR_R)) return b;
                    auto dst = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_reg;
                    *dst = (*lhs) >> (*rhs);
                }break;
                case DIV_I:{
                    if(auto b = begin(DIV_I)) return b;
                    auto dst = get_reg;
                    auto dst2 = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_unsigned;
                    if(!rhs)
                        *dst = 0;
                    else
                        *dst = *lhs / rhs;
                    *dst2 = *lhs - (*dst)*rhs;
                }break;
                case DIV_R:{
                    if(auto b = begin(DIV_R)) return b;
                    auto dst = get_reg;
                    auto dst2 = get_reg;
                    auto lhs = get_reg;
                    auto rhs = get_reg;
                    if(!*rhs)
                        *dst = 0;
                    else
                        *dst = *lhs / *rhs;
                    *dst2 = *lhs - (*dst)*(*rhs);
                }break;
                case PUSH_I:{
                    if(auto b = begin(PUSH_I)) return b;
                    auto src = get_unsigned;
                    *cast(uintptr_t*)registers[RSP] = src;
                    registers[RSP] += uintptr_t.sizeof;
                }break;
                case PUSH_R:{
                    if(auto b = begin(PUSH_R)) return b;
                    auto src = read_reg;
                    *cast(uintptr_t*)registers[RSP] = src;
                    registers[RSP] += uintptr_t.sizeof;
                }break;
                case POP:{
                    if(auto b = begin(POP)) return b;
                    auto dst = get_reg;
                    registers[RSP] -= uintptr_t.sizeof;
                    *dst = *cast(uintptr_t*)registers[RSP];
                }break;
                case CALL_I:{
                    if(auto b = begin(CALL_I)) return b;
                    Function* f = cast(Function*)get_unsigned;
                    int err = call_function(f);
                    if(err) {
                        backtrace;
                        badend = true;
                        return err;
                    }
                }break;
                case CALL_R:{
                    if(auto b = begin(CALL_R)) return b;
                    Function* f = cast(Function*)read_reg;
                    int err = call_function(f);
                    if(err) {
                        backtrace;
                        badend = true;
                        return err;
                    }
                }break;
                case TAIL_CALL_I:{
                    if(auto b = begin(TAIL_CALL_I)) return b;
                    Function* f = cast(Function*)get_unsigned;
                    int err = tail_call_function(f);
                    if(err) {
                        backtrace;
                        badend = true;
                        return err;
                    }
                    if(f.type == FunctionType.NATIVE){
                        if(!call_depth){
                            halted = true;
                            return 0;
                        }
                        registers[RIP] = call_stack[--call_depth];
                    }
                }break;
                case TAIL_CALL_R:{
                    if(auto b = begin(TAIL_CALL_R)) return b;
                    Function* f = cast(Function*)read_reg;
                    int err = tail_call_function(f);
                    if(err) {
                        backtrace;
                        badend = true;
                        return err;
                    }
                    if(f.type == FunctionType.NATIVE){
                        if(!call_depth){
                            halted = true;
                            return 0;
                        }
                        registers[RIP] = call_stack[--call_depth];
                    }
                }break;
                case RET:{
                    if(auto b = begin(RET)) return b;
                    if(!call_depth){
                        halted = true;
                        return 0;
                    }
                    registers[RIP] = call_stack[--call_depth];
                }break;
                case JUMP_ABS_I:{
                    if(auto b = begin(JUMP_ABS_I)) return b;
                    auto cmp_mode = get_unsigned;
                    auto amount = get_unsigned;
                    int should_jmp = check_cmp(cmp_mode);
                    if(should_jmp == -1) {
                        badend = true;
                        return 1;
                    }
                    if(should_jmp){
                        registers[RIP] = amount;
                    }
                }break;
                case JUMP_REL_I:{
                    if(auto b = begin(JUMP_REL_I)) return b;
                    auto cmp_mode = get_unsigned;
                    auto amount = get_unsigned;
                    int should_jmp = check_cmp(cmp_mode);
                    if(should_jmp == -1) {
                        badend = true;
                        return 1;
                    }
                    if(should_jmp){
                        registers[RIP] += amount*uintptr_t.sizeof;
                    }
                }break;
                case JUMP_R:{
                    if(auto b = begin(JUMP_R)) return b;
                    auto cmp_mode = get_unsigned;
                    auto jmp_mode = get_unsigned;
                    auto amount = read_reg;
                    int should_jmp = check_cmp(cmp_mode);
                    if(should_jmp == -1) {
                        badend = true;
                        return 1;
                    }
                    if(should_jmp){
                        registers[RIP] = amount;
                    }
                }break;
                case CMP_R:{
                    if(auto b = begin(CMP_R)) return b;
                    registers[RFLAGS] = 0;
                    auto lhs = read_reg;
                    auto rhs = read_reg;
                    if(lhs == rhs){
                        registers[RFLAGS] = CmpFlags.ZERO;
                    }
                    else if(lhs < rhs){
                        registers[RFLAGS] = CmpFlags.CARRY;
                    }
                }break;
                case CMP_I:{
                    if(auto b = begin(CMP_I)) return b;
                    registers[RFLAGS] = 0;
                    auto lhs = read_reg;
                    auto rhs = get_unsigned;
                    if(lhs == rhs){
                        registers[RFLAGS] = CmpFlags.ZERO;
                    }
                    else if(lhs < rhs){
                        registers[RFLAGS] = CmpFlags.CARRY;
                    }
                }break;
                case SCMP_R:{
                    if(auto b = begin(SCMP_R)) return b;
                    registers[RFLAGS] = 0;
                    auto lhs = cast(ptrdiff_t)read_reg;
                    auto rhs = cast(ptrdiff_t)read_reg;
                    if(lhs == rhs){
                        registers[RFLAGS] = CmpFlags.ZERO;
                    }
                    else if(lhs < rhs){
                        registers[RFLAGS] = CmpFlags.CARRY;
                    }
                }break;
                case SCMP_I:{
                    if(auto b = begin(SCMP_I)) return b;
                    registers[RFLAGS] = 0;
                    auto lhs = cast(ptrdiff_t)read_reg;
                    auto rhs = get_signed;
                    if(lhs == rhs){
                        registers[RFLAGS] = CmpFlags.ZERO;
                    }
                    else if(lhs < rhs){
                        registers[RFLAGS] = CmpFlags.CARRY;
                    }
                }break;
                case DUMP:
                    if(auto b = begin(DUMP)) return b;
                    dump;
                    break;
                case MSG:
                    if(auto b = begin(MSG)) return b;
                    if(!Fuzzing)fprintf(stderr, "%s\n", cast(char*)get_unsigned);
                    break;
                case MEMCPY_I:{
                    if(auto b = begin(MEMCPY_I)) return b;
                    auto dst = cast(void*)read_reg;
                    auto src = cast(void*)read_reg;
                    auto n = get_unsigned;
                    switch(n){
                        case 1  : memcpy(dst, src, 1); break;
                        case 2  : memcpy(dst, src, 2); break;
                        case 4  : memcpy(dst, src, 4); break;
                        case 8  : memcpy(dst, src, 8); break;
                        case 16 : memcpy(dst, src, 16); break;
                        default : memcpy(dst, src, n); break;
                    }
                }break;
                case MEMCPY_R:{
                    if(auto b = begin(MEMCPY_R)) return b;
                    auto dst = cast(void*)read_reg;
                    auto src = cast(void*)read_reg;
                    auto n = read_reg;
                    switch(n){
                        case 1  : memcpy(dst, src, 1); break;
                        case 2  : memcpy(dst, src, 2); break;
                        case 4  : memcpy(dst, src, 4); break;
                        case 8  : memcpy(dst, src, 8); break;
                        case 16 : memcpy(dst, src, 16); break;
                        default : memcpy(dst, src, n); break;
                    }
                }break;
                case PAUSE:
                    if(auto b = begin(PAUSE)) return b;
                    paused = true;
                    return 0;
                case DEBUG_OP:{
                    if(auto b = begin(DEBUG_OP)) return b;
                    int result = debugger;
                    if(paused || badend || result || halted)
                        return result;
                    }break;
                case BACKTRACE:
                    if(auto b = begin(BACKTRACE)) return b;
                    backtrace;
                    break;
                case LEA:{
                    if(auto b = begin(LEA)) return b;
                        auto dst = get_reg();
                        auto x = get_reg();
                        auto a = get_unsigned();
                        auto y = get_reg();
                        auto z = get_unsigned();
                        *dst = *x + a*(*y) + z;
                    }break;
            }
        }
    }
    }
}


int
parse_asm_string(VAllocator* allocator, const(char)[] text, UnlinkedProgram* prog){
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

enum TokenType: ubyte {
    BANG = '!',
    AT = '@',
    POUND = '#',
    DOLLAR = '$',
    PERCENT = '%',
    CARROT = '^',
    AMPERSAN = '&',
    ASTERISK = '*',
    LEFTPAREN = '(',
    RIGHTPAREN = ')',
    DASH = '-',
    EQUALS = '=',
    PLUS = '+',
    LEFTSQUAREBRACKET = '[',
    RIGHTSQUAREBRACKET = ']',
    LEFTCURLYBRACKET = '{',
    RIGHTCURLYBRACKET = '}',
    BACKSLASH = '\\',
    PIPE = '|',
    SLASH = '/',
    COMMA = ',',
    LESSTHAN = '<',
    GREATERTHAN = '>',
    DOT = '.',
    QUESTION = '?',
    SEMICOLON = ';',
    COLON = ':',
    APOSTROPHE = '\'',
    QUOTATION = '"',
    BACKTICK = '`',
    TILDE = '~',
    SPACE = ' ',
    NEWLINE = '\n',
    CARRIAGERETURN = '\r',
    TAB = '\t',
    UNPRINTABLE = 0,
    NUMBER,
    IDENTIFIER,
    EOF,
}

struct Token {
    TokenType type;
    ubyte length;
    ushort column;
    uint line;
    const(char)* _text;
    const(char)[] text() {
        return _text[0..length];
    }
}

Token
skip_comment(ref Tokenizer tokenizer, Token tok){
    with(TokenType){
        if(tok.type != POUND) return tok;
        while(tok.type != EOF && tok.type != NEWLINE){
            tok = tokenizer.current_token_and_advance;
        }
        return tok;
    }
}

struct Tokenizer {
    const(char)[] text;
    size_t cursor;
    uint line;
    ushort column;
    Token current_token;
    Token next_token;
    Token current_token_and_advance(){
        auto result = current_token;
        advance();
        return result;
    }
    static
    Tokenizer
    from(const(char)[] text){ with(TokenType){
        Tokenizer result;
        result.text = text;
        result.line = 1;
        result.column = 1;
        result._tokenizer_a_token(&result.current_token);
        if(result.current_token.type == NEWLINE){
            result.line++;
            result.column=1;
            }
        if(result.current_token.type == EOF){
            result.next_token.type = EOF;
            return result;
        }
        result._tokenizer_a_token(&result.next_token);
        return result;
    }
    }
    void
    _tokenizer_a_token(Token* tok){ with(TokenType){
        tok.line = line;
        tok.column = column;
        if(cursor == text.length){
            tok._text = "(EOF)";
            tok.length = 5;
            tok.type = EOF;
            return;
        }
        tok._text = &text[cursor];
        auto first_c = text[cursor];
        switch(first_c){
            // 93
            // 35
            case '_':
            case 'a': .. case 'z':
            case 'A': .. case 'Z':
                tok.type = IDENTIFIER;
                break;
            case '0': .. case '9':
                tok.type = NUMBER;
                break;
            case 0: .. case 8:
            case 11: .. case 12:
            case 14: .. case 31:
            case 127:
                tok.type = UNPRINTABLE;
                break;
            // All other values are valid
            default:
                tok.type = cast(TokenType)first_c;
                break;
        }
        size_t token_length = 1;
        auto cur = cursor + 1;
        ushort col = cast(ushort)(column + 1);
        auto s = text;
        enum MAX_TOK_LENGTH=255;
        if(tok.type == IDENTIFIER){
            for(;token_length < MAX_TOK_LENGTH && cur < s.length; ++token_length, ++cur, ++col){
                auto c = s[cur];
                switch(c){
                    case 0: .. case 47:
                    case 58: .. case 64:
                    case 91: .. case 94:
                    case 96:
                    case 123: .. case 127:
                        break;
                    default:
                    // case 48: .. case 57:
                    // case 65: .. case 90:
                    // case 95:
                    // case 97: .. case 122:
                    // case 128: .. case 255:
                        continue;
                }
                break;
            }
        }
        else if(tok.type == NUMBER){
            for(;token_length < MAX_TOK_LENGTH && cur < s.length; ++token_length, ++cur, ++col){
                auto c = s[cur];
                if((c<='9') & (c >='0') || ((c<='z') & (c>= 'a')) || ((c<='Z') & (c>='A')) || c == '.')
                    {}
                else
                    break;
            }
        }
        else if(tok.type == SPACE){
            for(;token_length < MAX_TOK_LENGTH && cur < s.length; ++token_length, ++cur, ++col){
                auto c = s[cur];
                if(c == ' ')
                    {}
                else
                    break;
            }
        }
        tok.length = cast(ubyte)token_length;
        column = col;
        cursor = cur;
    }
    }
    void
    advance(){ with(TokenType){
        if(current_token.type == EOF)
            return;
        if(next_token.type == EOF){
            current_token = next_token;
            return;
        }
        current_token = next_token;
        if(next_token.type == NEWLINE){
            line++;
            column = 1;
        }
        _tokenizer_a_token(&next_token);
        return;
    }
    }
}

enum AsmError: int {
    NO_ERROR = 0,
    PARSE_ERROR,
    LINK_ERROR,
}

struct ParseContext{
    Tokenizer tokenizer;
    VAllocator* allocator;
    UnlinkedProgram prog;
    ZString errmess;
    void
    err_print(A...)(Token tok, A args){
        StringBuilder!Mallocator sb;
        sb.FORMAT(tok.line, ':', tok.column, ": ParseError: ");
        foreach(a; args)
            sb.write(a);
        errmess = sb.detach;
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
                        }
                        prog.variables.push(var);
                    }
                    else {
                        err_print(tok, "Only function or variable declarations are legal at global scope, not ", Q(tok.text));
                        return PARSE_ERROR;
                    }
                }
                else {
                    err_print(tok, "Only function or variable declarations are legal at global scope, not ", Q(tok.text));
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
                foreach(ri; registerinfos){
                    if(ri.name == text){
                        result.reg = ri.register;
                        result.kind = REGISTER;
                        return result;
                    }
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

struct UnlinkedProgram{
    Barray!(AbstractFunction, VAllocator) functions;
    Barray!(AbstractVariable, VAllocator) variables;
    Barray!(AbstractArray,    VAllocator) arrays;
}

struct AbstractFunction {
    const(char)* first_char;
    const(char)[] name;
    int n_args;
    Barray!(AbstractInstruction, VAllocator) instructions;
    bool finished;
}

struct AbstractInstruction {
    const(char)* first_char;
    const(char)[] label;
    Instruction instruction;
    enum MAX_ARGS = 5;
    Argument[MAX_ARGS] args;
    int n_args;
}

struct AbstractVariable {
    const(char)[] name;
    Argument value;
    Token tok;
}

struct AbstractArray {
    uintptr_t id;
    Barray!(Argument, VAllocator) array;
}

// bitflags
enum ArgumentKind: uint {
    UNSET          = 0,
    STRING         = 0b1,
    IMMEDIATE      = 0b10,
    REGISTER       = 0b100,
    CMPMODE        = 0b1000,
    FUNCTION       = 0b10000,
    LABEL          = 0b100000,
    ARRAY          = 0b1000000,
    VARIABLE       = 0b10000000,
    CONSTANT       = 0b100000000,
}

struct Argument {
    ArgumentKind kind;
    const(char)* first_char;
    union {
        uintptr_t array;
        const(char)[] text; // this is borrow and unescaped
        uintptr_t immediate;
        RegisterNames reg;
        CmpMode cmp_mode;
        const(char)[] function_name;
        const(char)[] label_name;
        const(char)[] token;
        const(char)[] variable;
        const(char)[] constant;
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

struct InstructionInfo {
    Instruction instruction;
    const(char)[] NAME;
    const(char)[] asm_name;
    const(ArgumentKind)[] args;
}

// instructs with the same asm name should have distinct argument
// types so they can be distinguished.
immutable InstructionInfo[Instruction.max+1] INSTRUCTION_INFOS = {
    // this is in an IIFE just for the with
    with(Instruction) with(ArgumentKind){
    // IMM is something that can be coerced to a numeric constant
    // at link time.
    enum IMM = IMMEDIATE | STRING | FUNCTION | LABEL | ARRAY | VARIABLE;
    immutable i = [IMM];
    immutable ii = [IMM, IMM];
    immutable r = [REGISTER];
    immutable rr = [REGISTER, REGISTER];
    immutable ri = [REGISTER, IMM];
    immutable ir = [IMM, REGISTER];
    immutable rrr = [REGISTER, REGISTER, REGISTER];
    immutable rri = [REGISTER, REGISTER, IMM];
    immutable rir = [REGISTER, IMM, REGISTER];
    immutable rii = [REGISTER, IMM, IMM];
    immutable rrrr = [REGISTER, REGISTER, REGISTER, REGISTER];
    immutable rrri = [REGISTER, REGISTER, REGISTER, IMM];
    immutable rriri = [REGISTER, REGISTER, IMM, REGISTER, IMM];
    alias I = InstructionInfo;
    immutable result = cast(immutable)[
        I(ABORT,            "ABORT",            "abort"),
        I(NOP,              "NOP",              "nop"),
        I(READ,             "READ",             "read",  rr),
        I(READ_I,           "READ_I",           "read",  ri),
        I(LOCAL_READ,       "LOCAL_READ",       "local_read", ri),
        I(LOCAL_WRITE,      "LOCAL_WRITE",      "local_write", ir),
        I(LOCAL_WRITE_I,    "LOCAL_WRITE_I",    "local_write", ii),
        I(WRITE_R,          "WRITE_R",          "write", rr),
        I(WRITE_I,          "WRITE_I",          "write", ri),
        I(MOVE_R,           "MOVE_R",           "move",  rr),
        I(MOVE_I,           "MOVE_I",           "move",  ri),
        I(CMOVE_R,          "CMOVE_R",          "cmov", [CMPMODE, REGISTER, REGISTER]),
        I(CMOVE_I,          "CMOVE_I",          "cmov", [CMPMODE, REGISTER, IMM]),
        I(HALT,             "HALT",             "halt"),
        I(XOR_I,            "XOR_I",            "xor", rri),
        I(XOR_R,            "XOR_R",            "xor", rrr),
        I(OR_I,             "OR_I",             "or", rri),
        I(OR_R,             "OR_R",             "or", rrr),
        I(AND_I,            "AND_I",            "and", rri),
        I(AND_R,            "AND_R",            "and", rrr),
        I(LOGICAL_AND_I,    "LOGICAL_AND_I",    "logical_and", rri),
        I(LOGICAL_AND_R,    "LOGICAL_AND_R",    "logical_and", rrr),
        I(LOGICAL_OR_I,     "LOGICAL_OR_I",     "logical_or", rri),
        I(LOGICAL_OR_R,     "LOGICAL_OR_R",     "logical_or", rrr),
        I(ADD_I,            "ADD_I",            "add", rri),
        I(ADD_R,            "ADD_R",            "add", rrr),
        I(SUB_I,            "SUB_I",            "sub", rri),
        I(SUB_R,            "SUB_R",            "sub", rrr),
        I(FADD_I,           "FADD_I",           "fadd", rri),
        I(FADD_R,           "FADD_R",           "fadd", rrr),
        I(MUL_I,            "MUL_I",            "mul", rri),
        I(MUL_R,            "MUL_R",            "mul", rrr),
        I(DIV_I,            "DIV_I",            "div", rrri),
        I(DIV_R,            "DIV_R",            "div", rrrr),
        I(PUSH_I,           "PUSH_I",           "push", i),
        I(PUSH_R,           "PUSH_R",           "push", r),
        I(POP,              "POP",              "pop", r),
        I(CALL_I,           "CALL_I",           "call", i),
        I(CALL_R,           "CALL_R",           "call", r),
        I(TAIL_CALL_I,      "TAIL_CALL_I",      "tail_call", i),
        I(TAIL_CALL_R,      "TAIL_CALL_R",      "tail_call", r),
        I(RET,              "RET",              "ret"),
        I(JUMP_ABS_I,       "JUMP_ABS_I",       "jump", [CMPMODE, LABEL]),
        I(JUMP_REL_I,       "JUMP_REL_I",       "jump", [CMPMODE, IMMEDIATE]),
        I(JUMP_R,           "JUMP_R",           "jump", [CMPMODE, REGISTER]),
        I(CMP_R,            "CMP_R",            "cmp", rr),
        I(CMP_I,            "CMP_I",            "cmp", ri),
        I(SCMP_R,           "SCMP_R",           "scmp", rr),
        I(SCMP_I,           "SCMP_I",           "scmp", ri),
        I(DUMP,             "DUMP",             "dump"),
        I(SHIFTL_I,         "SHIFTL_I",         "shl", rri),
        I(SHIFTL_R,         "SHIFTL_R",         "shl", rrr),
        I(SHIFTR_I,         "SHIFTR_I",         "shr", rri),
        I(SHIFTR_R,         "SHIFTR_R",         "shr", rrr),
        I(PAUSE,            "PAUSE",            "pause"),
        I(MSG,              "MSG",              "msg", [STRING]),
        I(DEBUG_OP,         "DEBUG_OP",         "debug"),
        I(MEMCPY_I,         "MEMCPY_I",         "memcpy", rri),
        I(MEMCPY_R,         "MEMCPY_R",         "memcpy", rrr),
        I(BACKTRACE,        "BACKTRACE",        "bt"),
        I(ITOF,             "ITOF",             "itof", rr),
        I(FTOI,             "FTOI",             "ftoi", rr),
        I(NOT,              "NOT",              "not", rr),
        I(NEG,              "NEG",              "neg", rr),
        I(LEA,              "LEA",              "lea", rriri),
    ];
    foreach(index, res; result)
        assert(index == res.instruction, "mismatch for instruction ");
    return result;
    }
}();

BTable!(string, Barray!(InstructionInfo, Mallocator), Mallocator)
InstructionTable(){
    static __gshared BTable!(string, Barray!(InstructionInfo, Mallocator), Mallocator) result;
    static __gshared initialized = false;
    if(!initialized){
        initialized = true;
        foreach(ii; INSTRUCTION_INFOS){
            if(auto arr = ii.asm_name in result){
                arr.push(ii);
            }
            else {
                Barray!(InstructionInfo, Mallocator) arr;
                arr.push(ii);
                result[ii.asm_name] = arr;
            }
        }
    }
    return result;
}

struct RegisterInfo {
    RegisterNames register;
    string NAME;
    string name;
}

immutable RegisterInfo[28] registerinfos = [
    RegisterInfo(RegisterNames.R0,      "R0",       "r0"),
    RegisterInfo(RegisterNames.R1,      "R1",       "r1"),
    RegisterInfo(RegisterNames.R2,      "R2",       "r2"),
    RegisterInfo(RegisterNames.R3,      "R3",       "r3"),
    RegisterInfo(RegisterNames.R4,      "R4",       "r4"),
    RegisterInfo(RegisterNames.R5,      "R5",       "r5"),
    RegisterInfo(RegisterNames.R6,      "R6",       "r6"),
    RegisterInfo(RegisterNames.R7,      "R7",       "r7"),
    RegisterInfo(RegisterNames.R8,      "R8",       "r8"),
    RegisterInfo(RegisterNames.R9,      "R9",       "r9"),
    RegisterInfo(RegisterNames.R10,     "R10",      "r10"),
    RegisterInfo(RegisterNames.R11,     "R11",      "r11"),
    RegisterInfo(RegisterNames.R12,     "R12",      "r12"),
    RegisterInfo(RegisterNames.R13,     "R13",      "r13"),
    RegisterInfo(RegisterNames.R14,     "R14",      "r14"),
    RegisterInfo(RegisterNames.R15,     "R15",      "r15"),
    RegisterInfo(RegisterNames.RARG1,   "RARG1",    "rarg1"),
    RegisterInfo(RegisterNames.RARG2,   "RARG2",    "rarg2"),
    RegisterInfo(RegisterNames.RARG3,   "RARG3",    "rarg3"),
    RegisterInfo(RegisterNames.RARG4,   "RARG4",    "rarg4"),
    RegisterInfo(RegisterNames.ROUT1,   "ROUT1",    "rout1"),
    RegisterInfo(RegisterNames.ROUT2,   "ROUT2",    "rout2"),
    RegisterInfo(RegisterNames.RJUNK,   "RJUNK",    "rjunk"),
    RegisterInfo(RegisterNames.RSP,     "RSP",      "rsp"),
    RegisterInfo(RegisterNames.RBP,     "RBP",      "rbp"),
    RegisterInfo(RegisterNames.RIP,     "RIP",      "rip"),
    RegisterInfo(RegisterNames.RFLAGS,  "RFLAGS",   "rflags"),
    RegisterInfo(RegisterNames.RERROR,  "RERROR",   "rerror"),
];

enum Instruction: uintptr_t {
    ABORT,
    NOP,
    READ,
    READ_I,
    LOCAL_READ,
    LOCAL_WRITE,
    LOCAL_WRITE_I,
    WRITE_R,
    WRITE_I,
    MOVE_R,
    MOVE_I,
    CMOVE_R,
    CMOVE_I,
    HALT,
    XOR_I,
    XOR_R,
    OR_I,
    OR_R,
    AND_I,
    AND_R,
    LOGICAL_AND_I,
    LOGICAL_AND_R,
    LOGICAL_OR_I,
    LOGICAL_OR_R,
    ADD_I,
    ADD_R,
    SUB_I,
    SUB_R,
    FADD_I,
    FADD_R,
    MUL_I,
    MUL_R,
    DIV_I,
    DIV_R,
    PUSH_I,
    PUSH_R,
    POP,
    CALL_I,
    CALL_R,
    TAIL_CALL_I,
    TAIL_CALL_R,
    RET,
    JUMP_ABS_I,
    JUMP_REL_I,
    JUMP_R,
    CMP_R,
    CMP_I,
    SCMP_R,
    SCMP_I,
    DUMP,
    SHIFTL_I,
    SHIFTL_R,
    SHIFTR_I,
    SHIFTR_R,
    PAUSE,
    MSG,
    DEBUG_OP,
    MEMCPY_I,
    MEMCPY_R,
    BACKTRACE,
    ITOF,
    FTOI,
    NOT,
    NEG,
    LEA,
}
enum RegisterNames:uintptr_t {
    R0      = 0,  R1  =  1, R2  = 2,  R3 = 3, R4 = 4, R5 = 5, R6 = 6, R7 = 7, R8 = 8, R9 = 9,
    R10     = 10, R11 = 11, R12 = 12, R13= 13, // Arguments
    R14     = 14, R15 = 15, // Return
    RARG1   = R10,
    RARG2   = R11,
    RARG3   = R12,
    RARG4   = R13,
    RARGMAX = R13+1,
    ROUT1   = R14,
    ROUT2   = R15,
    RJUNK = 16,
    RSP = 17,
    RBP = 18,
    RIP = 19,
    RFLAGS = 20,
    RERROR = 21,
}
enum CmpMode: uintptr_t {
   EQ = 0,
   NE = 1,
   LT = 2,
   GT = 3,
   GE = 4,
   LE = 5,
   TRUE = 6,
   FALSE = 7,
}

struct CmpModeInfo {
    CmpMode mode;
    const(char)[] NAME;
    const(char)[] name;
}

immutable CmpModeInfo[8] CmpModes = [
    CmpModeInfo(CmpMode.EQ, "EQ", "eq"),
    CmpModeInfo(CmpMode.NE, "NE", "ne"),
    CmpModeInfo(CmpMode.LT, "LT", "lt"),
    CmpModeInfo(CmpMode.GT, "GT", "gt"),
    CmpModeInfo(CmpMode.GE, "GE", "ge"),
    CmpModeInfo(CmpMode.LE, "LE", "le"),
    CmpModeInfo(CmpMode.TRUE, "TRUE", "true"),
    CmpModeInfo(CmpMode.FALSE, "FALSE", "false"),
];


alias IntegerArray = Barray!(uintptr_t, VAllocator);
alias FunctionTable = BTable!(const(char)[], FunctionInfo, VAllocator);

struct Variable {
    const(char)[] name;
    uintptr_t value;
}

struct FunctionInfo {
    const(char)[] name;
    Function* func;
}

enum FunctionType: ubyte {
    INTERPRETED = 0,
    NATIVE = 1,
    // first arg is the interpreter
    // NATIVE_TAKES_INTERPRETER = 2,
}
struct Function {
    union {
        uintptr_t* instructions_;
        void function() native_function_;
        void function(uintptr_t) native_function_a;
        void function(uintptr_t, uintptr_t) native_function_aa;
        void function(uintptr_t, uintptr_t, uintptr_t) native_function_aaa;
        uintptr_t function() native_function_r;
        uintptr_t function(uintptr_t) native_function_ra;
        uintptr_t function(uintptr_t, uintptr_t) native_function_raa;
        uintptr_t function(uintptr_t, uintptr_t, uintptr_t) native_function_raaa;
    }
    FunctionType type;
    ubyte n_args;
    ubyte n_ret;
    ubyte pad;
    // if D had bitfields I could use the padding byte.
    uint length;

    uintptr_t[] instructions(){
        return instructions_[0..length];
    }
}
static assert(Function.sizeof == 16);

struct LinkedProgram {
    Box!(const(char)[], VAllocator) source_text;
    Box!(uintptr_t[], VAllocator) bytecode;
    Barray!(ZString, VAllocator) strings;
    Barray!(IntegerArray, VAllocator) arrays;
    FunctionTable functions;
    Box!(Function[], VAllocator) function_store;
    // storage for the variables
    Box!(uintptr_t[], VAllocator) variables;
    // table to look variables up by name
    BTable!(const(char)[], uintptr_t*, VAllocator) variable_table;
    Function* start;

    FunctionInfo
    addr_to_function(uintptr_t* ip){
        foreach(fi; functions.values){
            auto func = fi.func;
            uintptr_t* begin = func.instructions_;
            uintptr_t* end = func.instructions_ + func.length;
            if(ip >= begin && ip < end)
                return fi;
        }
        return FunctionInfo();
    }

    Token
    find_token(const(char)* first_char){
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

int
disassemble_one_instruction(SB)(LinkedProgram* prog, SB* sb, uintptr_t* ip){
    with(Instruction) with(ArgumentKind){
        uintptr_t inst = *ip;
        if(inst <= Instruction.max){
            auto ii = &INSTRUCTION_INFOS[inst];
            sb.write(ii.asm_name);
            if(ii.args.length){
                foreach(i, kind; ii.args){
                    sb.write(' ');
                    auto value = ip[i+1];
                    switch(kind){
                        default:{
                            auto func = cast(Function*)value;
                            foreach(v; prog.functions.values){
                                if(v.func == func){
                                    sb.write("function ");
                                    sb.write(v.name);
                                    goto handled;
                                }
                            }
                            foreach(str; prog.strings){
                                if(str.ptr == cast(const char*)value)
                                    goto case STRING;
                            }
                        }
                        // default:
                            sb.hex("0x", value);
                            handled:
                            continue;
                        case STRING:{
                            foreach(str; prog.strings){
                                if(str.ptr == cast(const char*)value){
                                    sb.write(Q(E(str[]), '"'));
                                    break;
                                }
                            }
                            continue;
                        }
                        case REGISTER:{
                            bool written = false;
                            foreach(ri; registerinfos){
                                if(ri.register == value){
                                    sb.write(ri.name);
                                    written = true;
                                    break;
                                }
                            }
                            if(!written){
                                sb.write("REGISTER");
                                sb.hex("0x", value);
                            }
                            continue;
                        }
                        case CMPMODE:{
                            if(value < CmpModes.length){
                                sb.write(CmpModes[value].name);
                            }
                            else {
                                sb.write("CMPMODE");
                                sb.hex("0x", value);
                            }
                            continue;
                        }
                    }
                }
            }
            return 1 + cast(int)ii.args.length;
        }
        else {
            sb.write("UNK");
            sb.hex("0x", inst);
            return 1;
        }
    }
}

struct LinkContext {
    VAllocator* allocator;
    VAllocator* temp_allocator;
    FunctionTable* builtins;
    UnlinkedProgram* unlinked;
    LinkedProgram prog;
    Box!(char[], Mallocator) errmess;

    void
    err_print(A...)(Token tok, A args){
        StringBuilder!Mallocator sb;
        sb.FORMAT(tok.line, ':', tok.column, ": LinkError: ");
        foreach(a; args)
            sb.write(a);
        errmess = sb.take;
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
        foreach(ref func; unlinked.functions[]){
            auto size = calculate_function_size(&func) + 4;
            total_size += size;
        }
        prog.bytecode.resize(total_size);
        size_t code_off = 0;
        prog.function_store.resize(unlinked.functions.count);
        foreach(i, ref func; unlinked.functions[]){
            auto size = calculate_function_size(&func);
            code_off += 2;
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
            code_off += 2 + size;
        }
        return AsmError.NO_ERROR;
    }

    //
    // Conver the un-escaped raw text into a ZString.
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
        ZString result = sb.detach;
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
        ZString result = sb.detach;
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
                            *(ip++) = cast(uintptr_t)var;
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

size_t
instruction_size(Instruction instruction){
    auto info = &INSTRUCTION_INFOS[instruction];
    return info.args.length+1;
}

AsmError
link_asm(VAllocator* allocator, VAllocator* temp_allocator, FunctionTable* builtins, UnlinkedProgram* unlinked, LinkedProgram* prog){
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
