import core.stdc.stdio: fprintf, stderr;
import core.stdc.string: memcpy;

import dlib.allocator;
import dlib.get_input;
import dlib.box;
import dlib.str_util: split, stripped;
import dlib.stringbuilder;

import dvm_defs;
import dvm_linked;
import dvm_instructions;
import dvm_regs;
import dvm_args;
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
    LinkedModule* current_program;
    uintptr_t[256] call_stack;
    uintptr_t call_depth;
    bool halted;
    bool paused;
    bool badend;
    bool debugging;
    LineHistory!VAllocator debugger_history;
    bool[RegisterNames.max+1] watches;
    int
    run(RunFlags flags = RunFlags.NONE)(LinkedModule* prog, size_t stack_size, const char* debug_history_file = null){
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
                case 4:
                    registers[ROUT1] = func.native_function_raaaa(registers[RARG1], registers[RARG2], registers[RARG3], registers[RARG4]);
                    return 0;
                default:
                    return 1;
            }
        }
        else {
            switch(func.n_args){
                case 0:
                    func.native_function_();
                    return 0;
                case 1:
                    func.native_function_a(registers[RARG1]);
                    return 0;
                case 2:
                    func.native_function_aa(registers[RARG1], registers[RARG2]);
                    return 0;
                case 3:
                    func.native_function_aaa(registers[RARG1], registers[RARG2], registers[RARG3]);
                    return 0;
                case 4:
                    func.native_function_aaaa(registers[RARG1], registers[RARG2], registers[RARG3], registers[RARG4]);
                    return 0;
                case 5:
                    func.native_function_aaaaa(registers[RARG1], registers[RARG2], registers[RARG3], registers[RARG4], registers[RARG5]);
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
            auto ri = get_register_info(i);
            if(!Fuzzing)fprintf(stderr, "[%s] = %#zx\n", ri.NAME.ptr, reg);
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
                foreach(i, w; watches){
                    if(!w) continue;
                    auto ri = get_register_info(i);
                    fprintf(stderr, "%s = %#zx\n", ri.name.ptr, registers[i]);
                }
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
                    auto buf = buff[0 ..len].stripped;
                    if(!buf.length){
                        return BEGIN_OK;
                    }
                    auto s = buf.split(' ');
                    auto cmd = s.head;
                    auto arg = s.tail?s.tail.stripped:null;
                    if(arg.length){
                        switch(cmd){
                            case "w": case "watch":{
                                auto ri = get_register_info(arg);
                                if(!ri) continue;
                                watches[ri.register] = true;
                            }continue;
                            case "uw": case "unwatch":{
                                auto ri = get_register_info(arg);
                                if(!ri) continue;
                                watches[ri.register] = false;
                            }continue;
                            default:
                                fprintf(stderr, "Unknown command: '%.*s'\n", cast(int)len, buff.ptr);
                                continue;
                        }
                    }
                    switch(buf){
                        case "next": case "n":
                            debugger_history.add_line(buf);
                            return BEGIN_OK;
                        case "c": case "continue":
                            debugger_history.add_line(buf);
                            return BEGIN_CONTINUE;
                        case "q": case "quit":
                            debugger_history.add_line(buf);
                            halted = true;
                            return BEGIN_END;
                        case "l": case "list":
                            debugger_history.add_line(buf);
                            print_current_function((cast(uintptr_t*)registers[RIP])-1);
                            continue;
                        case "d": case "dump":
                            debugger_history.add_line(buf);
                            dump;
                            continue;
                        case "bt": case "backtrace": case "where":
                            debugger_history.add_line(buf);
                            backtrace;
                            continue;
                        case "h": case "help":
                            fprintf(stderr, "%s", ("Commands:\n"
                            ~"  next, n           execute next instruction\n"
                            ~"  continue, c       execute until next breakpoint\n"
                            ~"  quit, q           halt execution\n"
                            ~"  list, l           print disassembly of entire function\n"
                            ~"  dump, d           print contents of registers\n"
                            ~"  backtrace, bt     print stacktrace\n"
                            ~"  where             alias of backtrace\n"
                            ~"  help, h           print out this info\n"
                            ~"  watch, w reg      add a watch for a register\n"
                            ~"  unwatch, uw reg   remove a watch for a register\n"
                            ).ptr);
                            continue;

                        default:
                            fprintf(stderr, "Unknown command: '%.*s'\n", cast(int)buf.length, buf.ptr);
                            continue;
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
            switch(inst){
                case HALT:
                    if(auto b = begin(HALT)) return b;
                    halted = true;
                    return 0;
                default:
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
                    *dst = -*rhs;
                }break;
                case BINNEG:{
                    if(auto b = begin(BINNEG)) return b;
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
disassemble_one_instruction(SB)(LinkedModule* prog, SB* sb, uintptr_t* ip){
    uintptr_t inst = *ip;
    if(inst > Instruction.max){
        sb.write("UNK");
        sb.hex("0x", inst);
        return 1;
    }
    with(Instruction) with(ArgumentKind){
        auto ii = &INSTRUCTION_INFOS[inst];
        sb.write(ii.asm_name);
        if(!ii.args.length) return 1;
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
                    if(auto ri = get_register_info(value)){
                        sb.write(ri.name);
                        written = true;
                        break;
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
        return 1 + cast(int)ii.args.length;
    }
}
