/*
 * Copyright © 2021-2023, David Priver
 */
module dvm.dvm_machine;
import core.stdc.stdio: fprintf, stderr;
import core.stdc.string: memcpy, memset;

import dlib.allocator;
import dlib.get_input;
import dlib.box;
import dlib.table;
import dlib.str_util: split, stripped, Split;
import dlib.stringbuilder;

import dvm.dvm_defs;
import dvm.dvm_linked;
import dvm.dvm_instructions;
import dvm.dvm_regs;
import dvm.dvm_args;
import dvm.dvm_linker : link_single_instruction, SingleInstructionLinkResult;
import dasm.dasm_parser : parse_single_instruction, SingleInstructionResult;
enum RunFlags: uint {
    NONE = 0,
    DEBUG = 1 << 0,
    DISASSEMBLE_EACH = 1 << 1,
    RUN_ONE = 1 << 2,
}

struct Machine {
    enum CmpFlags: uintptr_t {
        ZERO = 1 << 0,
        CARRY = 1 << 1,
    }
    Allocator allocator;
    Box!(void[]) stack;
    uintptr_t[RegisterNames.max+1] registers;
    LinkedModule* current_program;
    uintptr_t[256] call_stack;
    uintptr_t call_depth;
    bool halted;
    bool paused;
    bool badend;
    bool debugging;
    LineHistory debugger_history;
    bool[RegisterNames.max+1] watches;
    // Breakpoints: address -> original instruction
    Table!(uintptr_t, Instruction) breakpoints;
    // Memory watchpoints: address -> last known value
    Table!(uintptr_t, uintptr_t) mem_watchpoints;

    int
    run(RunFlags flags = RunFlags.NONE)(LinkedModule* prog, size_t stack_size, const char* debug_history_file = null){
        if(!prog.start || !prog.start.instructions_)
            return 1;
        halted = false;
        paused = false;
        badend = false;
        debugging = false;
        stack.allocator = allocator;
        stack.resize(stack_size);
        current_program = prog;
        registers[RegisterNames.RIP] = cast(uintptr_t)current_program.start.instructions_;
        registers[RegisterNames.RSP] = cast(uintptr_t)stack.data.ptr;
        registers[RegisterNames.RBP] = cast(uintptr_t)stack.data.ptr;
        call_depth = 0;
        debugger_history.allocator = allocator;
        breakpoints.data.allocator = allocator;
        mem_watchpoints.data.allocator = allocator;
        if(debug_history_file) debugger_history.load_history(debug_history_file);
        static if(flags & RunFlags.DEBUG){
            int result;
            for(;;){
                again:;
                result = run_interpreter!flags;
                while(result == BEGIN_CONTINUE_DEBUGGING){
                    result = run_interpreter!(RunFlags.RUN_ONE);
                    if(result == BEGIN_BAD || result == BEGIN_OK)
                        break;
                    result = run_interpreter!(RunFlags.RUN_ONE);
                    if(result == BEGIN_BAD || result == BEGIN_OK)
                        break;
                    goto again;
                }
                if(result == BEGIN_CONTINUE){
                    registers[RegisterNames.RIP] -= uintptr_t.sizeof;
                    result = run_interpreter!((flags & RunFlags.DISASSEMBLE_EACH)?RunFlags.DISASSEMBLE_EACH:RunFlags.NONE);
                }
                break;
            }
        }
        else {
            int result = run_interpreter!flags;
            if(result == BEGIN_CONTINUE)
                result = BEGIN_OK;
        }
        if(debug_history_file) debugger_history.dump(debug_history_file);
        debugger_history.cleanup;
        return result;
    }

    int
    do_call(Function* func, size_t n_args, uint call_floats = 0){with(RegisterNames){
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
            case NATIVE_VARARGS:{
                return call_native_varargs(func, n_args, call_floats);
            }
        }
    }}

    int
    call_native_varargs(Function* func, size_t n_total, uint call_floats = 0){with(RegisterNames){
        uintptr_t[16] args = void;
        size_t n_fixed = func.n_args;
        size_t n_variadic = n_total > n_fixed ? n_total - n_fixed : 0;

        // Fixed args come from RARG registers
        size_t n_fixed_from_regs = n_fixed < N_REG_ARGS ? n_fixed : N_REG_ARGS;
        for (size_t i = 0; i < n_fixed_from_regs; i++) {
            args[i] = registers[RARG1 + i];
        }

        // Variadic args come from DVM stack (always on stack per our convention)
        if (n_variadic > 0) {
            uintptr_t* stack_args = cast(uintptr_t*)(registers[RSP] - n_variadic * uintptr_t.sizeof);
            for (size_t i = 0; i < n_variadic; i++) {
                args[n_fixed + i] = stack_args[i];
            }
            // Caller cleanup - don't pop here
        }

        // Combine function-declared arg types with call-site float mask
        // call_floats provides per-call info about which variadic args are floats
        uint arg_types = func.arg_types | call_floats;

        // Use float-aware trampoline if any args or returns are floats
        if (arg_types != 0 || func.ret_types != 0) {
            import dvm.varargs_trampoline_float: call_varargs_float;
            registers[ROUT1] = call_varargs_float(
                cast(void*)func.native_function_,
                args.ptr,
                n_fixed,
                n_total,
                arg_types,
                func.ret_types
            );
        } else {
            import dvm.varargs_trampoline: call_varargs;
            registers[ROUT1] = call_varargs(
                cast(void*)func.native_function_,
                args.ptr,
                n_fixed,
                n_total
            );
        }
        return 0;
    }}

    int
    call_native(Function* func){with(RegisterNames){
        // Gather arguments from DVM registers
        uintptr_t[16] args = void;
        size_t n_args = func.n_args;
        for (size_t i = 0; i < n_args && i < N_REG_ARGS; i++) {
            args[i] = registers[RARG1 + i];
        }

        uintptr_t result;
        uintptr_t result2;

        // Use float-aware trampoline if any args or returns are floats, or if we have struct args
        if (func.arg_types != 0 || func.ret_types != 0 || func.struct_arg_sizes_ !is null) {
            import dvm.native_trampoline_float: call_native_float;
            // Check if there are 2 return values (for struct returns in XMM0+XMM1)
            uintptr_t* ret2_ptr = (func.n_ret > 1) ? &result2 : null;
            result = call_native_float(
                cast(void*)func.native_function_,
                args.ptr,
                n_args,
                func.arg_types,
                func.ret_types,
                ret2_ptr,
                func.struct_arg_sizes_
            );
        } else {
            import dvm.native_trampoline: call_native_trampoline;
            result = call_native_trampoline(
                cast(void*)func.native_function_,
                args.ptr,
                n_args
            );
        }

        // Store result if function returns a value
        if (func.n_ret) {
            registers[ROUT1] = result;
            if (func.n_ret > 1) {
                registers[ROUT2] = result2;
            }
        }
        return 0;
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

    // Print stack/local values. Stack grows up, so rbp < rsp.
    // from_rsp: true = from rsp down (stack top), false = from rbp up (locals)
    int print_stack_range(int start, int count, bool from_rsp){
        uintptr_t* rsp = cast(uintptr_t*)registers[RegisterNames.RSP];
        uintptr_t* rbp = cast(uintptr_t*)registers[RegisterNames.RBP];
        if(rbp > rsp){
            fprintf(stderr, "Invalid stack: rbp > rsp\n");
            return 0;
        }
        int max_slots = cast(int)(rsp - rbp);
        if(max_slots == 0){
            fprintf(stderr, "Stack is empty\n");
            return 0;
        }
        if(start < 0 || start >= max_slots){
            fprintf(stderr, "Slot %d out of range (0-%d)\n", start, max_slots - 1);
            return 0;
        }
        if(count < 0) count = max_slots - start;
        if(start + count > max_slots) count = max_slots - start;

        StringBuilder sb = {allocator: allocator};
        scope(exit) sb.cleanup;
        for(int i = start; i < start + count; i++){
            uintptr_t val = from_rsp ? rsp[-1 - i] : rbp[i];
            if(from_rsp)
                sb.FORMAT("[rsp-", (i + 1) * cast(int)uintptr_t.sizeof, "] = ");
            else
                sb.FORMAT("[rbp+", i * cast(int)uintptr_t.sizeof, "] = ");
            sb.hex("0x", val);
            resolve_value(&sb, val);
            sb.write('\n');
        }
        str text = sb.borrow;
        fprintf(stderr, "%.*s", cast(int)text.length, text.ptr);
        return count;
    }

    // Print a value with optional symbolic resolution
    void print_resolved(str name, uintptr_t value, char prefix = 0, bool do_resolve = true){
        StringBuilder sb = {allocator: allocator};
        scope(exit) sb.cleanup;
        if(prefix) sb.write(prefix);
        sb.FORMAT(name, " -> ");
        sb.hex("0x", value);
        if(do_resolve) resolve_value(&sb, value);
        sb.write('\n');
        str text = sb.borrow;
        fprintf(stderr, "%.*s", cast(int)text.length, text.ptr);
    }

    // Try to resolve a value to a symbolic name (function, string, etc.)
    void resolve_value(SB)(SB* sb, uintptr_t value){
        // Check functions
        Function* func = cast(Function*)value;
        foreach(v; current_program.functions.values){
            if(v.func == func){
                sb.write(" (function ");
                sb.write(v.name);
                sb.write(")");
                return;
            }
        }
        foreach(imp; current_program.imports.items){
            foreach(v; imp.value.functions.values){
                if(v.func == func){
                    sb.write(" (function ");
                    sb.write(imp.key);
                    sb.write('.');
                    sb.write(v.name);
                    sb.write(")");
                    return;
                }
            }
        }
        // Check strings
        foreach(str; current_program.strings){
            if(str.ptr == cast(const char*)value){
                sb.write(" (\"");
                sb.write(E(str[]));
                sb.write("\")");
                return;
            }
        }
        foreach(imp; current_program.imports.items){
            foreach(str; imp.value.strings){
                if(str.ptr == cast(const char*)value){
                    sb.write(" (\"");
                    sb.write(E(str[]));
                    sb.write("\")");
                    return;
                }
            }
        }
    }

    void
    dump(int off=0){
        StringBuilder sb = {allocator:allocator};
        scope(exit) sb.cleanup;
        foreach(i, reg; registers){
            immutable RegisterInfo* ri = get_register_info(i);
            sb.FORMAT("[", ri.NAME, "] = ");
            sb.hex("0x", reg);
            resolve_value(&sb, reg);
            sb.write('\n');
        }
        str text = sb.borrow;
        if(!Fuzzing)fprintf(stderr, "%.*s", cast(int)text.length, text.ptr);
        sb.reset;
        for(size_t i = 0; i < 4; i++){
            if(!Fuzzing)fprintf(stderr, "ip[%zu] = %#zx\n", i, (cast(uintptr_t*)registers[RegisterNames.RIP])[i]);
        }
        uintptr_t* ip = cast(uintptr_t*)registers[RegisterNames.RIP];
        ip += off;
        for(int i = 0; i < 4; i++){
            ip += disassemble_one_instruction(current_program, &sb, ip, get_real_instruction(ip));
            sb.write("\n     ");
        }
        text = sb.borrow;
        if(!Fuzzing)fprintf(stderr, "Dis: %.*s\n", cast(int)text.length, text.ptr);
    }

    void
    backtrace(){
        FunctionInfo current = current_program.addr_to_function(cast(uintptr_t*)registers[RegisterNames.RIP]);
        if(!Fuzzing)fprintf(stderr, "[%zu] %.*s\n", call_depth, cast(int)current.name.length, current.name.ptr);
        for(ptrdiff_t i = call_depth-1; i >= 0; i--){
            FunctionInfo fi = current_program.addr_to_function(cast(uintptr_t*)call_stack[i]);
            if(!Fuzzing)fprintf(stderr, "[%zu] %.*s\n", i, cast(int)fi.name.length, fi.name.ptr);
        }
    }

    void
    print_current_function(uintptr_t* ip){
        FunctionInfo func = current_program.addr_to_function(ip);
        size_t i = 0;
        uintptr_t[] insts = func.func.instructions;
        StringBuilder sb = {allocator:MALLOCATOR};
        scope(exit) sb.cleanup;
        sb.FORMAT("function ", func.name, "\n");
        while(insts.length){
            sb.FORMAT(' ', insts.ptr==ip?"-> ":"   ");
            Instruction real_inst = get_real_instruction(insts.ptr);
            int num = disassemble_one_instruction(current_program, &sb, insts.ptr, real_inst);
            sb.write('\n');
            insts = insts[num.. $];
            i++;
        }
        sb.write("end");
        str text = sb.borrow;
        if(!Fuzzing)fprintf(stderr, "\n%.*s\n", cast(int)text.length, text.ptr);
    }
    enum {
        BEGIN_OK = 0,
        BEGIN_BAD = 1,
        BEGIN_CONTINUE = 2,
        BEGIN_END = 3,
        BEGIN_CONTINUE_DEBUGGING = 4,
    }

    // Get original instruction at address, checking breakpoints if it's BREAK
    Instruction get_real_instruction(uintptr_t* addr){
        Instruction inst = cast(Instruction)*addr;
        if(inst == Instruction.BREAK){
            if(auto orig = cast(uintptr_t)addr in breakpoints){
                return *orig;
            }
        }
        return inst;
    }

    void
    print_context(uintptr_t* ip){
        FunctionInfo func = current_program.addr_to_function(ip);
        if(!func.func || func.func.type != func.func.type.INTERPRETED){
            // Address not in any known function (e.g., debugger scratch buffer)
            return;
        }
        ptrdiff_t current = 0;
        ptrdiff_t i = 0;
        uintptr_t[] insts = func.func.instructions;
        while(insts.length){
            Instruction real_inst = get_real_instruction(insts.ptr);
            size_t size = instruction_size(real_inst);
            if(insts.ptr == ip)
                current = i;
            insts = insts[size .. $];
            i++;
        }
        insts = func.func.instructions;
        ptrdiff_t n_lines = i;
        i = 0;
        StringBuilder sb = {allocator:MALLOCATOR};
        scope(exit) sb.cleanup;
        sb.FORMAT("function ", func.name, "\n");
        if(current > 6){
            sb.write("    ...\n");
        }
        while(insts.length){
            Instruction real_inst = get_real_instruction(insts.ptr);
            if(i >= current -5 && i <= current+5){
                sb.FORMAT(' ', insts.ptr==ip?"-> ":"   ");
                disassemble_one_instruction(current_program, &sb, insts.ptr, real_inst);
                sb.write('\n');
            }
            size_t size = instruction_size(real_inst);
            insts = insts[size .. $];
            i++;
        }
        if(n_lines <= current+5){
        }
        else
            sb.write("    ...\n");
        sb.write("end");
        str text = sb.borrow;
        if(!Fuzzing)fprintf(stderr, "\n%.*s\n", cast(int)text.length, text.ptr);
    }

    int
    run_interpreter(RunFlags flags = RunFlags.NONE)(){ with(RegisterNames) with(Instruction){
        int
        debugger(){
            if(debugging) return 0;
            debugging = true;
            int result = run_interpreter!(flags|RunFlags.DEBUG);
            debugging = false;
            if(result == 2){
                registers[RegisterNames.RIP] -= uintptr_t.sizeof;
                return 0;
            }
            return result;
        }
        static assert(uintptr_t.sizeof == 8);
        alias float_t = float;
        alias double_t = double;

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
        intptr_t
        get_signed(){
            intptr_t result = *cast(intptr_t*)registers[RIP];
            registers[RIP]+= uintptr_t.sizeof;
            return result;
        }
        float_t
        get_float(){
            float_t result = *cast(float_t*)registers[RIP];
            registers[RIP]+= uintptr_t.sizeof;
            return result;
        }
        double_t
        get_double(){
            double_t result = *cast(double_t*)registers[RIP];
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
        double_t
        double_read_reg(){
            return *cast(double_t*)get_reg();
        }
        int
        check_cmp(uintptr_t cmp){
            switch(cmp) with(CmpFlags) with(CmpMode){
                case EQ: return !!(registers[RFLAGS] & ZERO);
                case NE: return !(registers[RFLAGS] & ZERO);
                case LT: return (registers[RFLAGS] & (CARRY|ZERO)) == CARRY;
                case GT: return (registers[RFLAGS] & (CARRY|ZERO)) == 0;
                case LE: return !!(registers[RFLAGS] & (CARRY|ZERO));
                case GE: return (registers[RFLAGS] & CARRY) != CARRY;
                case TRUE: return 1;
                case FALSE: return 0;
                default: return -1;
            }
        }

        // Helper to try parsing and executing a dasm instruction
        // Returns: 0 = success (set RIP), 1 = parse/link error (printed), -1 = not valid dasm
        int try_execute_dasm(str buf){
            auto parse_result = parse_single_instruction(allocator, buf);
            if(!parse_result.success){
                if(parse_result.errmess.length)
                    fprintf(stderr, "%.*s\n", cast(int)parse_result.errmess.length, parse_result.errmess.ptr);
                return -1;
            }

            // Link the instruction with return address = current RIP
            auto link_result = link_single_instruction(allocator, parse_result.inst, current_program, registers[RIP]-uintptr_t.sizeof);
            if(!link_result.success){
                fprintf(stderr, "Link error: %.*s\n", cast(int)link_result.errmess.length, link_result.errmess.ptr);
                return 1;
            }

            // Allocate scratch buffer (zero-init so out-of-bounds hits ABORT)
            uintptr_t[] scratch_buf = allocator.zalloc!(uintptr_t)(SingleInstructionLinkResult.MAX_SIZE);
            memcpy(scratch_buf.ptr, link_result.bytecode.ptr, link_result.length * uintptr_t.sizeof);

            registers[RIP] = cast(uintptr_t)(scratch_buf.ptr);
            debugger_history.add_line(buf);
            return 0;
        }

        // pragma(inline, true)
        int
        begin(Instruction inst){
            static if(flags & RunFlags.DISASSEMBLE_EACH){{
                uintptr_t* ip = cast(uintptr_t*)registers[RIP];
                ip--;
                StringBuilder sb = {allocator:MALLOCATOR};
                scope(exit) sb.cleanup;
                disassemble_one_instruction(current_program, &sb, ip, get_real_instruction(ip));
                str text = sb.borrow;
                if(!Fuzzing)fprintf(stderr, "%.*s\n", cast(int)text.length, text.ptr);
            }}
            static if(flags & RunFlags.DEBUG){
                print_context((cast(uintptr_t*)registers[RIP])-1);
                foreach(i, w; watches){
                    if(!w) continue;
                    immutable(RegisterInfo)* ri = get_register_info(i);
                    fprintf(stderr, "%s = %#zx\n", ri.name.ptr, registers[i]);
                }
                // Check memory watchpoints
                foreach(ref it; mem_watchpoints.items){
                    uintptr_t current_val = *cast(uintptr_t*)it.key;
                    if(current_val != it.value){
                        fprintf(stderr, "Watchpoint hit: 0x%zx changed from 0x%zx to 0x%zx\n",
                                it.key, it.value, current_val);
                        it.value = current_val;
                    }
                }
                char[1024] buff = void;
                for(;;){
                    ptrdiff_t len = get_input_line(&debugger_history, "> ", buff[]);
                    if(len < 0){
                        halted = true;
                        return BEGIN_BAD;
                    }
                    if(!len){
                        return BEGIN_OK;
                    }
                    str buf = buff[0 ..len].stripped;
                    if(!buf.length){
                        return BEGIN_OK;
                    }
                    if(immutable(RegisterInfo)* ri = get_register_info(buf)){
                        print_resolved(ri.name, registers[ri.register]);
                        debugger_history.add_line(buf);
                        continue;
                    }
                    if(buf[0] == '*'){
                        if(immutable(RegisterInfo)* ri = get_register_info(buf[1..$])){
                            print_resolved(ri.name, *cast(uintptr_t*)registers[ri.register], '*');
                            debugger_history.add_line(buf);
                            continue;
                        }
                        else {
                            fprintf(stderr, "Unknown register name\n");
                            continue;
                        }
                    }
                    Split s = buf.split(' ');
                    str cmd = s.head;
                    str arg = s.tail?s.tail.stripped:null;
                    size_t mread = 0;
                    if(arg.length){
                        switch(cmd){
                            case "x": case "examine":{
                                // Memory examine: x <addr|reg> [count]
                                import dlib.parse_numbers: parse_unsigned_human;
                                auto parts = arg.split(' ');
                                uintptr_t addr;
                                // Try as register first
                                if(immutable(RegisterInfo)* ri = get_register_info(parts.head)){
                                    addr = registers[ri.register];
                                } else {
                                    auto r = parse_unsigned_human(parts.head);
                                    if(r.errored){
                                        fprintf(stderr, "Invalid address or register: %.*s\n", cast(int)parts.head.length, parts.head.ptr);
                                        continue;
                                    }
                                    addr = cast(uintptr_t)r.value;
                                }
                                int count = 8;
                                if(parts.tail.length){
                                    auto r = parse_unsigned_human(parts.tail.stripped);
                                    if(!r.errored) count = cast(int)r.value;
                                }
                                // Display memory
                                uintptr_t* ptr = cast(uintptr_t*)addr;
                                for(int i = 0; i < count; i++){
                                    if(i % 4 == 0){
                                        if(i > 0) fprintf(stderr, "\n");
                                        fprintf(stderr, "0x%zx: ", addr + i * uintptr_t.sizeof);
                                    }
                                    print_resolved(null, ptr[i], 0, false);
                                    fprintf(stderr, "  ");
                                }
                                fprintf(stderr, "\n");
                                debugger_history.add_line(buf);
                            }continue;
                            case "w": case "watch":{
                                import dlib.parse_numbers: parse_unsigned_human;
                                // Try as register first
                                if(immutable(RegisterInfo)* ri = get_register_info(arg)){
                                    watches[ri.register] = true;
                                    fprintf(stderr, "Watching register %.*s\n", cast(int)ri.name.length, ri.name.ptr);
                                } else {
                                    // Try as memory address
                                    auto r = parse_unsigned_human(arg);
                                    if(r.errored){
                                        fprintf(stderr, "Invalid register or address: %.*s\n", cast(int)arg.length, arg.ptr);
                                        continue;
                                    }
                                    uintptr_t addr = cast(uintptr_t)r.value;
                                    uintptr_t current_val = *cast(uintptr_t*)addr;
                                    mem_watchpoints[addr] = current_val;
                                    fprintf(stderr, "Watching memory at 0x%zx (current value: 0x%zx)\n", addr, current_val);
                                }
                                debugger_history.add_line(buf);
                            }continue;
                            case "b": case "break": case "breakpoint":{
                                // Set breakpoint at function or address
                                import dlib.parse_numbers: parse_unsigned_human;
                                uintptr_t addr;
                                bool addr_valid = false;

                                // Try as function name first
                                if(auto fi = arg in current_program.functions){
                                    if(fi.func.type != FunctionType.INTERPRETED){
                                        fprintf(stderr, "Cannot set breakpoint on native function: %.*s\n", cast(int)arg.length, arg.ptr);
                                        continue;
                                    }
                                    addr = cast(uintptr_t)fi.func.instructions_;
                                    addr_valid = true;
                                } else {
                                    // Try as address
                                    auto r = parse_unsigned_human(arg);
                                    if(r.errored){
                                        fprintf(stderr, "Unknown function: %.*s\n", cast(int)arg.length, arg.ptr);
                                        continue;
                                    }
                                    addr = cast(uintptr_t)r.value;
                                    // Find containing function and verify it's an instruction boundary
                                    FunctionInfo fi = current_program.addr_to_function(cast(uintptr_t*)addr);
                                    if(!fi.func || fi.func.type != FunctionType.INTERPRETED){
                                        fprintf(stderr, "Address 0x%zx not in any interpreted function\n", addr);
                                        continue;
                                    }
                                    // Walk instructions to verify boundary
                                    uintptr_t* ip = fi.func.instructions_;
                                    uintptr_t* end = ip + fi.func.length;
                                    while(ip < end){
                                        if(cast(uintptr_t)ip == addr){
                                            addr_valid = true;
                                            break;
                                        }
                                        ip += instruction_size(get_real_instruction(ip));
                                    }
                                    if(!addr_valid){
                                        fprintf(stderr, "Address 0x%zx is not at an instruction boundary\n", addr);
                                        continue;
                                    }
                                }

                                if(addr_valid){
                                    if(addr in breakpoints){
                                        fprintf(stderr, "Breakpoint already set at 0x%zx\n", addr);
                                    } else {
                                        Instruction orig = *cast(Instruction*)addr;
                                        breakpoints[addr] = orig;
                                        *cast(Instruction*)addr = Instruction.BREAK;
                                        fprintf(stderr, "Breakpoint set at 0x%zx\n", addr);
                                    }
                                }
                                debugger_history.add_line(buf);
                            }continue;
                            case "delete": case "del":{
                                // Delete breakpoint by address or function name
                                import dlib.parse_numbers: parse_uint64;
                                uintptr_t addr;
                                if(auto fi = arg in current_program.functions){
                                    addr = cast(uintptr_t)fi.func.instructions_;
                                } else {
                                    auto r = parse_uint64(arg);
                                    if(r.errored){
                                        fprintf(stderr, "Unknown function or invalid address: %.*s\n", cast(int)arg.length, arg.ptr);
                                        continue;
                                    }
                                    addr = cast(uintptr_t)r.value;
                                }
                                if(auto orig = addr in breakpoints){
                                    *cast(Instruction*)addr = *orig;
                                    // Can't actually remove from table, but instruction is restored
                                    fprintf(stderr, "Breakpoint disabled at 0x%zx\n", addr);
                                } else {
                                    fprintf(stderr, "No breakpoint at 0x%zx\n", addr);
                                }
                                debugger_history.add_line(buf);
                            }continue;
                            case "uw": case "unwatch":{
                                immutable(RegisterInfo)* ri = get_register_info(arg);
                                if(!ri) continue;
                                watches[ri.register] = false;
                                debugger_history.add_line(buf);
                            }continue;
                            case "m1": case "mread1": mread = 1; break;
                            case "m2": case "mread2": mread = 2; break;
                            case "m4": case "mread4": mread = 4; break;
                            case "m": case "mread":   mread = 8; break;
                            case "s": case "stack":
                            case "v": case "locals":{
                                import dlib.parse_numbers: parse_uint64;
                                bool from_rsp = (cmd == "s" || cmd == "stack");
                                if(arg == "all" || arg == "a"){
                                    print_stack_range(0, -1, from_rsp);
                                } else {
                                    auto parts = arg.split(' ');
                                    auto r1 = parse_uint64(parts.head);
                                    if(r1.errored){
                                        fprintf(stderr, "Invalid slot: %.*s\n", cast(int)parts.head.length, parts.head.ptr);
                                        continue;
                                    }
                                    int slot = cast(int)r1.value;
                                    if(parts.tail.length){
                                        auto r2 = parse_uint64(parts.tail.stripped);
                                        if(r2.errored){
                                            fprintf(stderr, "Invalid end slot: %.*s\n", cast(int)parts.tail.length, parts.tail.ptr);
                                            continue;
                                        }
                                        int end = cast(int)r2.value;
                                        print_stack_range(slot, end - slot + 1, from_rsp);
                                    } else {
                                        print_stack_range(slot, 1, from_rsp);
                                    }
                                }
                                debugger_history.add_line(buf);
                            }continue;
                            default:
                                // Commands with args that aren't recognized - try as dasm
                                int dasm_result = try_execute_dasm(buf);
                                if(dasm_result == 0) return BEGIN_CONTINUE_DEBUGGING;  // Execute
                                if(dasm_result == 1) continue;  // Error, stay in debugger
                                // dasm_result == -1 means wasn't valid dasm either
                                fprintf(stderr, "Unknown command: '%.*s'\n", cast(int)buf.length, buf.ptr);
                                continue;
                        }
                        if(mread){
                            immutable(RegisterInfo)* ri = get_register_info(arg);
                            if(!ri) continue;
                            uintptr_t val = 0;
                            uintptr_t*r = cast(uintptr_t*)registers[ri.register];
                            memcpy(&val, r, mread);
                            print_resolved(ri.name, val, 0, mread == 8);
                            debugger_history.add_line(buf);
                            continue;
                        }
                    }
                    switch(buf){
                        case "next": case "n":
                            // debugger_history.add_line(buf);
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
                            dump(-1);
                            continue;
                        case "bt": case "backtrace": case "where":
                            debugger_history.add_line(buf);
                            backtrace;
                            continue;
                        case "b": case "break": case "breakpoint":
                            // List breakpoints
                            if(breakpoints.count == 0){
                                fprintf(stderr, "No breakpoints set.\n");
                            } else {
                                fprintf(stderr, "Breakpoints:\n");
                                foreach(ref it; breakpoints.items){
                                    FunctionInfo fi = current_program.addr_to_function(cast(uintptr_t*)it.key);
                                    if(fi.name.length){
                                        fprintf(stderr, "  0x%zx (%.*s)\n", it.key, cast(int)fi.name.length, fi.name.ptr);
                                    } else {
                                        fprintf(stderr, "  0x%zx\n", it.key);
                                    }
                                }
                            }
                            continue;
                        case "s": case "stack":
                            debugger_history.add_line(buf);
                            print_stack_range(0, 10, true);
                            continue;
                        case "sa":
                            debugger_history.add_line(buf);
                            print_stack_range(0, -1, true);
                            continue;
                        case "v": case "locals":
                            debugger_history.add_line(buf);
                            print_stack_range(0, 10, false);
                            continue;
                        case "va":
                            debugger_history.add_line(buf);
                            print_stack_range(0, -1, false);
                            continue;
                        case "h": case "help":
                            fprintf(stderr, "%s", ("Commands:\n"
                            ~"  next, n           execute next instruction\n"
                            ~"  continue, c       execute until next breakpoint\n"
                            ~"  quit, q           halt execution\n"
                            ~"  list, l           print disassembly of entire function\n"
                            ~"  dump, d           print contents of registers\n"
                            ~"  break, b <func>   set breakpoint at function\n"
                            ~"  break, b <addr>   set breakpoint at address\n"
                            ~"  break, b          list all breakpoints\n"
                            ~"  delete <func|addr> remove breakpoint\n"
                            ~"  stack, s          print top 10 stack values (from rsp)\n"
                            ~"  stack all, sa     print entire stack\n"
                            ~"  stack N           print stack slot N\n"
                            ~"  stack N M         print stack slots N to M\n"
                            ~"  locals, v         print first 10 locals (from rbp)\n"
                            ~"  locals all, va    print all locals\n"
                            ~"  locals N          print local slot N\n"
                            ~"  locals N M        print local slots N to M\n"
                            ~"  backtrace, bt     print stacktrace\n"
                            ~"  where             alias of backtrace\n"
                            ~"  help, h           print out this info\n"
                            ~"  x <addr> [n]      examine n words of memory at addr\n"
                            ~"  watch, w <reg>    add a watch for a register\n"
                            ~"  watch, w <addr>   add a watchpoint for memory address\n"
                            ~"  unwatch, uw reg   remove a watch for a register\n"
                            ~"  mread, m reg      dereference the uintptr_t* stored in reg\n"
                            ).ptr);
                            continue;

                        default:
                            // Try as dasm instruction
                            int dasm_result = try_execute_dasm(buf);
                            if(dasm_result == 0) return BEGIN_CONTINUE_DEBUGGING;  // Execute
                            if(dasm_result == 1) continue;  // Error, stay in debugger
                            fprintf(stderr, "Unknown command: '%.*s'\n", cast(int)buf.length, buf.ptr);
                            continue;
                    }
                }
            }
            else
                return BEGIN_OK;
        }
        for(;;){
            Instruction inst = *cast(Instruction*)registers[RIP];
            registers[RIP] += uintptr_t.sizeof;
            /*final*/ switch(inst){
                case HALT:
                    if(int b = begin(HALT)) return b;
                    halted = true;
                    return 0;
                case BREAK:{
                    // Hit a breakpoint - restore original instruction and rewind
                    uintptr_t addr = registers[RIP] - uintptr_t.sizeof;
                    if(auto orig = addr in breakpoints){
                        *cast(Instruction*)addr = *orig;  // restore original
                    } else {
                        // User-written BREAK instruction - replace with NOP to continue
                        *cast(Instruction*)addr = NOP;
                    }
                    registers[RIP] = addr;  // rewind to re-execute
                    fprintf(stderr, "Breakpoint hit at 0x%zx\n", addr);
                    print_context(cast(uintptr_t*)addr);
                    int result = debugger();
                    if(paused || badend || result || halted)
                        return result;
                }break;
                default:
                case ABORT:
                    if(int b = begin(ABORT)) return b;
                    backtrace;
                    badend = true;
                    return 1;
                case NOP:
                    if(int b = begin(NOP)) return b;
                    break;
                case READ:{
                    if(int b = begin(READ)) return b;
                    uintptr_t* dst = get_reg();
                    uintptr_t* src = cast(uintptr_t*)read_reg();
                    *dst = *src;
                }break;
                case READ_I:{
                    if(int b = begin(READ_I)) return b;
                    uintptr_t* dst = get_reg();
                    uintptr_t* src = cast(uintptr_t*)get_unsigned();
                    *dst = *src;
                }break;
                case MOVE_R:{
                    if(int b = begin(MOVE_R)) return b;
                    uintptr_t* dst = get_reg();
                    uintptr_t* src = get_reg();
                    *dst = *src;
                }break;
                case LOCAL_READ:{
                    if(int b = begin(LOCAL_READ)) return b;
                    uintptr_t* dst = get_reg();
                    uintptr_t offset = get_unsigned();
                    ubyte* src = cast(ubyte*)registers[RBP];
                    src += offset;
                    *dst = *cast(uintptr_t*)src;
                }break;
                case LOCAL_WRITE:{
                    if(int b = begin(LOCAL_WRITE)) return b;
                    ubyte* dst = cast(ubyte*)registers[RBP];
                    uintptr_t offset = get_unsigned();
                    dst += offset;
                    uintptr_t* src = get_reg();
                    *cast(uintptr_t*)dst = *src;
                }break;
                case LOCAL_WRITE_I:{
                    if(int b = begin(LOCAL_WRITE_I)) return b;
                    ubyte* dst = cast(ubyte*)registers[RBP];
                    uintptr_t offset = get_unsigned();
                    dst += offset;
                    uintptr_t src = get_unsigned();
                    *cast(uintptr_t*)dst = src;
                }break;
                case MOVE_I:{
                    if(int b = begin(MOVE_I)) return b;
                    uintptr_t* dst = get_reg();
                    uintptr_t val = get_unsigned();
                    *dst = val;
                }break;
                case CMOVE_R:{
                    if(int b = begin(CMOVE_R)) return b;
                    uintptr_t cmp_mode = get_unsigned();
                    uintptr_t* dst = get_reg();
                    uintptr_t* src = get_reg();
                    int should_move = check_cmp(cmp_mode);
                    if(should_move == -1)
                        return 1;
                    if(should_move)
                        *dst = *src;
                }break;
                case CMOVE_I:{
                    if(int b = begin(CMOVE_I)) return b;
                    uintptr_t cmp_mode = get_unsigned();
                    uintptr_t* dst = get_reg();
                    uintptr_t val = get_unsigned();
                    int should_move = check_cmp(cmp_mode);
                    if(should_move == -1)
                        return 1;
                    if(should_move)
                        *dst = val;
                }break;
                case WRITE_R:{
                    if(int b = begin(WRITE_R)) return b;
                    uintptr_t* dst = cast(uintptr_t*)read_reg();
                    uintptr_t val = read_reg();
                    *dst = val;
                }break;
                case WRITE_I:{
                    if(int b = begin(WRITE_I)) return b;
                    uintptr_t* dst = cast(uintptr_t*)read_reg();
                    uintptr_t val = get_unsigned();
                    *dst = val;
                }break;
                case ITOD:{
                    if(int b = begin(ITOD)) return b;
                    double_t* dst = cast(double_t*)get_reg;
                    intptr_t* rhs = cast(intptr_t*)get_reg;
                    *dst = cast(double_t)*rhs;
                }break;
                case ITOF:{
                    if(int b = begin(ITOF)) return b;
                    uintptr_t* dst = get_reg;
                    intptr_t* rhs = cast(intptr_t*)get_reg;
                    float_t f = cast(float_t)*rhs;
                    *dst = cast(uintptr_t)*cast(uint*)&f;
                }break;
                case DTOI:{
                    if(int b = begin(DTOI)) return b;
                    intptr_t* dst = cast(intptr_t*)get_reg;
                    double_t* rhs = cast(double_t*)get_reg;
                    *dst = cast(intptr_t)*rhs;
                }break;
                case DTOF:{
                    if(int b = begin(DTOF)) return b;
                    float_t* dst = cast(float_t*)get_reg;
                    double_t* rhs = cast(double_t*)get_reg;
                    *dst = cast(float_t)*rhs;
                }break;
                case FTOD:{
                    if(int b = begin(FTOD)) return b;
                    double_t* dst = cast(double_t*)get_reg;
                    float_t* rhs = cast(float_t*)get_reg;
                    *dst = cast(double_t)*rhs;
                }break;
                case FTOI:{
                    if(int b = begin(FTOI)) return b;
                    intptr_t* dst = cast(intptr_t*)get_reg;
                    float_t* rhs = cast(float_t*)get_reg;
                    *dst = cast(intptr_t)*rhs;
                }break;
                case NOT:{
                    if(int b = begin(NOT)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* rhs = get_reg;
                    *dst = !*rhs;
                }break;
                case NEG:{
                    if(int b = begin(NEG)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* rhs = get_reg;
                    *dst = -*rhs;
                }break;
                case BINNEG:{
                    if(int b = begin(BINNEG)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* rhs = get_reg;
                    *dst = ~*rhs;
                }break;
                case DADD_I:{
                    if(int b = begin(DADD_I)) return b;
                    double_t* dst = cast(double_t*)get_reg;
                    double_t* lhs = cast(double_t*)get_reg;
                    double_t rhs = get_double;
                    *dst = *lhs + rhs;
                }break;
                case DADD_R:{
                    if(int b = begin(DADD_R)) return b;
                    double_t* dst = cast(double_t*)get_reg;
                    double_t* lhs = cast(double_t*)get_reg;
                    double_t* rhs = cast(double_t*)get_reg;
                    *dst = *lhs + *rhs;
                }break;
                case DSUB_I:{
                    if(int b = begin(DSUB_I)) return b;
                    double_t* dst = cast(double_t*)get_reg;
                    double_t* lhs = cast(double_t*)get_reg;
                    double_t rhs = get_double;
                    *dst = *lhs - rhs;
                }break;
                case DSUB_R:{
                    if(int b = begin(DSUB_R)) return b;
                    double_t* dst = cast(double_t*)get_reg;
                    double_t* lhs = cast(double_t*)get_reg;
                    double_t* rhs = cast(double_t*)get_reg;
                    *dst = *lhs - *rhs;
                }break;
                case DMUL_I:{
                    if(int b = begin(DMUL_I)) return b;
                    double_t* dst = cast(double_t*)get_reg;
                    double_t* lhs = cast(double_t*)get_reg;
                    double_t rhs = get_double;
                    *dst = *lhs * rhs;
                }break;
                case DMUL_R:{
                    if(int b = begin(DMUL_R)) return b;
                    double_t* dst = cast(double_t*)get_reg;
                    double_t* lhs = cast(double_t*)get_reg;
                    double_t* rhs = cast(double_t*)get_reg;
                    *dst = *lhs * *rhs;
                }break;
                case DDIV_I:{
                    if(int b = begin(DDIV_I)) return b;
                    double_t* dst = cast(double_t*)get_reg;
                    double_t* lhs = cast(double_t*)get_reg;
                    double_t rhs = get_double;
                    *dst = *lhs / rhs;
                }break;
                case DDIV_R:{
                    if(int b = begin(DDIV_R)) return b;
                    double_t* dst = cast(double_t*)get_reg;
                    double_t* lhs = cast(double_t*)get_reg;
                    double_t* rhs = cast(double_t*)get_reg;
                    *dst = *lhs / *rhs;
                }break;
                case DCMP_I:{
                    if(int b = begin(DCMP_I)) return b;
                    registers[RFLAGS] = 0;
                    double_t lhs = double_read_reg;
                    double_t rhs = get_double;
                    if(lhs == rhs){
                        registers[RFLAGS] = CmpFlags.ZERO;
                    }
                    else if(lhs < rhs){
                        registers[RFLAGS] = CmpFlags.CARRY;
                    }
                }break;
                case DCMP_R:{
                    if(int b = begin(DCMP_R)) return b;
                    registers[RFLAGS] = 0;
                    double_t lhs = double_read_reg;
                    double_t rhs = double_read_reg;
                    if(lhs == rhs){
                        registers[RFLAGS] = CmpFlags.ZERO;
                    }
                    else if(lhs < rhs){
                        registers[RFLAGS] = CmpFlags.CARRY;
                    }
                }break;
                case DNEG:{
                    if(int b = begin(DNEG)) return b;
                    double_t* dst = cast(double_t*)get_reg;
                    double_t* rhs = cast(double_t*)get_reg;
                    *dst = -*rhs;
                }break;
                case FADD_I:{
                    if(int b = begin(FADD_I)) return b;
                    float_t* dst = cast(float_t*)get_reg;
                    float_t* lhs = cast(float_t*)get_reg;
                    float_t rhs = get_float;
                    *dst = *lhs + rhs;
                }break;
                case FADD_R:{
                    if(int b = begin(FADD_R)) return b;
                    float_t* dst = cast(float_t*)get_reg;
                    float_t* lhs = cast(float_t*)get_reg;
                    float_t* rhs = cast(float_t*)get_reg;
                    *dst = *lhs + *rhs;
                }break;
                case FSUB_I:{
                    if(int b = begin(FSUB_I)) return b;
                    float_t* dst = cast(float_t*)get_reg;
                    float_t* lhs = cast(float_t*)get_reg;
                    float_t rhs = get_float;
                    *dst = *lhs - rhs;
                }break;
                case FSUB_R:{
                    if(int b = begin(FSUB_R)) return b;
                    float_t* dst = cast(float_t*)get_reg;
                    float_t* lhs = cast(float_t*)get_reg;
                    float_t* rhs = cast(float_t*)get_reg;
                    *dst = *lhs - *rhs;
                }break;
                case FMUL_I:{
                    if(int b = begin(FMUL_I)) return b;
                    float_t* dst = cast(float_t*)get_reg;
                    float_t* lhs = cast(float_t*)get_reg;
                    float_t rhs = get_float;
                    *dst = *lhs * rhs;
                }break;
                case FMUL_R:{
                    if(int b = begin(FMUL_R)) return b;
                    float_t* dst = cast(float_t*)get_reg;
                    float_t* lhs = cast(float_t*)get_reg;
                    float_t* rhs = cast(float_t*)get_reg;
                    *dst = *lhs * *rhs;
                }break;
                case FDIV_I:{
                    if(int b = begin(FDIV_I)) return b;
                    float_t* dst = cast(float_t*)get_reg;
                    float_t* lhs = cast(float_t*)get_reg;
                    float_t rhs = get_float;
                    *dst = *lhs / rhs;
                }break;
                case FDIV_R:{
                    if(int b = begin(FDIV_R)) return b;
                    float_t* dst = cast(float_t*)get_reg;
                    float_t* lhs = cast(float_t*)get_reg;
                    float_t* rhs = cast(float_t*)get_reg;
                    *dst = *lhs / *rhs;
                }break;
                case FCMP_I:{
                    if(int b = begin(FCMP_I)) return b;
                    registers[RFLAGS] = 0;
                    float_t lhs = float_read_reg;
                    float_t rhs = get_float;
                    if(lhs == rhs){
                        registers[RFLAGS] = CmpFlags.ZERO;
                    }
                    else if(lhs < rhs){
                        registers[RFLAGS] = CmpFlags.CARRY;
                    }
                }break;
                case FCMP_R:{
                    if(int b = begin(FCMP_R)) return b;
                    registers[RFLAGS] = 0;
                    float_t lhs = float_read_reg;
                    float_t rhs = float_read_reg;
                    if(lhs == rhs){
                        registers[RFLAGS] = CmpFlags.ZERO;
                    }
                    else if(lhs < rhs){
                        registers[RFLAGS] = CmpFlags.CARRY;
                    }
                }break;
                case FNEG:{
                    if(int b = begin(FNEG)) return b;
                    float_t* dst = cast(float_t*)get_reg;
                    float_t* rhs = cast(float_t*)get_reg;
                    *dst = -*rhs;
                }break;
                case ADD_I:{
                    if(int b = begin(ADD_I)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t rhs = get_unsigned;
                    *dst = (*lhs) + rhs;
                }break;
                case ADD_R:{
                    if(int b = begin(ADD_R)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t* rhs = get_reg;
                    *dst = (*lhs) + (*rhs);
                }break;
                case AND_I:{
                    if(int b = begin(AND_I)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t rhs = get_unsigned;
                    *dst = (*lhs) & rhs;
                }break;
                case AND_R:{
                    if(int b = begin(AND_R)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t* rhs = get_reg;
                    *dst = (*lhs) & (*rhs);
                }break;
                case LOGICAL_AND_I:{
                    if(int b = begin(LOGICAL_AND_I)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t rhs = get_unsigned;
                    *dst = (*lhs) && rhs;
                }break;
                case LOGICAL_AND_R:{
                    if(int b = begin(LOGICAL_AND_R)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t* rhs = get_reg;
                    *dst = (*lhs) && (*rhs);
                }break;
                case OR_I:{
                    if(int b = begin(OR_I)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t rhs = get_unsigned;
                    *dst = (*lhs) | rhs;
                }break;
                case OR_R:{
                    if(int b = begin(OR_R)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t* rhs = get_reg;
                    *dst = (*lhs) | (*rhs);
                }break;
                case LOGICAL_OR_I:{
                    if(int b = begin(LOGICAL_OR_I)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t rhs = get_unsigned;
                    *dst = (*lhs) || rhs;
                }break;
                case LOGICAL_OR_R:{
                    if(int b = begin(LOGICAL_OR_R)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t* rhs = get_reg;
                    *dst = (*lhs) || (*rhs);
                }break;
                case XOR_I:{
                    if(int b = begin(XOR_I)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t rhs = get_unsigned;
                    *dst = (*lhs) ^ rhs;
                }break;
                case XOR_R:{
                    if(int b = begin(XOR_R)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t* rhs = get_reg;
                    *dst = (*lhs) ^ (*rhs);
                }break;
                case SUB_I:{
                    if(int b = begin(SUB_I)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t rhs = get_unsigned;
                    *dst = (*lhs) - rhs;
                }break;
                case SUB_R:{
                    if(int b = begin(SUB_R)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t* rhs = get_reg;
                    *dst = (*lhs) - (*rhs);
                }break;
                case MUL_I:{
                    if(int b = begin(MUL_I)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t rhs = get_unsigned;
                    *dst = (*lhs) * rhs;
                }break;
                case MUL_R:{
                    if(int b = begin(MUL_R)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t* rhs = get_reg;
                    *dst = (*lhs) * (*rhs);
                }break;
                case SHIFTL_I:{
                    if(int b = begin(SHIFTL_I)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t rhs = get_unsigned;
                    *dst = (*lhs) << rhs;
                }break;
                case SHIFTL_R:{
                    if(int b = begin(SHIFTL_R)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t* rhs = get_reg;
                    *dst = (*lhs) << (*rhs);
                }break;
                case SHIFTR_I:{
                    if(int b = begin(SHIFTR_I)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t rhs = get_unsigned;
                    *dst = (*lhs) >> rhs;
                }break;
                case SHIFTR_R:{
                    if(int b = begin(SHIFTR_R)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t* rhs = get_reg;
                    *dst = (*lhs) >> (*rhs);
                }break;
                case DIV_I:{
                    if(int b = begin(DIV_I)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* dst2 = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t rhs = get_unsigned;
                    if(!rhs)
                        *dst = 0;
                    else
                        *dst = *lhs / rhs;
                    *dst2 = *lhs - (*dst)*rhs;
                }break;
                case DIV_R:{
                    if(int b = begin(DIV_R)) return b;
                    uintptr_t* dst = get_reg;
                    uintptr_t* dst2 = get_reg;
                    uintptr_t* lhs = get_reg;
                    uintptr_t* rhs = get_reg;
                    if(!*rhs)
                        *dst = 0;
                    else
                        *dst = *lhs / *rhs;
                    *dst2 = *lhs - (*dst)*(*rhs);
                }break;
                case PUSH_I:{
                    if(int b = begin(PUSH_I)) return b;
                    uintptr_t src = get_unsigned;
                    *cast(uintptr_t*)registers[RSP] = src;
                    registers[RSP] += uintptr_t.sizeof;
                }break;
                case PUSH_R:{
                    if(int b = begin(PUSH_R)) return b;
                    uintptr_t src = read_reg;
                    *cast(uintptr_t*)registers[RSP] = src;
                    registers[RSP] += uintptr_t.sizeof;
                }break;
                case POP:{
                    if(int b = begin(POP)) return b;
                    uintptr_t* dst = get_reg;
                    registers[RSP] -= uintptr_t.sizeof;
                    *dst = *cast(uintptr_t*)registers[RSP];
                }break;
                case CALL_I:{
                    if(int b = begin(CALL_I)) return b;
                    Function* f = cast(Function*)get_unsigned;
                    int err = do_call(f, f.n_args);
                    if(err) {
                        backtrace;
                        badend = true;
                        return err;
                    }
                }break;
                case CALL_I_NARGS:{
                    if(int b = begin(CALL_I_NARGS)) return b;
                    Function* f = cast(Function*)get_unsigned;
                    size_t n_args = cast(size_t)get_unsigned;
                    int err = do_call(f, n_args);
                    if(err) {
                        backtrace;
                        badend = true;
                        return err;
                    }
                }break;
                case CALL_I_NARGS_FLOATS:{
                    if(int b = begin(CALL_I_NARGS_FLOATS)) return b;
                    Function* f = cast(Function*)get_unsigned;
                    size_t n_args = cast(size_t)get_unsigned;
                    uint call_floats = cast(uint)get_unsigned;
                    int err = do_call(f, n_args, call_floats);
                    if(err) {
                        backtrace;
                        badend = true;
                        return err;
                    }
                }break;
                case CALL_R:{
                    if(int b = begin(CALL_R)) return b;
                    Function* f = cast(Function*)read_reg;
                    int err = do_call(f, f.n_args);
                    if(err) {
                        backtrace;
                        badend = true;
                        return err;
                    }
                }break;
                case CALL_R_NARGS:{
                    if(int b = begin(CALL_R_NARGS)) return b;
                    Function* f = cast(Function*)read_reg;
                    size_t n_args = cast(size_t)get_unsigned;
                    int err = do_call(f, n_args);
                    if(err) {
                        backtrace;
                        badend = true;
                        return err;
                    }
                }break;
                case CALL_R_NARGS_FLOATS:{
                    if(int b = begin(CALL_R_NARGS_FLOATS)) return b;
                    Function* f = cast(Function*)read_reg;
                    size_t n_args = cast(size_t)get_unsigned;
                    uint call_floats = cast(uint)get_unsigned;
                    int err = do_call(f, n_args, call_floats);
                    if(err) {
                        backtrace;
                        badend = true;
                        return err;
                    }
                }break;
                case TAIL_CALL_I:{
                    if(int b = begin(TAIL_CALL_I)) return b;
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
                    if(int b = begin(TAIL_CALL_R)) return b;
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
                    if(int b = begin(RET)) return b;
                    if(!call_depth){
                        halted = true;
                        return 0;
                    }
                    registers[RIP] = call_stack[--call_depth];
                }break;
                case JUMP_ABS_I:{
                    if(int b = begin(JUMP_ABS_I)) return b;
                    uintptr_t cmp_mode = get_unsigned;
                    uintptr_t amount = get_unsigned;
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
                    if(int b = begin(JUMP_REL_I)) return b;
                    uintptr_t cmp_mode = get_unsigned;
                    uintptr_t amount = get_unsigned;
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
                    if(int b = begin(JUMP_R)) return b;
                    uintptr_t cmp_mode = get_unsigned;
                    uintptr_t jmp_mode = get_unsigned;
                    uintptr_t amount = read_reg;
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
                    if(int b = begin(CMP_R)) return b;
                    registers[RFLAGS] = 0;
                    uintptr_t lhs = read_reg;
                    uintptr_t rhs = read_reg;
                    if(lhs == rhs){
                        registers[RFLAGS] = CmpFlags.ZERO;
                    }
                    else if(lhs < rhs){
                        registers[RFLAGS] = CmpFlags.CARRY;
                    }
                }break;
                case CMP_I:{
                    if(int b = begin(CMP_I)) return b;
                    registers[RFLAGS] = 0;
                    uintptr_t lhs = read_reg;
                    uintptr_t rhs = get_unsigned;
                    if(lhs == rhs){
                        registers[RFLAGS] = CmpFlags.ZERO;
                    }
                    else if(lhs < rhs){
                        registers[RFLAGS] = CmpFlags.CARRY;
                    }
                }break;
                case SCMP_R:{
                    if(int b = begin(SCMP_R)) return b;
                    registers[RFLAGS] = 0;
                    intptr_t lhs = cast(intptr_t)read_reg;
                    intptr_t rhs = cast(intptr_t)read_reg;
                    if(lhs == rhs){
                        registers[RFLAGS] = CmpFlags.ZERO;
                    }
                    else if(lhs < rhs){
                        registers[RFLAGS] = CmpFlags.CARRY;
                    }
                }break;
                case SCMP_I:{
                    if(int b = begin(SCMP_I)) return b;
                    registers[RFLAGS] = 0;
                    intptr_t lhs = cast(intptr_t)read_reg;
                    intptr_t rhs = get_signed;
                    if(lhs == rhs){
                        registers[RFLAGS] = CmpFlags.ZERO;
                    }
                    else if(lhs < rhs){
                        registers[RFLAGS] = CmpFlags.CARRY;
                    }
                }break;
                case DUMP:
                    if(int b = begin(DUMP)) return b;
                    dump;
                    break;
                case MSG:
                    if(int b = begin(MSG)) return b;
                    if(!Fuzzing)fprintf(stderr, "%s\n", cast(char*)get_unsigned);
                    break;
                case MEMCPY_I:{
                    if(int b = begin(MEMCPY_I)) return b;
                    void* dst = cast(void*)read_reg;
                    void* src = cast(void*)read_reg;
                    uintptr_t n = get_unsigned;
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
                    if(int b = begin(MEMCPY_R)) return b;
                    void* dst = cast(void*)read_reg;
                    void* src = cast(void*)read_reg;
                    uintptr_t n = read_reg;
                    switch(n){
                        case 1  : memcpy(dst, src, 1); break;
                        case 2  : memcpy(dst, src, 2); break;
                        case 4  : memcpy(dst, src, 4); break;
                        case 8  : memcpy(dst, src, 8); break;
                        case 16 : memcpy(dst, src, 16); break;
                        default : memcpy(dst, src, n); break;
                    }
                }break;
                case MEMZERO_I:{
                    if(int b = begin(MEMZERO_I)) return b;
                    void* dst = cast(void*)read_reg;
                    uintptr_t n = get_unsigned;
                    memset(dst, 0, n);
                }break;
                case MEMZERO_R:{
                    if(int b = begin(MEMZERO_R)) return b;
                    void* dst = cast(void*)read_reg;
                    uintptr_t n = read_reg;
                    memset(dst, 0, n);
                }break;
                case PAUSE:
                    if(int b = begin(PAUSE)) return b;
                    paused = true;
                    return 0;
                case DEBUG_OP:{
                    if(int b = begin(DEBUG_OP)) return b;
                    int result = debugger();
                    if(paused || badend || result || halted)
                        return result;
                }break;
                case BACKTRACE:
                    if(int b = begin(BACKTRACE)) return b;
                    backtrace();
                    break;
                case LEA:{
                    if(int b = begin(LEA)) return b;
                    uintptr_t* dst = get_reg();
                    uintptr_t* x = get_reg();
                    uintptr_t a = get_unsigned();
                    uintptr_t* y = get_reg();
                    uintptr_t z = get_unsigned();
                    *dst = *x + a*(*y) + z;
                }break;
                case READ1:{
                    if(int b = begin(READ1)) return b;
                    uintptr_t* dst = get_reg();
                    ubyte* src = cast(ubyte*)read_reg();
                    *dst = *src;
                }break;
                case READ2:{
                    if(int b = begin(READ2)) return b;
                    uintptr_t* dst = get_reg();
                    ushort* src = cast(ushort*)read_reg();
                    *dst = *src;
                }break;
                case READ4:{
                    if(int b = begin(READ4)) return b;
                    uintptr_t* dst = get_reg();
                    uint* src = cast(uint*)read_reg();
                    *dst = *src;
                }break;
                case WRITE1:{
                    if(int b = begin(WRITE1)) return b;
                    ubyte* dst = cast(ubyte*)read_reg();
                    uintptr_t val = read_reg();
                    *dst = cast(ubyte)val;
                }break;
                case WRITE2:{
                    if(int b = begin(WRITE2)) return b;
                    ushort* dst = cast(ushort*)read_reg();
                    uintptr_t val = read_reg();
                    *dst = cast(ushort)val;
                }break;
                case WRITE4:{
                    if(int b = begin(WRITE4)) return b;
                    uint* dst = cast(uint*)read_reg();
                    uintptr_t val = read_reg();
                    *dst = cast(uint)val;
                }break;
                case SREAD1:{
                    if(int b = begin(SREAD1)) return b;
                    uintptr_t* dst = get_reg();
                    byte* src = cast(byte*)read_reg();
                    *dst = cast(uintptr_t)cast(intptr_t)*src;
                }break;
                case SREAD2:{
                    if(int b = begin(SREAD2)) return b;
                    uintptr_t* dst = get_reg();
                    short* src = cast(short*)read_reg();
                    *dst = cast(uintptr_t)cast(intptr_t)*src;
                }break;
                case SREAD4:{
                    if(int b = begin(SREAD4)) return b;
                    uintptr_t* dst = get_reg();
                    int* src = cast(int*)read_reg();
                    *dst = cast(uintptr_t)cast(intptr_t)*src;
                }break;
            }
            static if(flags & RunFlags.RUN_ONE)
                return BEGIN_CONTINUE;
        }
    }
    }
}

int
disassemble_one_instruction(SB)(LinkedModule* prog, SB* sb, uintptr_t* ip, uintptr_t override_inst = Instruction.max + 1){
    uintptr_t inst = (override_inst <= Instruction.max) ? override_inst : *ip;
    if(inst > Instruction.max){
        sb.write("UNK");
        sb.hex("0x", inst);
        return 1;
    }
    with(Instruction) with(ArgumentKind){
        immutable InstructionInfo* ii = &INSTRUCTION_INFOS[inst];
        sb.write(ii.asm_name);
        if(!ii.args.length) return 1;
        foreach(i, kind; ii.args){
            sb.write(' ');
            uintptr_t value = ip[i+1];
            switch(kind){
                default:{
                    Function* func = cast(Function*)value;
                    foreach(v; prog.functions.values){
                        if(v.func == func){
                            sb.write("function ");
                            sb.write(v.name);
                            goto handled;
                        }
                    }
                    // Check imports
                    foreach(imp; prog.imports.items){
                        foreach(v; imp.value.functions.values){
                            if(v.func == func){
                                sb.write("function ");
                                sb.write(imp.key);
                                sb.write('.');
                                sb.write(v.name);
                                goto handled;
                            }
                        }
                    }
                    foreach(str; prog.strings){
                        if(str.ptr == cast(const char*)value)
                            goto case STRING;
                    }
                    foreach(imp; prog.imports.items){
                        foreach(str; imp.value.strings){
                            if(str.ptr == cast(const char*)value)
                                goto case STRING;
                        }
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
                            goto handled;
                        }
                    }
                    foreach(imp; prog.imports.items){
                        foreach(str; imp.value.strings){
                            if(str.ptr == cast(const char*)value){
                                sb.write(Q(E(str[]), '"'));
                                goto handled;
                            }
                        }
                    }
                    continue;
                }
                case REGISTER:{
                    bool written = false;
                    if(immutable RegisterInfo* ri = get_register_info(value)){
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
