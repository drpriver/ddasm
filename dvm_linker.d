import core.stdc.stdio: fprintf, stderr;

import dlib.zstring;
import dlib.allocator;
import dlib.box;
import dlib.stringbuilder;
import dlib.parse_numbers: parse_hex_inner;
import dlib.btable;

import dasm_token;
import dvm_defs;
import dvm_linked;
import dvm_unlinked;
import dvm_args;
import dvm_instructions;

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


// This is really stupid and unnecessarily entangles us to
// a module coming from dasm. Unless the dasm tokenizer and
// davescript tokenizer are shared? That would be kind of 
// weird though. It's ok for now as it is just for reporting
// errors.
Token
find_token(ref LinkedModule mod, const(char)* first_char){
    import dasm_tokenizer: Tokenizer;
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
