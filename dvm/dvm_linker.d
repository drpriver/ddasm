/*
 * Copyright © 2021-2022, David Priver
 */
module dvm.dvm_linker;
import core.stdc.stdio: fprintf, stderr;

import dlib.zstring;
import dlib.allocator;
import dlib.box;
import dlib.stringbuilder;
import dlib.parse_numbers: parse_hex_inner;
import dlib.btable;
import dlib.str_util: split;

import dvm.dvm_defs;
import dvm.dvm_linked;
import dvm.dvm_unlinked;
import dvm.dvm_args;
import dvm.dvm_instructions;

struct LinkContext {
    VAllocator* allocator;
    VAllocator* temp_allocator;
    FunctionTable* builtins;
    UnlinkedModule* unlinked;
    BTable!(str, LinkedModule*, VAllocator)* modules;

    LinkedModule prog;
    void delegate(const char*, out str, out int, out int) find_loc;
    Box!(char[], Mallocator) errmess;

    void
    err_print(A...)(const char* first_char, A args){
        StringBuilder!Mallocator sb;
        int line, column;
        str fn;
        find_loc(first_char, fn, line, column);
        sb.FORMAT(fn, ':', line, ':', column, ": LinkError: ");
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
                err_print(var.first_char, "Duplicate variable: ", Q(var.name));
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
                err_print(func.first_char, "Duplicate function: ", Q(func.name));
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

    uintptr_t
    find_function(str function_name, const char* first_char){
        if(auto func = function_name in  prog.functions)
            return cast(uintptr_t)func.func;
        auto s = function_name.split('.');
        if(s.tail.length){
            if(!modules){
                err_print(first_char, "Reference to unknown module: ", Q(s.head));
                return 0;
            }
            if(auto mod = s.head in *modules){
                if(auto func = s.tail in (*mod).functions){
                    return cast(uintptr_t)func.func;
                }
                else {
                    err_print(first_char, "Reference to function ", Q(s.tail), " not found in module ", Q(s.head));
                    return 0;
                }
            }
            else {
                err_print(first_char, "Reference to unknown module: ", Q(s.head));
                return 0;
            }
        }

        err_print(first_char, "Reference to unknown function: ", Q(function_name));
        return 0;
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
                        err_print(v.first_char, "BUG");
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
                    case FUNCTION:{
                        auto fi = find_function(v.function_name, v.first_char);
                        if(!fi)
                            return AsmError.LINK_ERROR;
                        else
                            (*actual)[j] = fi;
                        }break;
                    case ARRAY:
                        (*actual)[j] = cast(uintptr_t)prog.arrays[i].bdata.data.ptr;
                        break;
                    case VARIABLE:
                        if(auto var = v.variable in prog.variable_table)
                            (*actual)[j] = cast(uintptr_t)*var;
                        else {
                            err_print(v.first_char, "Reference to unknown variable: ", Q(v.variable));
                            return AsmError.LINK_ERROR;
                        }
                        break;

                    case CONSTANT:
                        err_print(v.first_char, "TODO");
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
                    err_print(var.value.first_char, "BUG");
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
                case FUNCTION:{
                    auto fi = find_function(var.value.function_name, var.value.first_char);
                    if(!fi)
                        return AsmError.LINK_ERROR;
                    else
                        *dest = fi;
                }break;
                case ARRAY:
                    *dest = cast(uintptr_t)prog.arrays[var.value.array].bdata.data.ptr;
                    break;
                case VARIABLE:
                    if(auto variable = var.value.variable in prog.variable_table)
                        (*dest) = cast(uintptr_t)*variable;
                    else {
                        err_print(var.value.first_char, "Reference to unknown variable: ", Q(var.value.variable));
                        return AsmError.LINK_ERROR;
                    }
                    break;
                case CONSTANT:
                    err_print(var.value.first_char, "TODO");
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
        BTable!(str, uintptr_t, VAllocator) labels;
        labels.allocator = temp_allocator;
        scope(exit) labels.cleanup;
        // look for labels
        {
            uintptr_t* ip = func.instructions_;
            foreach(inst; afunc.instructions[]){
                if(inst.label.length){
                    if(inst.label in labels){
                        err_print(inst.first_char, "Duplicate label ", Q(inst.label));
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
                        err_print(afunc.first_char, "BUG: link_function");
                        return AsmError.LINK_ERROR;
                    case STRING:{
                        ZString s = make_string(arg.text);
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
                        auto fi = find_function(arg.function_name, arg.first_char);
                        if(!fi)
                            return AsmError.LINK_ERROR;
                        else
                            *(ip++) = fi;
                    }break;
                    case LABEL:{
                        if(auto label = arg.label_name in labels){
                            *(ip++) = *label;
                        }
                        else {
                            err_print(arg.first_char, "Reference to unknown label: ", Q(arg.label_name));
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
                            err_print(arg.first_char, "Reference to unknown variable: ", Q(arg.variable));
                            return AsmError.LINK_ERROR;
                        }
                        break;

                    case CONSTANT:
                        err_print(arg.first_char, "TODO");
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
link_module(VAllocator* allocator, VAllocator* temp_allocator, FunctionTable* builtins, UnlinkedModule* unlinked, LinkedModule* prog, scope void delegate(const char*, out str, out int, out int) find_loc, BTable!(str, LinkedModule*, VAllocator)* modules){
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
    ctx.prog.name = prog.name;
    ctx.find_loc = find_loc;
    ctx.modules = modules;
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
