/*
 * Copyright © 2021-2023, David Priver
 */
module dvm.dvm_linker;
import core.stdc.stdio: fprintf, stderr;

import dlib.zstring;
import dlib.allocator;
import dlib.box;
import dlib.stringbuilder;
import dlib.parse_numbers: parse_hex_inner;
import dlib.table;
import dlib.str_util: split;
import dlib.file_util: read_file, FileResult;

import dvm.dvm_defs;
import dvm.dvm_linked;
import dvm.dvm_unlinked;
import dvm.dvm_args;
import dvm.dvm_instructions;

// Convert un-escaped raw text into a ZString, processing escape sequences.
ZString process_escapes(Allocator allocator, const char[] text){
    StringBuilder sb;
    sb.allocator = allocator;
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
            sb.write('\\');
            continue;
        }
        sb.write(c);
    }
    return sb.zdetach;
}

struct LinkContext {
    Allocator allocator;
    Allocator temp_allocator;
    FunctionTable* builtins;
    UnlinkedModule* unlinked;
    Table!(str, LinkedModule*)* modules;

    LinkedModule prog;
    void delegate(const char*, out str, out int, out int) find_loc;
    Box!(char[]) errmess;

    void
    err_print(A...)(const char* first_char, A args){
        StringBuilder sb = {allocator:MALLOCATOR};
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
        // Calculate total storage needed (sum of all variable sizes)
        size_t total_size = 0;
        foreach(ref var; unlinked.variables[]){
            total_size += var.size;
        }
        if(total_size)
            prog.variables.resize(total_size);
        // Assign offsets and build variable table
        size_t offset = 0;
        foreach(ref var; unlinked.variables[]){
            if(var.name in prog.variable_table){
                err_print(var.first_char, "Duplicate variable: ", Q(var.name));
                return AsmError.LINK_ERROR;
            }
            prog.variable_table[var.name] = &prog.variables.data[offset];
            offset += var.size;
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
        ZString result = process_escapes(allocator, text);
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
        foreach(i, const ref AbstractArray abstract_array; unlinked.arrays[]){with(ArgumentKind){
            IntegerArray* actual = &prog.arrays[i];
            foreach(j, const ref Argument v; abstract_array.array[]){
                switch(v.kind){
                    default:
                    case LABEL:
                        err_print(v.first_char, "BUG: Unknown label");
                        return AsmError.LINK_ERROR;
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
                        uintptr_t fi = find_function(v.function_name, v.first_char);
                        if(!fi)
                            return AsmError.LINK_ERROR;
                        else
                            (*actual)[j] = fi;
                        }break;
                    case ARRAY:
                        (*actual)[j] = cast(uintptr_t)prog.arrays[i].bdata.data.ptr;
                        break;
                    case VARIABLE:
                        if(uintptr_t** var = v.variable in prog.variable_table)
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
        size_t offset = 0;
        foreach(const ref AbstractVariable var; unlinked.variables[]){
            uintptr_t* base = &prog.variables.data[offset];
            // Zero-initialize the entire variable storage
            foreach(j; 0 .. var.size)
                base[j] = 0;
            // Link each initializer - track position separately from arg index
            size_t pos = 0;
            foreach(const ref Argument arg; var.initializers[]){
                uintptr_t* dest = &base[pos];
                switch(arg.kind){
                    default:
                    case LABEL:
                        err_print(arg.first_char, "BUG LABEL");
                        return AsmError.LINK_ERROR;
                    case UNSET:
                        err_print(arg.first_char, "BUG");
                        return AsmError.LINK_ERROR;
                    case STRING:{
                        ZString s = make_string(arg.text);
                        *dest = cast(uintptr_t)s.ptr;
                        pos++;
                    }break;
                    case IMMEDIATE:
                        *dest = arg.immediate;
                        pos++;
                        break;
                    case REGISTER:
                        *dest = arg.reg;
                        pos++;
                        break;
                    case CMPMODE:
                        *dest = arg.cmp_mode;
                        pos++;
                        break;
                    case FUNCTION:{
                        auto fi = find_function(arg.function_name, arg.first_char);
                        if(!fi)
                            return AsmError.LINK_ERROR;
                        else
                            *dest = fi;
                        pos++;
                    }break;
                    case ARRAY:
                        *dest = cast(uintptr_t)prog.arrays[arg.array].bdata.data.ptr;
                        pos++;
                        break;
                    case VARIABLE:
                        if(auto variable = arg.variable in prog.variable_table)
                            (*dest) = cast(uintptr_t)*variable;
                        else {
                            err_print(arg.first_char, "Reference to unknown variable: ", Q(arg.variable));
                            return AsmError.LINK_ERROR;
                        }
                        pos++;
                        break;
                    case CONSTANT:
                        err_print(arg.first_char, "TODO");
                        return AsmError.LINK_ERROR;
                    case EMBED:{
                        // Read file and copy bytes as words
                        import core.stdc.string: memcpy;
                        ZString path = make_string(arg.embed.path);
                        FileResult file = read_file(path.ptr, temp_allocator);
                        if(file.errored){
                            err_print(arg.first_char, "Failed to read embed file: ", arg.embed.path);
                            return AsmError.LINK_ERROR;
                        }
                        scope(exit) file.value.dealloc;
                        // Validate offset and length
                        size_t file_len = file.value.data.length;
                        if(arg.embed.offset >= file_len){
                            err_print(arg.first_char, "Embed offset exceeds file size");
                            return AsmError.LINK_ERROR;
                        }
                        if(arg.embed.offset + arg.embed.length > file_len){
                            err_print(arg.first_char, "Embed range exceeds file size");
                            return AsmError.LINK_ERROR;
                        }
                        // Round up to full words (remaining bytes zero-padded by var init)
                        size_t num_words = (arg.embed.length + 7) / 8;
                        if(pos + num_words > var.size){
                            err_print(arg.first_char, "Embed data exceeds variable size");
                            return AsmError.LINK_ERROR;
                        }
                        // Copy data (var storage already zero-initialized)
                        ubyte* src = cast(ubyte*)file.value.data.ptr + arg.embed.offset;
                        memcpy(dest, src, arg.embed.length);
                        pos += num_words;
                    }break;
                }
            }
            offset += var.size;
        }
        return AsmError.NO_ERROR;
    }
    }

    AsmError
    link_functions(){
        foreach(i, ref Function func; prog.function_store.data){
            AbstractFunction* afunc = &unlinked.functions[i];
            if(AsmError err = link_function(afunc, &func))
                return err;
        }
        return AsmError.NO_ERROR;
    }

    AsmError
    link_function(AbstractFunction* afunc, Function* func){
        Table!(str, uintptr_t) labels;
        labels.data.allocator = temp_allocator;
        scope(exit) labels.cleanup;
        // look for labels
        {
            uintptr_t* ip = func.instructions_;
            foreach(const ref AbstractInstruction inst; afunc.instructions[]){
                if(inst.label.length){
                    if(inst.label in labels){
                        err_print(inst.first_char, "Duplicate label ", Q(inst.label));
                        return AsmError.LINK_ERROR;
                    }
                    labels[inst.label] = cast(uintptr_t)ip;
                    continue;
                }
                ip += instruction_size(inst.instruction);
            }
        }
        uintptr_t* ip = func.instructions_;
        foreach(const ref AbstractInstruction inst; afunc.instructions[]){
            if(inst.label.length) continue;
            *(ip++) = inst.instruction;
            foreach(const ref Argument arg;inst.args[0..inst.n_args]){
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
                        uintptr_t fi = find_function(arg.function_name, arg.first_char);
                        if(!fi)
                            return AsmError.LINK_ERROR;
                        else
                            *(ip++) = fi;
                    }break;
                    case LABEL:{
                        if(uintptr_t* label = arg.label_name in labels){
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
                        if(uintptr_t** var = arg.variable in prog.variable_table){
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
        if(AsmError err = allocate_arrays)
            return err;
        if(AsmError err = allocate_variables)
            return err;
        if(AsmError err = allocate_functions)
            return err;
        if(AsmError err = link_functions)
            return err;
        if(AsmError err = link_arrays)
            return err;
        if(AsmError err = link_variables)
            return err;
        return AsmError.NO_ERROR;
    }
}

size_t
calculate_function_size(AbstractFunction* func){
    size_t size = 0;
    foreach(const ref AbstractInstruction inst; func.instructions[]){
        if(inst.label.length) continue;
        size += instruction_size(inst.instruction);
    }
    return size;
}

struct SingleInstructionLinkResult {
    // opcode + up to 5 args + return jump (MOVE_I RIP addr = 3 words)
    enum MAX_SIZE = 16;
    uintptr_t[MAX_SIZE] bytecode;
    size_t length;
    ZString errmess;
    bool success;
}

// Link a single AbstractInstruction using the symbol tables from an existing LinkedModule.
// Appends a MOVE_I RIP <return_addr> to jump back after execution.
// Returns bytecode ready for execution.
SingleInstructionLinkResult
link_single_instruction(Allocator allocator, const ref AbstractInstruction inst, LinkedModule* prog, uintptr_t return_addr){
    import dvm.dvm_regs : RegisterNames;
    SingleInstructionLinkResult result;
    size_t idx = 0;

    result.bytecode[idx++] = inst.instruction;

    foreach(const ref Argument arg; inst.args[0..inst.n_args]){
        if(idx >= result.MAX_SIZE){
            result.errmess = ZString.literal("Too many arguments");
            return result;
        }

        with(ArgumentKind) switch(arg.kind){
            default:
            case UNSET:
                result.errmess = ZString.literal("Invalid argument");
                return result;

            case STRING:
                ZString z = process_escapes(allocator, arg.text);
                prog.strings ~= z;
                result.bytecode[idx++] = cast(uintptr_t)z.ptr;
                break;

            case IMMEDIATE:
                result.bytecode[idx++] = arg.immediate;
                break;

            case REGISTER:
                result.bytecode[idx++] = arg.reg;
                break;

            case CMPMODE:
                result.bytecode[idx++] = arg.cmp_mode;
                break;

            case FUNCTION:
                if(auto fi = arg.function_name in prog.functions){
                    result.bytecode[idx++] = cast(uintptr_t)fi.func;
                } else {
                    // Try module.function lookup
                    auto s = arg.function_name.split('.');
                    if(s.tail.length){
                        if(auto mod = s.head in prog.imports){
                            if(auto fi = s.tail in (*mod).functions){
                                result.bytecode[idx++] = cast(uintptr_t)fi.func;
                                break;
                            }
                        }
                    }
                    StringBuilder sb = {allocator: allocator};
                    sb.FORMAT("Unknown function: ", arg.function_name);
                    result.errmess = sb.zdetach;
                    return result;
                }
                break;

            case VARIABLE:
                if(auto var = arg.variable in prog.variable_table){
                    result.bytecode[idx++] = cast(uintptr_t)*var;
                } else {
                    // Try module.variable lookup
                    auto s = arg.variable.split('.');
                    if(s.tail.length){
                        if(auto mod = s.head in prog.imports){
                            if(auto var = s.tail in (*mod).variable_table){
                                result.bytecode[idx++] = cast(uintptr_t)*var;
                                break;
                            }
                        }
                    }
                    StringBuilder sb = {allocator: allocator};
                    sb.FORMAT("Unknown variable: ", arg.variable);
                    result.errmess = sb.zdetach;
                    return result;
                }
                break;

            case LABEL:
                result.errmess = ZString.literal("Labels not supported in single instruction");
                return result;

            case ARRAY:
                result.errmess = ZString.literal("Arrays not supported in single instruction");
                return result;

            case CONSTANT:
                result.errmess = ZString.literal("Constants not supported in single instruction");
                return result;
        }
    }

    // Append MOVE_I RIP <return_addr> to jump back after execution
    if(idx + 3 > result.MAX_SIZE){
        result.errmess = ZString.literal("Instruction too large for return jump");
        return result;
    }
    result.bytecode[idx++] = Instruction.MOVE_I;
    result.bytecode[idx++] = RegisterNames.RIP;
    result.bytecode[idx++] = return_addr;

    result.length = idx;
    result.success = true;
    return result;
}

AsmError
link_module(
    Allocator allocator,
    Allocator temp_allocator,
    FunctionTable* builtins,
    UnlinkedModule* unlinked,
    LinkedModule* prog,
    scope void delegate(const char*, out str, out int, out int) find_loc,
    Table!(str, LinkedModule*)* modules,
){
    LinkContext ctx;
    ctx.allocator = allocator;
    ctx.temp_allocator = temp_allocator;
    ctx.builtins  = builtins;
    ctx.unlinked  = unlinked;
    ctx.prog.bytecode.allocator = allocator;
    ctx.prog.strings.bdata.allocator = allocator;
    ctx.prog.arrays.bdata.allocator = allocator;
    ctx.prog.functions.data.allocator = allocator;
    ctx.prog.functions.extend(builtins.items);
    ctx.prog.function_store.allocator = allocator;
    ctx.prog.variables.allocator = allocator;
    ctx.prog.variable_table.data.allocator = allocator;
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
    ctx.prog.imports.data.allocator = allocator;
    ctx.prog.imports.extend(modules.items);
    *prog = ctx.prog;
    return AsmError.NO_ERROR;
}
