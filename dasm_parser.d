/*
 * Copyright © 2021-2022, David Priver
 */
import core.stdc.stdio: fprintf, stderr;

import dlib.allocator;
import dlib.zstring;
import dlib.btable;
import dlib.stringbuilder;
import dlib.parse_numbers;

import dvm_defs;
import dvm_unlinked;
import dvm_instructions;
import dvm_args;
import dvm_regs;
import dasm_token;
import dasm_tokenizer;


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
    }
    return &constants_table;
}
