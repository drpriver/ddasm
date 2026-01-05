/*
 * Copyright © 2021-2023, David Priver
 */
module dasm.dasm_parser;
import core.stdc.stdio: fprintf, stderr;

import dlib.aliases;
import dlib.allocator;
import dlib.zstring;
import dlib.table;
import dlib.stringbuilder;
import dlib.parse_numbers: parse_unsigned_human, IntegerResult, is_float_literal, parse_float, FloatResult;

import dvm.dvm_defs;
import dvm.dvm_unlinked;
import dvm.dvm_instructions;
import dvm.dvm_args;
import dvm.dvm_regs;
import dasm.dasm_token;
import dasm.dasm_tokenizer;


int
parse_asm_string(Allocator allocator, str text, UnlinkedModule* prog){
    ParseContext ctx;
    ctx.allocator = allocator;
    ctx.tokenizer = Tokenizer.from(text);
    ctx.prog.functions.bdata.allocator = ctx.allocator;
    ctx.prog.variables.bdata.allocator = ctx.allocator;
    ctx.prog.arrays.bdata.allocator = ctx.allocator;
    ctx.prog.imports.bdata.allocator = ctx.allocator;
    ctx.prog.dlimports.bdata.allocator = ctx.allocator;
    int err = ctx.parse_asm();
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
    Allocator allocator;
    UnlinkedModule prog;
    ZString errmess;
    void
    err_print(A...)(Token tok, A args){
        StringBuilder sb = {allocator:MALLOCATOR};
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
                        if(tok.type == NUMBER){
                            IntegerResult!ulong err = parse_unsigned_human(tok.text);
                            if(err.errored){
                                err_print(tok, "Unable to parse a number from ", Q(tok.text));
                                return PARSE_ERROR;
                            }
                            if(err.value > int.max){
                                err_print(tok, "number (", tok.text, ") exceeds ", int.max);
                                return PARSE_ERROR;
                            }
                            tok = tokenizer.current_token_and_advance;
                            func.n_args = cast(int)err.value;
                        }
                        else {
                        }
                        int err = parse_function(&func);
                        if(err) return err;
                        prog.functions.push(func);
                    }
                    else if(tok.text == "import"){
                        tok = tokenizer.current_token_and_advance;
                        if(tok.type != SPACE){
                            err_print(tok, "import must be followed by a space: ", Q(tok.text));
                            return PARSE_ERROR;
                        }
                        tok = tokenizer.current_token_and_advance;
                        if(tok.type != IDENTIFIER){
                            err_print(tok, "expected an import name");
                            return PARSE_ERROR;
                        }
                        prog.imports.push(tok.text);
                    }
                    else if(tok.text == "dlimport"){
                        // dlimport alias
                        //   "libpath1"
                        //   "libpath2"
                        //   funcname n_args n_ret [varargs]
                        //   objname object
                        //   ...
                        // end
                        DlimportDecl decl;
                        decl.library_paths.bdata.allocator = allocator;
                        decl.funcs.bdata.allocator = allocator;
                        decl.objs.bdata.allocator = allocator;
                        tok = tokenizer.current_token_and_advance;
                        if(tok.type != SPACE){
                            err_print(tok, "dlimport must be followed by a space");
                            return PARSE_ERROR;
                        }
                        tok = tokenizer.current_token_and_advance;
                        if(tok.type != IDENTIFIER){
                            err_print(tok, "expected alias name");
                            return PARSE_ERROR;
                        }
                        decl.alias_name = tok.text;
                        // Skip to next line
                        tok = tokenizer.current_token_and_advance;
                        while(tok.type == SPACE || tok.type == TAB)
                            tok = tokenizer.current_token_and_advance;
                        tok = tokenizer.skip_comment(tok);
                        if(tok.type != NEWLINE && tok.type != EOF){
                            err_print(tok, "expected newline after dlimport header");
                            return PARSE_ERROR;
                        }
                        int err = parse_dlimport(&decl);
                        if(err) return err;
                        if(decl.library_paths.count == 0){
                            err_print(tok, "dlimport block must contain at least one library path");
                            return PARSE_ERROR;
                        }
                        prog.dlimports.push(decl);
                    }
                    else if (tok.text == "module"){
                        if(prog.name.length){
                            err_print(tok, "Module already has a name (", Q(prog.name), ")");
                            return PARSE_ERROR;
                        }
                        tok = tokenizer.current_token_and_advance;
                        if(tok.type != SPACE){
                            err_print(tok, "module must be followed by a space: ", Q(tok.text));
                            return PARSE_ERROR;
                        }
                        tok = tokenizer.current_token_and_advance;
                        if(tok.type != IDENTIFIER){
                            err_print(tok, "expected a module name");
                            return PARSE_ERROR;
                        }
                        prog.name = tok.text;
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
                        var.first_char = tok.text.ptr;
                        var.name = tok.text;
                        var.initializers.bdata.allocator = allocator;
                        tok = tokenizer.current_token_and_advance;
                        if(tok.type != SPACE){
                            err_print(tok, "var name must be followed by a space");
                            return PARSE_ERROR;
                        }
                        // Parse size (must be a number)
                        tok = tokenizer.current_token_and_advance;
                        while(tok.type == SPACE || tok.type == TAB)
                            tok = tokenizer.current_token_and_advance;
                        if(tok.type != NUMBER){
                            err_print(tok, "expected variable size (a number), got ", Q(tok.text));
                            return PARSE_ERROR;
                        }
                        IntegerResult!ulong size_result = parse_unsigned_human(tok.text);
                        if(size_result.errored){
                            err_print(tok, "Unable to parse size from ", Q(tok.text));
                            return PARSE_ERROR;
                        }
                        var.size = cast(size_t)size_result.value;
                        if(var.size == 0){
                            err_print(tok, "variable size must be at least 1");
                            return PARSE_ERROR;
                        }
                        // Parse initializers until 'end'
                        for(;;){
                            tok = tokenizer.current_token_and_advance;
                            while(tok.type == SPACE || tok.type == TAB || tok.type == NEWLINE)
                                tok = tokenizer.current_token_and_advance;
                            tok = tokenizer.skip_comment(tok);
                            if(tok.type == NEWLINE)
                                continue;
                            if(tok.type == EOF){
                                err_print(tok, "unexpected EOF in variable declaration, expected 'end'");
                                return PARSE_ERROR;
                            }
                            if(tok.type == IDENTIFIER && tok.text == "end")
                                break;
                            Argument arg = parse_one_argument(tok);
                            switch(arg.kind){
                                case UNSET:
                                case REGISTER:
                                case LABEL:
                                    if(!errmess.length) err_print(tok, "Invalid variable initializer");
                                    return PARSE_ERROR;
                                default:
                                    var.initializers.push(arg);
                                    break;
                            }
                        }
                        // Validate initializer count (embeds contribute ceil(length/8) words)
                        size_t effective_count = 0;
                        foreach(ref arg; var.initializers[]){
                            if(arg.kind == EMBED){
                                effective_count += (arg.embed.length + 7) / 8;
                            } else {
                                effective_count++;
                            }
                        }
                        if(effective_count > var.size){
                            err_print(tok, "too many initializers: got ", effective_count, " but size is ", var.size);
                            return PARSE_ERROR;
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
            while(tok.type == SPACE || tok.type == NEWLINE || tok.type == TAB || tok.type == SEMICOLON)
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
                uintptr_t end = func.instructions[$-1].instruction;
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
        Token first_tok = tok;
        import dlib.barray: Barray;
        Barray!(InstructionInfo)* infos = InstructionTable.get_item(cast(string)tok.text);
        if(!infos){
            err_print(tok, Q(tok.text), " does not match any known instruction");
            return PARSE_ERROR;
        }
        for(;;){
            tok = tokenizer.current_token_and_advance;
            while(tok.type == SPACE || tok.type == TAB)
                tok = tokenizer.current_token_and_advance;
            if(tok.type == NEWLINE || tok.type == SEMICOLON)
                break;
            if(tok.type == POUND){
                tok = tokenizer.skip_comment(tok);
                break;
            }
            Argument arg = parse_one_argument(tok);
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
                        case SEMICOLON:
                            continue;
                        default:{
                            Argument arg = parse_one_argument(tok);
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
            case APOSTROPHE:
            case QUOTATION:{
                TokenType q = tok.type;
                const char * before = tok.text.ptr;
                bool backslash = false;
                tok = tokenizer.current_token_and_advance;
                for(;;){
                    if(tok.type == q && !backslash)
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
                IntegerResult!ulong e = parse_unsigned_human(tok.text);
                if(e.errored){
                    err_print(tok, "Unable to parse a number from ", Q(tok.text));
                    return result;
                }
                ulong value = e.value;
                if(minuses & 1)
                    value = -value;
                result.kind = IMMEDIATE;
                result.immediate = value;
                return result;
            }
            case NUMBER:{
                // Check if this is a float literal
                if(is_float_literal(tok.text)){
                    FloatResult f = parse_float(tok.text);
                    if(f.errored){
                        err_print(tok, "Unable to parse a float from ", Q(tok.text));
                        return result;
                    }
                    // Bit-cast double to uintptr_t
                    result.immediate = *cast(ulong*)&f.value;
                    result.kind = IMMEDIATE;
                    return result;
                }
                IntegerResult!ulong e = parse_unsigned_human(tok.text);
                if(e.errored){
                    err_print(tok, "Unable to parse a number from ", Q(tok.text));
                    return result;
                }
                result.immediate = e.value;
                result.kind = IMMEDIATE;
                return result;
            }
            case IDENTIFIER:{
                str text = tok.text;
                if(uintptr_t* val = cast(string)text in *ConstantsTable){
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
                if(immutable(RegisterInfo)* ri = get_register_info(text)){
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
                    // if(kind == FUNCTION){
                        Token fake = tok;
                        for(;;){
                            if(tokenizer.current_token.type == DOT){
                                tok = tokenizer.current_token_and_advance;
                                fake.length += tok.length;
                                if(tokenizer.current_token.type == IDENTIFIER){
                                    tok = tokenizer.current_token_and_advance;
                                    fake.length += tok.length;
                                    continue;
                                }
                                else {
                                    err_print(tok, "Expected an identifier");
                                    return;
                                }
                            }
                            break;
                        }
                        result.kind = kind;
                        result.function_name = fake.text;
                    // }
                    // else {
                        // result.kind = kind;
                        // these all pun, so whatever
                        // result.function_name = tok.text;
                        // return;
                    // }
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
                    case "embed":
                        // Parse: embed "path" offset length
                        tok = tokenizer.current_token_and_advance;
                        while(tok.type == SPACE || tok.type == TAB)
                            tok = tokenizer.current_token_and_advance;
                        // Parse path string
                        if(tok.type != QUOTATION && tok.type != APOSTROPHE){
                            err_print(tok, "expected string path after embed");
                            return result;
                        }
                        TokenType q = tok.type;
                        const char * before = tok.text.ptr;
                        bool backslash = false;
                        tok = tokenizer.current_token_and_advance;
                        for(;;){
                            if(tok.type == q && !backslash)
                                break;
                            if(tok.type == BACKSLASH)
                                backslash = !backslash;
                            if(tok.type == NEWLINE || tok.type == EOF){
                                err_print(tok, "bad quotation in embed path, no terminating quote");
                                return result;
                            }
                            if(tok.type != BACKSLASH)
                                backslash = false;
                            tok = tokenizer.current_token_and_advance;
                        }
                        ptrdiff_t length = tok.text.ptr - before;
                        result.embed.path = before[1..length];
                        // Parse offset
                        tok = tokenizer.current_token_and_advance;
                        while(tok.type == SPACE || tok.type == TAB)
                            tok = tokenizer.current_token_and_advance;
                        if(tok.type != NUMBER){
                            err_print(tok, "expected number for offset in embed");
                            return result;
                        }
                        IntegerResult!ulong offset_result = parse_unsigned_human(tok.text);
                        if(offset_result.errored){
                            err_print(tok, "Unable to parse offset from ", Q(tok.text));
                            return result;
                        }
                        result.embed.offset = cast(size_t)offset_result.value;
                        // Parse length
                        tok = tokenizer.current_token_and_advance;
                        while(tok.type == SPACE || tok.type == TAB)
                            tok = tokenizer.current_token_and_advance;
                        if(tok.type != NUMBER){
                            err_print(tok, "expected number for length in embed");
                            return result;
                        }
                        IntegerResult!ulong len_result = parse_unsigned_human(tok.text);
                        if(len_result.errored){
                            err_print(tok, "Unable to parse length from ", Q(tok.text));
                            return result;
                        }
                        result.embed.length = cast(size_t)len_result.value;
                        result.kind = EMBED;
                        return result;
                    default:
                        break;
                }
                return result;
            }
            default:
                err_print(tok, "Unable to match ", Q(tok.text), " to any valid argument type.");
                return result;
        }
    }
    }

    int parse_dlimport(DlimportDecl* decl){with(AsmError)with(TokenType){
        Token tok;
        // Parse library paths and function specs until "end"
        for(;;){
            tok = tokenizer.current_token_and_advance;
            while(tok.type == SPACE || tok.type == TAB || tok.type == NEWLINE)
                tok = tokenizer.current_token_and_advance;
            tok = tokenizer.skip_comment(tok);
            if(tok.type == NEWLINE)
                continue;
            if(tok.type == EOF){
                err_print(tok, "unexpected EOF in dlimport block");
                return PARSE_ERROR;
            }
            if(tok.type != IDENTIFIER){
                err_print(tok, "expected one of 'path', 'var', 'function', or 'end'");
                return PARSE_ERROR;
            }
            if(tok.text == "end")
                break;
            // Check for library path
            if(tok.text == "path"){
                tok = tokenizer.current_token_and_advance;
                if(tok.type != SPACE){
                    err_print(tok, "path must be followed by a space: ", Q(tok.text));
                    return PARSE_ERROR;
                }
                tok = tokenizer.current_token_and_advance;
                if(tok.type != QUOTATION){
                    err_print(tok, "path must be followed by a quoted path: ", Q(tok.text));
                    return PARSE_ERROR;
                }
                const(char)* lib_start = tok._text + 1; // skip opening quote
                tok = tokenizer.current_token_and_advance;
                while(tok.type != QUOTATION && tok.type != NEWLINE && tok.type != EOF){
                    tok = tokenizer.current_token_and_advance;
                }
                if(tok.type != QUOTATION){
                    err_print(tok, "unterminated library path string");
                    return PARSE_ERROR;
                }
                decl.library_paths.push(lib_start[0 .. tok._text - lib_start]);
                tok = tokenizer.current_token_and_advance;
            }
            else if(tok.text == "var"){
                tok = tokenizer.current_token_and_advance;
                while(tok.type == SPACE || tok.type == TAB)
                    tok = tokenizer.current_token_and_advance;
                if(tok.type != IDENTIFIER){
                    err_print(tok, "expected identifier after 'var': ", Q(tok.text));
                    return PARSE_ERROR;
                }
                decl.objs ~= DlimportObjSpec(tok.text);
                tok = tokenizer.current_token_and_advance;
            }
            else if(tok.text == "function"){
                tok = tokenizer.current_token_and_advance;
                while(tok.type == SPACE || tok.type == TAB)
                    tok = tokenizer.current_token_and_advance;
                if(tok.type != IDENTIFIER){
                    err_print(tok, "expected identifier after 'function': ", Q(tok.text));
                    return PARSE_ERROR;
                }
                DlimportFuncSpec spec = {name:tok.text};
                tok = tokenizer.current_token_and_advance;
                while(tok.type == SPACE || tok.type == TAB)
                    tok = tokenizer.current_token_and_advance;
                if(tok.type != NUMBER){
                    err_print(tok, "expected argument count: ", Q(tok.text));
                    return PARSE_ERROR;
                }
                IntegerResult!ulong n_args_res = parse_unsigned_human(tok.text);
                if(n_args_res.errored || n_args_res.value > 255){
                    err_print(tok, "invalid argument count: ", Q(tok.text));
                    return PARSE_ERROR;
                }
                spec.n_args = cast(ubyte)n_args_res.value;
                tok = tokenizer.current_token_and_advance;
                while(tok.type == SPACE || tok.type == TAB)
                    tok = tokenizer.current_token_and_advance;
                if(tok.type != NUMBER){
                    err_print(tok, "expected return count");
                    return PARSE_ERROR;
                }
                IntegerResult!ulong n_ret_res = parse_unsigned_human(tok.text);
                if(n_ret_res.errored || n_ret_res.value > 2){
                    err_print(tok, "invalid return count (must be 0, 1, or 2): ", Q(tok.text));
                    return PARSE_ERROR;
                }
                spec.n_ret = cast(ubyte)n_ret_res.value;
                tok = tokenizer.current_token_and_advance;
                // Check for optional "varargs" marker (peek ahead)
                // Skip spaces to peek at next token
                while(tokenizer.current_token.type == SPACE || tokenizer.current_token.type == TAB)
                    tokenizer.advance;
                if(tokenizer.current_token.type == IDENTIFIER && tokenizer.current_token.text == "varargs"){
                    spec.is_varargs = true;
                    tokenizer.advance; // consume "varargs"
                }
                // Check for optional arg_types mask (integer immediate)
                while(tokenizer.current_token.type == SPACE || tokenizer.current_token.type == TAB)
                    tokenizer.advance;
                if(tokenizer.current_token.type == NUMBER){
                    IntegerResult!ulong arg_types_res = parse_unsigned_human(tokenizer.current_token.text);
                    if(!arg_types_res.errored){
                        spec.arg_types = cast(uint)arg_types_res.value;
                        tokenizer.advance;
                        // Check for optional ret_types mask
                        while(tokenizer.current_token.type == SPACE || tokenizer.current_token.type == TAB)
                            tokenizer.advance;
                        if(tokenizer.current_token.type == NUMBER){
                            IntegerResult!ulong ret_types_res = parse_unsigned_human(tokenizer.current_token.text);
                            if(!ret_types_res.errored){
                                spec.ret_types = cast(uint)ret_types_res.value;
                                tokenizer.advance;
                            }
                        }
                    }
                }
                // Check for optional struct_args [size0 size1 ...]
                while(tokenizer.current_token.type == SPACE || tokenizer.current_token.type == TAB)
                    tokenizer.advance;
                if(tokenizer.current_token.type == IDENTIFIER && tokenizer.current_token.text == "struct_args"){
                    tokenizer.advance;
                    while(tokenizer.current_token.type == SPACE || tokenizer.current_token.type == TAB)
                        tokenizer.advance;
                    if(tokenizer.current_token.type != LEFTSQUAREBRACKET){
                        err_print(tokenizer.current_token, "expected '[' after struct_args");
                        return PARSE_ERROR;
                    }
                    tokenizer.advance;
                    // Initialize allocator for struct_arg_sizes
                    spec.struct_arg_sizes.bdata.allocator = allocator;
                    // Parse sizes until ']'
                    while(tokenizer.current_token.type != RIGHTSQUAREBRACKET){
                        while(tokenizer.current_token.type == SPACE || tokenizer.current_token.type == TAB)
                            tokenizer.advance;
                        if(tokenizer.current_token.type == RIGHTSQUAREBRACKET)
                            break;
                        if(tokenizer.current_token.type != NUMBER){
                            err_print(tokenizer.current_token, "expected number in struct_args list");
                            return PARSE_ERROR;
                        }
                        IntegerResult!ulong size_res = parse_unsigned_human(tokenizer.current_token.text);
                        if(size_res.errored || size_res.value > ushort.max){
                            err_print(tokenizer.current_token, "invalid struct size: ", Q(tokenizer.current_token.text));
                            return PARSE_ERROR;
                        }
                        spec.struct_arg_sizes.push(cast(ushort)size_res.value);
                        tokenizer.advance;
                    }
                    tokenizer.advance; // consume ']'
                }
                decl.funcs.push(spec);
            }
            else {
                err_print(tok, "expected one of 'path', 'var', 'function', or 'end'");
                return PARSE_ERROR;
            }
            while(tok.type == SPACE || tok.type == TAB)
                tok = tokenizer.current_token_and_advance;
            tok = tokenizer.skip_comment(tok);
            if(tok.type != NEWLINE && tok.type != EOF){
                err_print(tok, "trailing tokens: ", Q(tok.text));
                return PARSE_ERROR;
            }
        }
        return 0;
    }}
}

Table!(string, uintptr_t)*
ConstantsTable(){
    __gshared bool initialized;
    alias TableT = Table!(string, uintptr_t);
    __gshared TableT constants_table;
    if(!initialized){
        initialized = true;
        constants_table.data.allocator = MALLOCATOR;
        foreach(ii; INSTRUCTION_INFOS){
            constants_table[ii.NAME] = ii.instruction;
        }
        foreach(ri; registerinfos){
            constants_table[ri.NAME] = ri.register;
        }
        // Probably should remove now that we have pointer-sized
        // literals, so you can just write 0p1.
        constants_table["PTRSIZE"] = (void*).sizeof;
        constants_table["USIZE"] = (uintptr_t).sizeof;
    }
    return &constants_table;
}
