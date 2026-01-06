/*
 * PPToken to CToken Converter (Phase 5+)
 * Converts preprocessed tokens to C tokens for parsing
 * Copyright 2025, David Priver
 */
module cfront.c_pp_to_c;

import dlib.aliases;
import dlib.allocator : Allocator;
import dlib.barray : Barray, make_barray;
import dlib.stringbuilder : StringBuilder;
import cfront.c_pp_token;

// Import CTokenType from the tokenizer module
// We'll define it here to avoid circular imports
enum CTokenType : uint {
    // Single character tokens
    LEFT_PAREN = '(', RIGHT_PAREN = ')', LEFT_BRACE = '{', RIGHT_BRACE = '}',
    LEFT_BRACKET = '[', RIGHT_BRACKET = ']',
    COMMA = ',', DOT = '.', SEMICOLON = ';', COLON = ':', QUESTION = '?',
    PLUS = '+', MINUS = '-', STAR = '*', SLASH = '/', PERCENT = '%',
    AMP = '&', PIPE = '|', CARET = '^', TILDE = '~',

    // One or two character tokens
    BANG = '!', BANG_EQUAL = BANG + 127,
    EQUAL = '=', EQUAL_EQUAL = EQUAL + 127,
    GREATER = '>', GREATER_EQUAL = GREATER + 127,
    LESS = '<', LESS_EQUAL = LESS + 127,
    LESS_LESS = LESS + 256,      // <<
    GREATER_GREATER = GREATER + 256, // >>
    AMP_AMP = AMP + 256,         // &&
    PIPE_PIPE = PIPE + 256,      // ||
    ARROW = MINUS + 256,         // ->
    PLUS_PLUS = PLUS + 256,      // ++
    MINUS_MINUS = MINUS + 127,   // --

    // Compound assignment
    PLUS_EQUAL = PLUS + 127,
    MINUS_EQUAL = 400,
    STAR_EQUAL = STAR + 127,
    SLASH_EQUAL = SLASH + 127,
    PERCENT_EQUAL = PERCENT + 127,
    AMP_EQUAL = AMP + 127,
    PIPE_EQUAL = PIPE + 127,
    CARET_EQUAL = CARET + 127,
    LESS_LESS_EQUAL = 401,      // <<=
    GREATER_GREATER_EQUAL = 402, // >>=

    // Literals
    IDENTIFIER = 500,
    NUMBER = 501,
    HEX = 502,
    STRING = 503,
    CHAR_LITERAL = 504,
    FLOAT_LITERAL = 505,

    // Type keywords
    VOID = 600,
    INT = 601,
    CHAR = 602,
    LONG = 603,
    SHORT = 604,
    FLOAT = 605,
    DOUBLE = 606,
    UNSIGNED = 607,
    SIGNED = 608,
    CONST = 609,
    VOLATILE = 610,
    STATIC = 611,
    EXTERN = 612,
    STRUCT = 613,
    UNION = 614,
    ENUM = 615,
    TYPEDEF = 616,
    SIZEOF = 617,
    STATIC_ASSERT = 618,
    ASM = 619,
    FLOAT16 = 620,
    REGISTER = 621,
    RESTRICT = 622,
    BOOL = 623,
    AUTO = 624,
    ATOMIC = 625,
    INLINE = 626,
    INT128 = 627,
    UINT128 = 628,
    NORETURN = 629,
    ALIGNOF = 630,
    COUNTOF = 631,
    TRUE_KW = 632,
    FALSE_KW = 633,
    COMPLEX = 634,
    DECIMAL32 = 635,
    DECIMAL64 = 636,
    DECIMAL128 = 637,
    GENERIC = 638,
    FLOAT32 = 639,
    FLOAT64 = 640,
    FLOAT128 = 641,
    FLOAT32X = 642,
    FLOAT64X = 643,
    TYPEOF = 644,
    CONSTEXPR = 645,

    // Control flow keywords
    IF = 700,
    ELSE = 701,
    WHILE = 702,
    FOR = 703,
    DO = 704,
    SWITCH = 705,
    CASE = 706,
    DEFAULT = 707,
    BREAK = 708,
    CONTINUE = 709,
    RETURN = 710,
    GOTO = 711,
    DASM = 712,

    // Preprocessor
    HASH = '#',
    PRAGMA = 800,

    // Special
    ELLIPSIS = 900,  // ...
    FUNC = 901,
    EOF = 0,
    ERROR = 1,
}

struct CToken {
    CTokenType type;
    str lexeme;
    int line;
    int column;
    str file;
    // Expansion location (where macro was invoked, if from macro expansion)
    int expansion_line;
    int expansion_column;
    str expansion_file;
}

struct PPToCConverter {
    Allocator allocator;
    PPToken[] input;
    Barray!CToken* output;
    size_t pos = 0;

    int convert(){
        while(pos < input.length){
            PPToken pp = input[pos];

            // Skip whitespace and newlines
            if(pp.type == PPTokenType.PP_WHITESPACE ||
                pp.type == PPTokenType.PP_NEWLINE){
                pos++;
                continue;
            }

            // EOF
            if(pp.type == PPTokenType.PP_EOF){
                add_token(CTokenType.EOF, "", pp);
                pos++;
                continue;
            }

            // String literal - handle concatenation
            if(pp.type == PPTokenType.PP_STRING){
                convert_string();
                continue;
            }

            // Character literal
            if(pp.type == PPTokenType.PP_CHAR){
                add_token(CTokenType.CHAR_LITERAL, pp.lexeme, pp);
                pos++;
                continue;
            }

            // Number
            if(pp.type == PPTokenType.PP_NUMBER){
                convert_number(pp);
                pos++;
                continue;
            }

            // Identifier - check for keywords
            if(pp.type == PPTokenType.PP_IDENTIFIER){
                CTokenType keyword = check_keyword(pp.lexeme);
                add_token(keyword, pp.lexeme, pp);
                pos++;
                continue;
            }

            // Punctuator
            if(pp.type == PPTokenType.PP_PUNCTUATOR){
                CTokenType punct = convert_punctuator(pp.lexeme);
                add_token(punct, pp.lexeme, pp);
                pos++;
                continue;
            }

            // Unknown - skip
            pos++;
        }

        // Add EOF if not already present
        if(output.count == 0 || (*output)[output.count - 1].type != CTokenType.EOF){
            CToken eof;
            eof.type = CTokenType.EOF;
            eof.lexeme = "";
            *output ~= eof;
        }

        return 0;
    }

    void add_token(CTokenType type, str lexeme, PPToken source){
        CToken tok;
        tok.type = type;
        tok.lexeme = lexeme;
        tok.line = source.line;
        tok.column = source.column;
        tok.file = source.file;
        tok.expansion_line = source.expansion_line;
        tok.expansion_column = source.expansion_column;
        tok.expansion_file = source.expansion_file;
        *output ~= tok;
    }

    // Concatenate adjacent string literals
    void convert_string(){
        StringBuilder sb;
        sb.allocator = allocator;

        PPToken first = input[pos];
        int start_line = first.line;
        int start_column = first.column;
        str start_file = first.file;

        while(pos < input.length){
            PPToken pp = input[pos];

            if(pp.type == PPTokenType.PP_STRING){
                // Remove quotes and append content
                if(pp.lexeme.length >= 2){
                    sb.write(pp.lexeme[1 .. $ - 1]);
                }
                pos++;
            } else if(pp.type == PPTokenType.PP_WHITESPACE ||
                       pp.type == PPTokenType.PP_NEWLINE){
                // Skip whitespace between strings
                pos++;
            } else {
                break;
            }
        }

        // Create combined string token
        StringBuilder result;
        result.allocator = allocator;
        result.write('"');
        result.write(sb.borrow());
        result.write('"');

        CToken tok;
        tok.type = CTokenType.STRING;
        tok.lexeme = result.borrow();
        tok.line = start_line;
        tok.column = start_column;
        tok.file = start_file;
        *output ~= tok;
    }

    // Convert pp-number to NUMBER, HEX, or FLOAT_LITERAL
    void convert_number(PPToken pp){
        str s = pp.lexeme;
        CTokenType type = CTokenType.NUMBER;

        // Check for hex prefix
        if(s.length >= 2 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X')){
            type = CTokenType.HEX;
        }
        else {
            // Check for float literal (contains '.' or 'e/E')
            bool has_dot = false;
            bool has_exp = false;
            foreach(c; s){
                if(c == '.') has_dot = true;
                if(c == 'e' || c == 'E') has_exp = true;
            }
            if(has_dot || has_exp){
                type = CTokenType.FLOAT_LITERAL;
            }
        }

        add_token(type, s, pp);
    }

    // Check if identifier is a keyword
    CTokenType check_keyword(str name){
        // Keywords sorted by length for efficient matching
        switch(name.length){
            case 2:{
                if(name == "if") return CTokenType.IF;
                if(name == "do") return CTokenType.DO;
                break;
            }
            case 3:{
                if(name == "int") return CTokenType.INT;
                if(name == "for") return CTokenType.FOR;
                if(name == "asm") return CTokenType.ASM;
                break;
            }
            case 4:{
                if(name == "void") return CTokenType.VOID;
                if(name == "char") return CTokenType.CHAR;
                if(name == "long") return CTokenType.LONG;
                if(name == "else") return CTokenType.ELSE;
                if(name == "case") return CTokenType.CASE;
                if(name == "enum") return CTokenType.ENUM;
                if(name == "goto") return CTokenType.GOTO;
                if(name == "dasm") return CTokenType.DASM;
                if(name == "auto") return CTokenType.AUTO;
                if(name == "bool") return CTokenType.BOOL;
                if(name == "true") return CTokenType.TRUE_KW;
                break;
            }
            case 5:{
                if(name == "short") return CTokenType.SHORT;
                if(name == "float") return CTokenType.FLOAT;
                if(name == "while") return CTokenType.WHILE;
                if(name == "break") return CTokenType.BREAK;
                if(name == "const") return CTokenType.CONST;
                if(name == "union") return CTokenType.UNION;
                if(name == "__asm") return CTokenType.ASM;
                if(name == "_Bool") return CTokenType.BOOL;
                if(name == "false") return CTokenType.FALSE_KW;
                break;
            }
            case 6:{
                if(name == "double") return CTokenType.DOUBLE;
                if(name == "return") return CTokenType.RETURN;
                if(name == "switch") return CTokenType.SWITCH;
                if(name == "static") return CTokenType.STATIC;
                if(name == "extern") return CTokenType.EXTERN;
                if(name == "struct") return CTokenType.STRUCT;
                if(name == "sizeof") return CTokenType.SIZEOF;
                if(name == "signed") return CTokenType.SIGNED;
                if(name == "inline") return CTokenType.INLINE;
                if(name == "__dasm") return CTokenType.DASM;
                if(name == "typeof") return CTokenType.TYPEOF;
                break;
            }
            case 7:{
                if(name == "typedef") return CTokenType.TYPEDEF;
                if(name == "default") return CTokenType.DEFAULT;
                if(name == "__asm__") return CTokenType.ASM;
                if(name == "_Atomic") return CTokenType.ATOMIC;
                if(name == "alignof") return CTokenType.ALIGNOF;
                break;
            }
            case 8:{
                if(name == "unsigned") return CTokenType.UNSIGNED;
                if(name == "volatile") return CTokenType.VOLATILE;
                if(name == "continue") return CTokenType.CONTINUE;
                if(name == "_Alignof") return CTokenType.ALIGNOF;
                if(name == "_Countof") return CTokenType.COUNTOF;
                if(name == "noreturn") return CTokenType.NORETURN;
                if(name == "register") return CTokenType.REGISTER;
                if(name == "restrict") return CTokenType.RESTRICT;
                if(name == "_Float16") return CTokenType.FLOAT16;
                if(name == "_Float32") return CTokenType.FLOAT32;
                if(name == "__int128") return CTokenType.INT128;
                if(name == "_Float64") return CTokenType.FLOAT64;
                if(name == "__inline") return CTokenType.INLINE;
                if(name == "_Complex") return CTokenType.COMPLEX;
                if(name == "_Generic") return CTokenType.GENERIC;
                if(name == "__dasm__") return CTokenType.DASM;
                if(name == "__func__") return CTokenType.FUNC;
                break;
            }
            case 9:{
                if(name == "_Noreturn") return CTokenType.NORETURN;
                if(name == "_Float128") return CTokenType.FLOAT128;
                if(name == "_Float32x") return CTokenType.FLOAT32X;
                if(name == "_Float64x") return CTokenType.FLOAT64X;
                if(name == "constexpr") return CTokenType.CONSTEXPR;
                break;
            }
            case 10:{
                if(name == "__restrict") return CTokenType.RESTRICT;
                if(name == "__inline__") return CTokenType.INLINE;
                if(name == "__int128_t") return CTokenType.INT128;
                if(name == "_Decimal32") return CTokenType.DECIMAL32;
                if(name == "_Decimal64") return CTokenType.DECIMAL64;
                if(name == "__typeof__") return CTokenType.TYPEOF;
                break;
            }
            case 11:{
                if(name == "__uint128_t") return CTokenType.UINT128;
                if(name == "_Decimal128") return CTokenType.DECIMAL128;
                break;
            }
            case 12:{
                if(name == "__restrict__") return CTokenType.RESTRICT;
                if(name == "__FUNCTION__") return CTokenType.FUNC;
                break;
            }
            case 13:{
                if(name == "static_assert") return CTokenType.STATIC_ASSERT;
                break;
            }
            case 14:{
                if(name == "_Static_assert") return CTokenType.STATIC_ASSERT;
                break;
            }
            case 19:{
                if(name == "__PRETTY_FUNCTION__") return CTokenType.FUNC;
                break;
            }
            default:
                break;
        }

        return CTokenType.IDENTIFIER;
    }

    // Convert punctuator to CTokenType
    CTokenType convert_punctuator(str p){
        if(p.length == 1){
            switch(p[0]){
                case '(': return CTokenType.LEFT_PAREN;
                case ')': return CTokenType.RIGHT_PAREN;
                case '{': return CTokenType.LEFT_BRACE;
                case '}': return CTokenType.RIGHT_BRACE;
                case '[': return CTokenType.LEFT_BRACKET;
                case ']': return CTokenType.RIGHT_BRACKET;
                case ',': return CTokenType.COMMA;
                case '.': return CTokenType.DOT;
                case ';': return CTokenType.SEMICOLON;
                case ':': return CTokenType.COLON;
                case '?': return CTokenType.QUESTION;
                case '+': return CTokenType.PLUS;
                case '-': return CTokenType.MINUS;
                case '*': return CTokenType.STAR;
                case '/': return CTokenType.SLASH;
                case '%': return CTokenType.PERCENT;
                case '&': return CTokenType.AMP;
                case '|': return CTokenType.PIPE;
                case '^': return CTokenType.CARET;
                case '~': return CTokenType.TILDE;
                case '!': return CTokenType.BANG;
                case '=': return CTokenType.EQUAL;
                case '<': return CTokenType.LESS;
                case '>': return CTokenType.GREATER;
                case '#': return CTokenType.HASH;
                default: break;
            }
        }
        if(p.length == 2){
            if(p == "==") return CTokenType.EQUAL_EQUAL;
            if(p == "!=") return CTokenType.BANG_EQUAL;
            if(p == "<=") return CTokenType.LESS_EQUAL;
            if(p == ">=") return CTokenType.GREATER_EQUAL;
            if(p == "<<") return CTokenType.LESS_LESS;
            if(p == ">>") return CTokenType.GREATER_GREATER;
            if(p == "&&") return CTokenType.AMP_AMP;
            if(p == "||") return CTokenType.PIPE_PIPE;
            if(p == "++") return CTokenType.PLUS_PLUS;
            if(p == "--") return CTokenType.MINUS_MINUS;
            if(p == "->") return CTokenType.ARROW;
            if(p == "+=") return CTokenType.PLUS_EQUAL;
            if(p == "-=") return CTokenType.MINUS_EQUAL;
            if(p == "*=") return CTokenType.STAR_EQUAL;
            if(p == "/=") return CTokenType.SLASH_EQUAL;
            if(p == "%=") return CTokenType.PERCENT_EQUAL;
            if(p == "&=") return CTokenType.AMP_EQUAL;
            if(p == "|=") return CTokenType.PIPE_EQUAL;
            if(p == "^=") return CTokenType.CARET_EQUAL;
            if(p == "##") return CTokenType.HASH;  // Token paste (shouldn't appear after preprocess)
        }
        if(p.length == 3){
            if(p == "...") return CTokenType.ELLIPSIS;
            if(p == "<<=") return CTokenType.LESS_LESS_EQUAL;
            if(p == ">>=") return CTokenType.GREATER_GREATER_EQUAL;
        }

        // Unknown punctuator - return as-is (should not happen)
        return CTokenType.ERROR;
    }
}

// Convert PPToken array to CToken array
int pp_to_c_tokens(PPToken[] input, Barray!CToken* output, Allocator allocator){
    PPToCConverter converter;
    converter.allocator = allocator;
    converter.input = input;
    converter.output = output;
    return converter.convert();
}
