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

    // Preprocessor
    HASH = '#',
    PRAGMA = 800,

    // Special
    ELLIPSIS = 900,  // ...
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

    int convert() {
        while (pos < input.length) {
            PPToken pp = input[pos];

            // Skip whitespace and newlines
            if (pp.type == PPTokenType.PP_WHITESPACE ||
                pp.type == PPTokenType.PP_NEWLINE) {
                pos++;
                continue;
            }

            // EOF
            if (pp.type == PPTokenType.PP_EOF) {
                add_token(CTokenType.EOF, "", pp);
                pos++;
                continue;
            }

            // String literal - handle concatenation
            if (pp.type == PPTokenType.PP_STRING) {
                convert_string();
                continue;
            }

            // Character literal
            if (pp.type == PPTokenType.PP_CHAR) {
                add_token(CTokenType.CHAR_LITERAL, pp.lexeme, pp);
                pos++;
                continue;
            }

            // Number
            if (pp.type == PPTokenType.PP_NUMBER) {
                convert_number(pp);
                pos++;
                continue;
            }

            // Identifier - check for keywords
            if (pp.type == PPTokenType.PP_IDENTIFIER) {
                CTokenType keyword = check_keyword(pp.lexeme);
                add_token(keyword, pp.lexeme, pp);
                pos++;
                continue;
            }

            // Punctuator
            if (pp.type == PPTokenType.PP_PUNCTUATOR) {
                CTokenType punct = convert_punctuator(pp.lexeme);
                add_token(punct, pp.lexeme, pp);
                pos++;
                continue;
            }

            // Unknown - skip
            pos++;
        }

        // Add EOF if not already present
        if (output.count == 0 || (*output)[output.count - 1].type != CTokenType.EOF) {
            CToken eof;
            eof.type = CTokenType.EOF;
            eof.lexeme = "";
            *output ~= eof;
        }

        return 0;
    }

    void add_token(CTokenType type, str lexeme, PPToken source) {
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
    void convert_string() {
        StringBuilder sb;
        sb.allocator = allocator;

        PPToken first = input[pos];
        int start_line = first.line;
        int start_column = first.column;
        str start_file = first.file;

        while (pos < input.length) {
            PPToken pp = input[pos];

            if (pp.type == PPTokenType.PP_STRING) {
                // Remove quotes and append content
                if (pp.lexeme.length >= 2) {
                    sb.write(pp.lexeme[1 .. $ - 1]);
                }
                pos++;
            } else if (pp.type == PPTokenType.PP_WHITESPACE ||
                       pp.type == PPTokenType.PP_NEWLINE) {
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

    // Convert pp-number to NUMBER or HEX
    void convert_number(PPToken pp) {
        str s = pp.lexeme;
        CTokenType type = CTokenType.NUMBER;

        // Check for hex prefix
        if (s.length >= 2 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
            type = CTokenType.HEX;
        }

        add_token(type, s, pp);
    }

    // Check if identifier is a keyword
    CTokenType check_keyword(str name) {
        // Keywords sorted by length for efficient matching
        if (name.length == 2) {
            if (str_eq(name, "if")) return CTokenType.IF;
            if (str_eq(name, "do")) return CTokenType.DO;
        }
        if (name.length == 3) {
            if (str_eq(name, "int")) return CTokenType.INT;
            if (str_eq(name, "for")) return CTokenType.FOR;
            if (str_eq(name, "asm")) return CTokenType.ASM;
        }
        if (name.length == 4) {
            if (str_eq(name, "void")) return CTokenType.VOID;
            if (str_eq(name, "char")) return CTokenType.CHAR;
            if (str_eq(name, "long")) return CTokenType.LONG;
            if (str_eq(name, "else")) return CTokenType.ELSE;
            if (str_eq(name, "case")) return CTokenType.CASE;
            if (str_eq(name, "enum")) return CTokenType.ENUM;
            if (str_eq(name, "goto")) return CTokenType.GOTO;
            if (str_eq(name, "auto")) return CTokenType.AUTO;
            if (str_eq(name, "bool")) return CTokenType.BOOL;
            if (str_eq(name, "true")) return CTokenType.TRUE_KW;
        }
        if (name.length == 5) {
            if (str_eq(name, "short")) return CTokenType.SHORT;
            if (str_eq(name, "float")) return CTokenType.FLOAT;
            if (str_eq(name, "while")) return CTokenType.WHILE;
            if (str_eq(name, "break")) return CTokenType.BREAK;
            if (str_eq(name, "const")) return CTokenType.CONST;
            if (str_eq(name, "union")) return CTokenType.UNION;
            if (str_eq(name, "__asm")) return CTokenType.ASM;
            if (str_eq(name, "_Bool")) return CTokenType.BOOL;
            if (str_eq(name, "false")) return CTokenType.FALSE_KW;
        }
        if (name.length == 6) {
            if (str_eq(name, "double")) return CTokenType.DOUBLE;
            if (str_eq(name, "return")) return CTokenType.RETURN;
            if (str_eq(name, "switch")) return CTokenType.SWITCH;
            if (str_eq(name, "static")) return CTokenType.STATIC;
            if (str_eq(name, "extern")) return CTokenType.EXTERN;
            if (str_eq(name, "struct")) return CTokenType.STRUCT;
            if (str_eq(name, "sizeof")) return CTokenType.SIZEOF;
            if (str_eq(name, "signed")) return CTokenType.SIGNED;
            if (str_eq(name, "inline")) return CTokenType.INLINE;
        }
        if (name.length == 7) {
            if (str_eq(name, "typedef")) return CTokenType.TYPEDEF;
            if (str_eq(name, "default")) return CTokenType.DEFAULT;
            if (str_eq(name, "__asm__")) return CTokenType.ASM;
            if (str_eq(name, "_Atomic")) return CTokenType.ATOMIC;
            if (str_eq(name, "alignof")) return CTokenType.ALIGNOF;
        }
        if (name.length == 8) {
            if (str_eq(name, "unsigned")) return CTokenType.UNSIGNED;
            if (str_eq(name, "volatile")) return CTokenType.VOLATILE;
            if (str_eq(name, "continue")) return CTokenType.CONTINUE;
            if (str_eq(name, "_Alignof")) return CTokenType.ALIGNOF;
            if (str_eq(name, "_Countof")) return CTokenType.COUNTOF;
            if (str_eq(name, "noreturn")) return CTokenType.NORETURN;
            if (str_eq(name, "register")) return CTokenType.REGISTER;
            if (str_eq(name, "restrict")) return CTokenType.RESTRICT;
            if (str_eq(name, "_Float16")) return CTokenType.FLOAT16;
            if (str_eq(name, "__inline")) return CTokenType.INLINE;
            if (str_eq(name, "_Complex")) return CTokenType.COMPLEX;
            if (str_eq(name, "_Generic")) return CTokenType.GENERIC;
        }
        if (name.length == 9) {
            if (str_eq(name, "_Noreturn")) return CTokenType.NORETURN;
        }
        if (name.length == 10) {
            if (str_eq(name, "__restrict")) return CTokenType.RESTRICT;
            if (str_eq(name, "__inline__")) return CTokenType.INLINE;
            if (str_eq(name, "__int128_t")) return CTokenType.INT128;
            if (str_eq(name, "_Decimal32")) return CTokenType.DECIMAL32;
            if (str_eq(name, "_Decimal64")) return CTokenType.DECIMAL64;
        }
        if (name.length == 11) {
            if (str_eq(name, "__uint128_t")) return CTokenType.UINT128;
            if (str_eq(name, "_Decimal128")) return CTokenType.DECIMAL128;
        }
        if (name.length == 12) {
            if (str_eq(name, "__restrict__")) return CTokenType.RESTRICT;
        }
        if (name.length == 14) {
            if (str_eq(name, "_Static_assert")) return CTokenType.STATIC_ASSERT;
            if (str_eq(name, "static_assert")) return CTokenType.STATIC_ASSERT;
        }

        return CTokenType.IDENTIFIER;
    }

    // Convert punctuator to CTokenType
    CTokenType convert_punctuator(str p) {
        if (p.length == 1) {
            switch (p[0]) {
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
        if (p.length == 2) {
            if (str_eq(p, "==")) return CTokenType.EQUAL_EQUAL;
            if (str_eq(p, "!=")) return CTokenType.BANG_EQUAL;
            if (str_eq(p, "<=")) return CTokenType.LESS_EQUAL;
            if (str_eq(p, ">=")) return CTokenType.GREATER_EQUAL;
            if (str_eq(p, "<<")) return CTokenType.LESS_LESS;
            if (str_eq(p, ">>")) return CTokenType.GREATER_GREATER;
            if (str_eq(p, "&&")) return CTokenType.AMP_AMP;
            if (str_eq(p, "||")) return CTokenType.PIPE_PIPE;
            if (str_eq(p, "++")) return CTokenType.PLUS_PLUS;
            if (str_eq(p, "--")) return CTokenType.MINUS_MINUS;
            if (str_eq(p, "->")) return CTokenType.ARROW;
            if (str_eq(p, "+=")) return CTokenType.PLUS_EQUAL;
            if (str_eq(p, "-=")) return CTokenType.MINUS_EQUAL;
            if (str_eq(p, "*=")) return CTokenType.STAR_EQUAL;
            if (str_eq(p, "/=")) return CTokenType.SLASH_EQUAL;
            if (str_eq(p, "%=")) return CTokenType.PERCENT_EQUAL;
            if (str_eq(p, "&=")) return CTokenType.AMP_EQUAL;
            if (str_eq(p, "|=")) return CTokenType.PIPE_EQUAL;
            if (str_eq(p, "^=")) return CTokenType.CARET_EQUAL;
            if (str_eq(p, "##")) return CTokenType.HASH;  // Token paste (shouldn't appear after preprocess)
        }
        if (p.length == 3) {
            if (str_eq(p, "...")) return CTokenType.ELLIPSIS;
            if (str_eq(p, "<<=")) return CTokenType.LESS_LESS_EQUAL;
            if (str_eq(p, ">>=")) return CTokenType.GREATER_GREATER_EQUAL;
        }

        // Unknown punctuator - return as-is (should not happen)
        return CTokenType.ERROR;
    }
}

// Convert PPToken array to CToken array
int pp_to_c_tokens(PPToken[] input, Barray!CToken* output, Allocator allocator) {
    PPToCConverter converter;
    converter.allocator = allocator;
    converter.input = input;
    converter.output = output;
    return converter.convert();
}
