/*
 * C Front-End Tokenizer for ddasm
 * Copyright 2025, David Priver
 */
module cfront.c_tokenizer;

import core.stdc.stdio : fprintf, stderr;
import dlib.aliases;
import dlib.allocator : Allocator;
import dlib.barray : Barray, make_barray;
import dlib.table : Table;

enum CTokenType : uint {
    // Single character tokens
    LEFT_PAREN = '(', RIGHT_PAREN = ')', LEFT_BRACE = '{', RIGHT_BRACE = '}',
    LEFT_BRACKET = '[', RIGHT_BRACKET = ']',
    COMMA = ',', DOT = '.', SEMICOLON = ';', COLON = ':',
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
    MINUS_EQUAL = 400,  // Can't use MINUS + 127, already used
    STAR_EQUAL = STAR + 127,
    SLASH_EQUAL = SLASH + 127,
    PERCENT_EQUAL = PERCENT + 127,
    AMP_EQUAL = AMP + 127,
    PIPE_EQUAL = PIPE + 127,
    CARET_EQUAL = CARET + 127,

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
    STATIC = 610,
    EXTERN = 611,
    STRUCT = 612,
    UNION = 613,
    ENUM = 614,
    TYPEDEF = 615,
    SIZEOF = 616,
    STATIC_ASSERT = 617,

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

    // Preprocessor (minimal support for MVP)
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
}

struct CTokenizer {
    const(ubyte)[] source;
    Barray!CToken* tokens;
    Allocator allocator;
    int start = 0;
    int current = 0;
    int line = 1;
    int column = 1;
    int start_column = 1;

    // Preprocessor defines: name -> replacement text (empty string = expands to nothing)
    Table!(str, str) defines;

    bool ERROR_OCCURRED = false;

    void init() {
        defines.data.allocator = allocator;
    }

    void error(str message) {
        ERROR_OCCURRED = true;
        fprintf(stderr, "[line %d, col %d]: Tokenize Error: %.*s\n",
                line, column, cast(int)message.length, message.ptr);
    }

    bool at_end() {
        return current >= source.length;
    }

    int tokenize_tokens() {
        while (!at_end) {
            start = current;
            start_column = column;
            tokenize_token();
            if (ERROR_OCCURRED) return 1;
        }
        *tokens ~= CToken(CTokenType.EOF, "", line, column);
        return 0;
    }

    void tokenize_token() {
        with (CTokenType) {
            auto c = advance();
            switch (c) {
                case '(':
                case ')':
                case '{':
                case '}':
                case '[':
                case ']':
                case ',':
                case ';':
                case ':':
                case '~':
                    add_token(cast(CTokenType)c);
                    break;

                case '.':
                    // Check for ...
                    if (peek == '.' && peekNext == '.') {
                        advance();
                        advance();
                        add_token(ELLIPSIS);
                    } else {
                        add_token(DOT);
                    }
                    break;

                case '+':
                    if (match('+')) add_token(PLUS_PLUS);
                    else if (match('=')) add_token(PLUS_EQUAL);
                    else add_token(PLUS);
                    break;

                case '-':
                    if (match('-')) add_token(MINUS_MINUS);
                    else if (match('>')) add_token(ARROW);
                    else if (match('=')) add_token(MINUS_EQUAL);
                    else add_token(MINUS);
                    break;

                case '*':
                    add_token(match('=') ? STAR_EQUAL : STAR);
                    break;

                case '%':
                    add_token(match('=') ? PERCENT_EQUAL : PERCENT);
                    break;

                case '^':
                    add_token(match('=') ? CARET_EQUAL : CARET);
                    break;

                case '&':
                    if (match('&')) add_token(AMP_AMP);
                    else if (match('=')) add_token(AMP_EQUAL);
                    else add_token(AMP);
                    break;

                case '|':
                    if (match('|')) add_token(PIPE_PIPE);
                    else if (match('=')) add_token(PIPE_EQUAL);
                    else add_token(PIPE);
                    break;

                case '!':
                    add_token(match('=') ? BANG_EQUAL : BANG);
                    break;

                case '=':
                    add_token(match('=') ? EQUAL_EQUAL : EQUAL);
                    break;

                case '<':
                    if (match('<')) add_token(LESS_LESS);
                    else if (match('=')) add_token(LESS_EQUAL);
                    else add_token(LESS);
                    break;

                case '>':
                    if (match('>')) add_token(GREATER_GREATER);
                    else if (match('=')) add_token(GREATER_EQUAL);
                    else add_token(GREATER);
                    break;

                case '#':
                    // Preprocessor directive - for now, consume until newline
                    do_preprocessor();
                    break;

                case '/':
                    if (match('/')) {
                        // Single-line comment
                        while (peek != '\n' && !at_end) advance();
                    } else if (match('*')) {
                        // Multi-line comment
                        do_block_comment();
                    } else if (match('=')) {
                        add_token(SLASH_EQUAL);
                    } else {
                        add_token(SLASH);
                    }
                    break;

                // Skip whitespace
                case ' ':
                case '\r':
                case '\t':
                    break;

                case '\n':
                    line++;
                    column = 1;
                    break;

                case '\'':
                    do_char_literal();
                    break;

                case '"':
                    do_string();
                    break;

                case '0': .. case '9':
                    do_number(c);
                    break;

                case 'a': .. case 'z':
                case 'A': .. case 'Z':
                case '_':
                    do_identifier();
                    break;

                default:
                    error("Unexpected character");
                    break;
            }
        }
    }

    void do_preprocessor() {
        // Skip whitespace after #
        while (peek == ' ' || peek == '\t') advance();

        // Get directive name
        int directive_start = current;
        while (peek.is_alpha) advance();
        str directive = cast(str)source[directive_start .. current];

        if (directive == "pragma") {
            // Skip to end of line, store as pragma token
            while (peek != '\n' && !at_end) advance();
            add_token(CTokenType.PRAGMA);
        } else if (directive == "define") {
            // #define NAME or #define NAME VALUE
            // Skip whitespace after 'define'
            while (peek == ' ' || peek == '\t') advance();

            // Get macro name
            int name_start = current;
            while (peek.is_ident_char) advance();
            str macro_name = cast(str)source[name_start .. current];

            if (macro_name.length == 0) {
                error("Expected macro name after #define");
                return;
            }

            // Skip whitespace after name
            while (peek == ' ' || peek == '\t') advance();

            // Get replacement text (everything until end of line)
            int value_start = current;
            while (peek != '\n' && !at_end) advance();
            str value = cast(str)source[value_start .. current];

            // Trim trailing whitespace from value
            while (value.length > 0 && (value[$ - 1] == ' ' || value[$ - 1] == '\t')) {
                value = value[0 .. $ - 1];
            }

            // Store in defines table
            defines[macro_name] = value;
        } else {
            // Skip other preprocessor directives
            while (peek != '\n' && !at_end) advance();
        }
    }

    void do_block_comment() {
        while (!at_end) {
            if (peek == '*' && peekNext == '/') {
                advance();
                advance();
                return;
            }
            if (peek == '\n') {
                line++;
                column = 0;
            }
            advance();
        }
        error("Unterminated block comment");
    }

    void do_identifier() {
        while (peek.is_ident_char) advance();
        auto text = cast(str)source[start .. current];

        // Check for macro substitution
        if (str* replacement = text in defines) {
            str value = *replacement;
            if (value.length == 0) {
                // Macro expands to nothing - don't emit any token
                return;
            }
            // Tokenize the replacement text and emit those tokens
            tokenize_macro_replacement(value);
            return;
        }

        // Check for keywords
        CTokenType type = CTokenType.IDENTIFIER;
        switch (text) {
            case "void":     type = CTokenType.VOID; break;
            case "int":      type = CTokenType.INT; break;
            case "char":     type = CTokenType.CHAR; break;
            case "long":     type = CTokenType.LONG; break;
            case "short":    type = CTokenType.SHORT; break;
            case "float":    type = CTokenType.FLOAT; break;
            case "double":   type = CTokenType.DOUBLE; break;
            case "unsigned": type = CTokenType.UNSIGNED; break;
            case "signed":   type = CTokenType.SIGNED; break;
            case "const":    type = CTokenType.CONST; break;
            case "static":   type = CTokenType.STATIC; break;
            case "extern":   type = CTokenType.EXTERN; break;
            case "struct":   type = CTokenType.STRUCT; break;
            case "union":    type = CTokenType.UNION; break;
            case "enum":     type = CTokenType.ENUM; break;
            case "typedef":  type = CTokenType.TYPEDEF; break;
            case "sizeof":          type = CTokenType.SIZEOF; break;
            case "_Static_assert":  type = CTokenType.STATIC_ASSERT; break;
            case "if":              type = CTokenType.IF; break;
            case "else":     type = CTokenType.ELSE; break;
            case "while":    type = CTokenType.WHILE; break;
            case "for":      type = CTokenType.FOR; break;
            case "do":       type = CTokenType.DO; break;
            case "switch":   type = CTokenType.SWITCH; break;
            case "case":     type = CTokenType.CASE; break;
            case "default":  type = CTokenType.DEFAULT; break;
            case "break":    type = CTokenType.BREAK; break;
            case "continue": type = CTokenType.CONTINUE; break;
            case "return":   type = CTokenType.RETURN; break;
            case "goto":     type = CTokenType.GOTO; break;
            default: break;
        }
        add_token(type);
    }

    // Tokenize macro replacement text and emit tokens
    void tokenize_macro_replacement(str text) {
        size_t i = 0;

        while (i < text.length) {
            ubyte c = text[i];

            // Skip whitespace
            if (c == ' ' || c == '\t') {
                i++;
                continue;
            }

            // Single-character tokens
            if (c == '(' || c == ')' || c == '[' || c == ']' ||
                c == '{' || c == '}' || c == ',' || c == ';' ||
                c == '+' || c == '-' || c == '*' || c == '/' ||
                c == '%' || c == '&' || c == '|' || c == '^' ||
                c == '~' || c == '<' || c == '>' || c == '!' ||
                c == '=') {
                *tokens ~= CToken(cast(CTokenType) c, text[i .. i + 1], line, start_column);
                i++;
                continue;
            }

            // Hex number
            if (c == '0' && i + 1 < text.length && (text[i + 1] == 'x' || text[i + 1] == 'X')) {
                size_t num_start = i;
                i += 2;  // skip 0x
                while (i < text.length && text[i].is_hex_digit) i++;
                *tokens ~= CToken(CTokenType.HEX, text[num_start .. i], line, start_column);
                continue;
            }

            // Decimal number
            if (c.is_digit) {
                size_t num_start = i;
                while (i < text.length && text[i].is_digit) i++;
                // Skip suffixes like L, U, UL
                while (i < text.length && (text[i] == 'L' || text[i] == 'l' ||
                                            text[i] == 'U' || text[i] == 'u')) i++;
                *tokens ~= CToken(CTokenType.NUMBER, text[num_start .. i], line, start_column);
                continue;
            }

            // String literal
            if (c == '"') {
                size_t str_start = i;
                i++;  // skip opening "
                while (i < text.length && text[i] != '"') {
                    if (text[i] == '\\' && i + 1 < text.length) i++;  // skip escaped char
                    i++;
                }
                if (i < text.length) i++;  // skip closing "
                *tokens ~= CToken(CTokenType.STRING, text[str_start .. i], line, start_column);
                continue;
            }

            // Char literal
            if (c == '\'') {
                size_t chr_start = i;
                i++;  // skip opening '
                while (i < text.length && text[i] != '\'') {
                    if (text[i] == '\\' && i + 1 < text.length) i++;
                    i++;
                }
                if (i < text.length) i++;  // skip closing '
                *tokens ~= CToken(CTokenType.CHAR_LITERAL, text[chr_start .. i], line, start_column);
                continue;
            }

            // Identifier
            if (c.is_alpha || c == '_') {
                size_t id_start = i;
                while (i < text.length && text[i].is_ident_char) i++;
                str ident = text[id_start .. i];

                // Check for nested macro
                if (str* nested = ident in defines) {
                    if ((*nested).length > 0) {
                        tokenize_macro_replacement(*nested);
                    }
                    // else: empty macro, emit nothing
                } else {
                    *tokens ~= CToken(CTokenType.IDENTIFIER, ident, line, start_column);
                }
                continue;
            }

            // Unknown character - skip it
            i++;
        }
    }

    void do_string() {
        bool backslash = false;
        while ((peek != '"' || backslash) && !at_end) {
            if (peek == '\n') {
                line++;
                column = 0;
            }
            if (peek == '\\') backslash = !backslash;
            else backslash = false;
            advance();
        }
        if (at_end) {
            error("Unterminated string");
            return;
        }
        advance(); // closing "
        add_token(CTokenType.STRING);
    }

    void do_char_literal() {
        bool backslash = false;
        while ((peek != '\'' || backslash) && !at_end) {
            if (peek == '\\') backslash = !backslash;
            else backslash = false;
            advance();
        }
        if (at_end) {
            error("Unterminated character literal");
            return;
        }
        advance(); // closing '
        add_token(CTokenType.CHAR_LITERAL);
    }

    void do_number(ubyte c) {
        if (c == '0') {
            if (peek == 'x' || peek == 'X') {
                advance();
                while (peek.is_hex_digit) advance();
                add_token(CTokenType.HEX);
                return;
            }
        }
        while (peek.is_digit) advance();
        // Handle suffixes like L, U, UL, etc.
        while (peek == 'l' || peek == 'L' || peek == 'u' || peek == 'U') {
            advance();
        }
        add_token(CTokenType.NUMBER);
    }

    bool match(ubyte c) {
        if (at_end) return false;
        if (source[current] != c) return false;
        current++;
        column++;
        return true;
    }

    ubyte advance() {
        column++;
        return source[current++];
    }

    ubyte peek() {
        return at_end ? 0 : source[current];
    }

    ubyte peekNext() {
        return current + 1 >= source.length ? 0 : source[current + 1];
    }

    void add_token(CTokenType type) {
        auto lex = cast(str)source[start .. current];
        *tokens ~= CToken(type, lex, line, start_column);
    }
}

// Character classification helpers
bool is_alpha()(ubyte c) {
    c |= 32; // ASCII tolower
    return c >= 'a' && c <= 'z';
}

bool is_digit()(ubyte c) {
    return c >= '0' && c <= '9';
}

bool is_hex_digit()(ubyte c) {
    c |= 0x20;
    return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f');
}

bool is_ident_char()(ubyte c) {
    return is_alpha(c) || is_digit(c) || c == '_';
}
