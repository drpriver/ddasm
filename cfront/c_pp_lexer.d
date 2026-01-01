/*
 * C Preprocessor Lexer (Phase 3)
 * Converts source characters to preprocessing tokens
 * Copyright 2025, David Priver
 */
module cfront.c_pp_lexer;

import core.stdc.stdio : fprintf, stderr;
import dlib.aliases;
import dlib.allocator : Allocator;
import dlib.barray : Barray, make_barray;
import dlib.stringbuilder : StringBuilder;
import cfront.c_pp_token;

struct PPLexer {
    const(ubyte)[] source;
    Barray!PPToken* tokens;
    Allocator allocator;
    str current_file;

    int current = 0;
    int start = 0;
    int line = 1;
    int column = 1;
    int start_column = 1;

    bool error_occurred = false;

    // Initialize the lexer
    void init(){
        // Nothing special needed
    }

    // Main entry point: tokenize entire source to PPTokens
    int tokenize(){
        while(!at_end()){
            // Skip line splices (backslash-newline) before starting a token
            skip_line_splices();
            if(at_end()) break;
            start = current;
            start_column = column;
            scan_token();
            if(error_occurred) return 1;
        }
        add_token(PPTokenType.PP_EOF, "");
        return 0;
    }

    // Skip any backslash-newline sequences at current position
    void skip_line_splices(){
        while(current < source.length && source[current] == '\\'){
            if(current + 1 < source.length && source[current + 1] == '\n'){
                current += 2;
                line++;
                column = 1;
            } else if(current + 2 < source.length && source[current + 1] == '\r' && source[current + 2] == '\n'){
                current += 3;
                line++;
                column = 1;
            } else {
                break;
            }
        }
    }

    // Check if at end of source
    bool at_end() const {
        return current >= source.length;
    }

    // Peek current character
    ubyte peek() const {
        if(at_end()) return 0;
        return source[current];
    }

    // Peek next character
    ubyte peek_next() const {
        if(current + 1 >= source.length) return 0;
        return source[current + 1];
    }

    // Peek N characters ahead
    ubyte peek_n(int n) const {
        if(current + n >= source.length) return 0;
        return source[current + n];
    }

    // Advance and return current character
    // Handles line splicing (backslash-newline)
    ubyte advance(){
        if(at_end()) return 0;

        // Handle line splicing: \ followed by newline
        while(source[current] == '\\' && current + 1 < source.length){
            if(source[current + 1] == '\n'){
                current += 2;
                line++;
                column = 1;
                if(at_end()) return 0;
            } else if(source[current + 1] == '\r' && current + 2 < source.length && source[current + 2] == '\n'){
                current += 3;
                line++;
                column = 1;
                if(at_end()) return 0;
            } else {
                break;
            }
        }

        ubyte c = source[current];
        current++;
        column++;
        return c;
    }

    // Match expected character
    bool match(ubyte expected){
        if(at_end()) return false;
        if(peek() != expected) return false;
        advance();
        return true;
    }

    // Add a token
    void add_token(PPTokenType type, str lexeme){
        PPToken tok;
        tok.type = type;
        tok.lexeme = lexeme;
        tok.line = line;
        tok.column = start_column;
        tok.file = current_file;
        *tokens ~= tok;
    }

    // Add token using source slice
    void add_token_slice(PPTokenType type){
        add_token(type, cast(str)source[start .. current]);
    }

    // Report error
    void error(str msg){
        error_occurred = true;
        fprintf(stderr, "%.*s:%d:%d: Lexer error: %.*s\n",
                cast(int)current_file.length, current_file.ptr,
                line, column,
                cast(int)msg.length, msg.ptr);
    }

    // Scan a single token
    void scan_token(){
        ubyte c = advance();

        // Newline - significant for preprocessor
        if(c == '\n'){
            line++;
            column = 1;
            add_token(PPTokenType.PP_NEWLINE, "\n");
            return;
        }

        // Carriage return (handle \r\n)
        if(c == '\r'){
            if(peek() == '\n'){
                advance();
            }
            line++;
            column = 1;
            add_token(PPTokenType.PP_NEWLINE, "\n");
            return;
        }

        // Whitespace (not newline)
        if(c == ' ' || c == '\t'){
            while(peek() == ' ' || peek() == '\t'){
                advance();
            }
            add_token(PPTokenType.PP_WHITESPACE, " ");
            return;
        }

        // Block comment
        if(c == '/' && peek() == '*'){
            advance();  // consume *
            scan_block_comment();
            return;
        }

        // Line comment
        if(c == '/' && peek() == '/'){
            advance();  // consume second /
            while(!at_end() && peek() != '\n'){
                advance();
            }
            // Emit as whitespace
            add_token(PPTokenType.PP_WHITESPACE, " ");
            return;
        }

        // String literal
        if(c == '"'){
            scan_string();
            return;
        }

        // Wide/UTF string prefixes
        if((c == 'L' || c == 'u' || c == 'U') && peek() == '"'){
            advance();  // consume "
            scan_string();
            return;
        }
        if(c == 'u' && peek() == '8' && peek_next() == '"'){
            advance();  // consume 8
            advance();  // consume "
            scan_string();
            return;
        }

        // Character literal
        if(c == '\''){
            scan_char();
            return;
        }

        // Wide/UTF char prefixes
        if((c == 'L' || c == 'u' || c == 'U') && peek() == '\''){
            advance();  // consume '
            scan_char();
            return;
        }

        // Identifier or keyword
        if(is_ident_start(c)){
            scan_identifier();
            return;
        }

        // Number (preprocessing number is more permissive)
        if(is_digit(c)){
            scan_pp_number();
            return;
        }

        // Dot followed by digit is a pp-number
        if(c == '.' && is_digit(peek())){
            scan_pp_number();
            return;
        }

        // Multi-character punctuators
        scan_punctuator(c);
    }

    // Scan block comment (replace with single space)
    void scan_block_comment(){
        int depth = 1;
        while(!at_end()){
            if(peek() == '*' && peek_next() == '/'){
                advance();
                advance();
                depth--;
                if(depth == 0) break;
            } else if(peek() == '\n'){
                advance();
                line++;
                column = 1;
            } else {
                advance();
            }
        }
        // Block comments become single space
        add_token(PPTokenType.PP_WHITESPACE, " ");
    }

    // Scan string literal (including escape sequences)
    void scan_string(){
        while(!at_end() && peek() != '"'){
            if(peek() == '\\' && peek_next() != 0){
                advance();  // backslash
                advance();  // escaped char
            } else if(peek() == '\n'){
                error("Unterminated string literal");
                return;
            } else {
                advance();
            }
        }
        if(at_end()){
            error("Unterminated string literal");
            return;
        }
        advance();  // closing "
        add_token_slice(PPTokenType.PP_STRING);
    }

    // Scan character literal
    void scan_char(){
        while(!at_end() && peek() != '\''){
            if(peek() == '\\' && peek_next() != 0){
                advance();  // backslash
                advance();  // escaped char
            } else if(peek() == '\n'){
                error("Unterminated character literal");
                return;
            } else {
                advance();
            }
        }
        if(at_end()){
            error("Unterminated character literal");
            return;
        }
        advance();  // closing '
        add_token_slice(PPTokenType.PP_CHAR);
    }

    // Scan identifier
    void scan_identifier(){
        while(is_ident_char(peek())){
            advance();
        }
        add_token_slice(PPTokenType.PP_IDENTIFIER);
    }

    // Scan preprocessing number
    // A pp-number is: digit | . digit, followed by any of:
    // digit, identifier-char, e/E/p/P followed by +/-, .
    void scan_pp_number(){
        while(!at_end()){
            ubyte c = peek();

            // Digit or letter (identifier continuation)
            if(is_ident_char(c)){
                advance();
                continue;
            }

            // Dot
            if(c == '.'){
                advance();
                continue;
            }

            // Exponent with sign: e+, e-, E+, E-, p+, p-, P+, P-
            if((c == 'e' || c == 'E' || c == 'p' || c == 'P') &&
                (peek_next() == '+' || peek_next() == '-')){
                advance();  // e/E/p/P
                advance();  // +/-
                continue;
            }

            break;
        }
        add_token_slice(PPTokenType.PP_NUMBER);
    }

    // Scan punctuator (including multi-character ones)
    void scan_punctuator(ubyte c){
        // Check for multi-character punctuators
        ubyte n = peek();

        // Three-character punctuators
        if(c == '.' && n == '.' && peek_next() == '.'){
            advance(); advance();
            add_token(PPTokenType.PP_PUNCTUATOR, "...");
            return;
        }
        if(c == '<' && n == '<' && peek_next() == '='){
            advance(); advance();
            add_token(PPTokenType.PP_PUNCTUATOR, "<<=");
            return;
        }
        if(c == '>' && n == '>' && peek_next() == '='){
            advance(); advance();
            add_token(PPTokenType.PP_PUNCTUATOR, ">>=");
            return;
        }

        // Two-character punctuators
        switch(c){
            case '#':
                if(n == '#'){
                    advance();
                    add_token(PPTokenType.PP_PUNCTUATOR, "##");
                    return;
                }
                break;
            case '<':
                if(n == '<'){ advance(); add_token(PPTokenType.PP_PUNCTUATOR, "<<"); return; }
                if(n == '='){ advance(); add_token(PPTokenType.PP_PUNCTUATOR, "<="); return; }
                break;
            case '>':
                if(n == '>'){ advance(); add_token(PPTokenType.PP_PUNCTUATOR, ">>"); return; }
                if(n == '='){ advance(); add_token(PPTokenType.PP_PUNCTUATOR, ">="); return; }
                break;
            case '=':
                if(n == '='){ advance(); add_token(PPTokenType.PP_PUNCTUATOR, "=="); return; }
                break;
            case '!':
                if(n == '='){ advance(); add_token(PPTokenType.PP_PUNCTUATOR, "!="); return; }
                break;
            case '&':
                if(n == '&'){ advance(); add_token(PPTokenType.PP_PUNCTUATOR, "&&"); return; }
                if(n == '='){ advance(); add_token(PPTokenType.PP_PUNCTUATOR, "&="); return; }
                break;
            case '|':
                if(n == '|'){ advance(); add_token(PPTokenType.PP_PUNCTUATOR, "||"); return; }
                if(n == '='){ advance(); add_token(PPTokenType.PP_PUNCTUATOR, "|="); return; }
                break;
            case '+':
                if(n == '+'){ advance(); add_token(PPTokenType.PP_PUNCTUATOR, "++"); return; }
                if(n == '='){ advance(); add_token(PPTokenType.PP_PUNCTUATOR, "+="); return; }
                break;
            case '-':
                if(n == '-'){ advance(); add_token(PPTokenType.PP_PUNCTUATOR, "--"); return; }
                if(n == '='){ advance(); add_token(PPTokenType.PP_PUNCTUATOR, "-="); return; }
                if(n == '>'){ advance(); add_token(PPTokenType.PP_PUNCTUATOR, "->"); return; }
                break;
            case '*':
                if(n == '='){ advance(); add_token(PPTokenType.PP_PUNCTUATOR, "*="); return; }
                break;
            case '/':
                if(n == '='){ advance(); add_token(PPTokenType.PP_PUNCTUATOR, "/="); return; }
                break;
            case '%':
                if(n == '='){ advance(); add_token(PPTokenType.PP_PUNCTUATOR, "%="); return; }
                break;
            case '^':
                if(n == '='){ advance(); add_token(PPTokenType.PP_PUNCTUATOR, "^="); return; }
                break;
            default:
                break;
        }

        // Single-character punctuator - use source slice
        add_token_slice(PPTokenType.PP_PUNCTUATOR);
    }
}

// Tokenize source to PPToken array
int pp_tokenize(const(ubyte)[] source, str filename, Barray!PPToken* tokens, Allocator allocator){
    PPLexer lexer;
    lexer.source = source;
    lexer.tokens = tokens;
    lexer.allocator = allocator;
    lexer.current_file = filename;
    lexer.init();
    return lexer.tokenize();
}
