/*
 * C Preprocessor Token Definitions
 * Copyright 2025, David Priver
 */
module cfront.c_pp_token;

import dlib.aliases;
import dlib.allocator : Allocator;
import dlib.table : Table;

// Preprocessing token types (C standard phase 3)
enum PPTokenType : uint {
    PP_IDENTIFIER,
    PP_NUMBER,           // Preprocessing number (more general than C number)
    PP_STRING,           // "..."
    PP_CHAR,             // '...'
    PP_PUNCTUATOR,       // Operators and punctuation
    PP_WHITESPACE,       // Significant for stringification
    PP_NEWLINE,          // Significant for directive boundaries
    PP_HEADER_NAME,      // <...> or "..." in #include
    PP_PLACEMARKER,      // Empty token for ## edge cases
    PP_EOF,
}

// Preprocessing token
struct PPToken {
    PPTokenType type;
    str lexeme;          // Token text
    int line;
    int column;
    str file;

    // Expansion location (where macro was invoked, if this came from macro expansion)
    int expansion_line;
    int expansion_column;
    str expansion_file;

    // For hide set tracking (prevents infinite macro recursion)
    bool has_hide_set;
    HideSet* hide_set;

    // Check if this token matches a string
    bool matches(str s) const {
        if(lexeme.length != s.length) return false;
        for(size_t i = 0; i < lexeme.length; i++){
            if(lexeme[i] != s[i]) return false;
        }
        return true;
    }

    // Check if this is a specific punctuator
    bool is_punct(str p) const {
        return type == PPTokenType.PP_PUNCTUATOR && matches(p);
    }

    // Check if this is a specific identifier
    bool is_ident(str id) const {
        return type == PPTokenType.PP_IDENTIFIER && matches(id);
    }
}

// Macro definition with token-based replacement
struct PPMacroDef {
    str name;
    PPToken[] replacement;   // Token sequence (not string!)
    str[] params;            // Parameter names (null for object-like)
    bool is_function_like;
    bool is_variadic;        // Last param is ... or __VA_ARGS__
    bool is_undefined;       // True if #undef'd
    bool is_builtin;         // __FILE__, __LINE__, etc.
    bool is_null;            // null macro

    // Find parameter index by name, -1 if not found
    int find_param(str param_name) const {
        foreach(i, p; params){
            if(p == param_name) return cast(int)i;
        }
        return -1;
    }
}

// Hide set for preventing infinite macro recursion (C standard "blue paint")
struct HideSet {
    Table!(str, bool) hidden;

    static HideSet create(Allocator alloc){
        HideSet hs;
        hs.hidden.data.allocator = alloc;
        return hs;
    }

    bool is_hidden(str name){
        // Use iteration due to betterC slice comparison issues
        foreach(item; hidden.items){
            if(item.key==name) return true;
        }
        return false;
    }

    // Create a new hide set with an additional hidden name
    HideSet with_hidden(str name){
        HideSet result;
        result.hidden.data.allocator = hidden.data.allocator;
        // Copy existing
        foreach(item; hidden.items){
            result.hidden[item.key] = true;
        }
        // Add new
        result.hidden[name] = true;
        return result;
    }
}

// Conditional block state for #if/#elif/#else/#endif
struct PPCondBlock {
    bool condition_met;      // Was any branch of this #if/#elif/#else true?
    bool currently_active;   // Is current branch being processed?
    bool seen_else;          // Have we seen #else for this block?
}

// Include stack frame
struct PPIncludeFrame {
    PPToken[] remaining_tokens;  // Tokens left to process in parent file
    size_t position;             // Current position in remaining_tokens
    str filename;
    int line;
}

// Helper: Check if character is identifier start
bool is_ident_start(ubyte c){
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

// Helper: Check if character is identifier continuation
bool is_ident_char(ubyte c){
    return is_ident_start(c) || (c >= '0' && c <= '9');
}

// Helper: Check if character is digit
bool is_digit(ubyte c){
    return c >= '0' && c <= '9';
}

// Helper: Check if character is hex digit
bool is_hex_digit(ubyte c){
    return is_digit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

// Helper: Check if character is whitespace (not newline)
bool is_pp_whitespace(ubyte c){
    return c == ' ' || c == '\t' || c == '\r';
}
