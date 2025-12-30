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
import dlib.file_util : read_file, FileResult;

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
    VOLATILE = 610,
    STATIC = 611,
    EXTERN = 612,
    STRUCT = 613,
    UNION = 614,
    ENUM = 615,
    TYPEDEF = 616,
    SIZEOF = 617,
    STATIC_ASSERT = 618,

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
    str file;  // Source file path
}

// Conditional compilation block state
struct CondBlock {
    bool condition_met;     // Was any branch of this #if/#elif/#else true?
    bool currently_active;  // Is current branch being processed?
    bool seen_else;         // Have we seen #else for this block?
}

// Include stack frame - saves tokenizer state when entering an included file
struct IncludeFrame {
    const(ubyte)[] source;
    int current;
    int start;
    int line;
    int column;
    int start_column;
    str current_file;       // Path of file being processed
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

    // Conditional compilation stack
    Barray!CondBlock cond_stack;

    // Include system
    Barray!IncludeFrame include_stack;
    Table!(str, bool) included_files;  // Files already included (for include guards)
    str current_file;                   // Current file being processed

    // Include search paths
    static immutable str[] INCLUDE_PATHS = [
        "/usr/include/SDL2",
        "/usr/include/x86_64-linux-gnu",  // glibc arch-specific headers
        "/usr/include",
        "/usr/local/include"
    ];

    bool ERROR_OCCURRED = false;

    void init() {
        defines.data.allocator = allocator;
        cond_stack.bdata.allocator = allocator;
        include_stack.bdata.allocator = allocator;
        included_files.data.allocator = allocator;

        // Predefined macros for SDL headers compatibility
        // Platform macros
        version(linux) {
            defines["__linux__"] = "1";
            defines["__unix__"] = "1";
        }
        version(Windows) {
            defines["_WIN32"] = "1";
        }
        version(OSX) {
            defines["__APPLE__"] = "1";
            defines["__MACH__"] = "1";
        }

        // Note: __cplusplus is NOT defined (we're C, not C++)

        // SDL-specific attributes that expand to nothing for us
        defines["DECLSPEC"] = "";
        defines["SDLCALL"] = "";
        defines["SDL_DEPRECATED"] = "";
        defines["SDL_UNUSED"] = "";
        defines["SDL_FORCE_INLINE"] = "static inline";
        defines["SDL_INLINE"] = "static inline";

        // Pretend to be GCC for SDL's compiler detection
        defines["__GNUC__"] = "4";
        defines["__GNUC_MINOR__"] = "0";

        // Size/type macros
        defines["__SIZEOF_POINTER__"] = "8";  // 64-bit
        defines["__x86_64__"] = "1";

        // Disable complex number types (not needed for SDL)
        defines["__HAVE_FLOAT128"] = "0";
        defines["__HAVE_FLOAT64X"] = "0";
        defines["__HAVE_FLOAT32"] = "0";
        defines["__HAVE_FLOAT64"] = "0";

        // glibc compatibility (C++ extern "C" wrappers)
        defines["__BEGIN_DECLS"] = "";
        defines["__END_DECLS"] = "";
        defines["__THROW"] = "";
        defines["__THROWNL"] = "";
        // Note: __nonnull is a function-like macro, so we skip its definition
        // and handle it as unknown macro invocation at parse time
        defines["__attribute_pure__"] = "";
        defines["__attribute_malloc__"] = "";
        defines["__wur"] = "";  // warn_unused_result

        // glibc compatibility - va_list support
        defines["__gnuc_va_list"] = "void*";

        // glibc bits/types.h support
        defines["__STD_TYPE"] = "typedef";
        defines["__extension__"] = "";
        defines["__inline"] = "";
        defines["__inline__"] = "";
        defines["__restrict"] = "";
        defines["__restrict__"] = "";
        defines["__volatile__"] = "";
        defines["__asm__"] = ""; // NOTE: This will break asm() usage

        // glibc type macros (from bits/typesizes.h)
        defines["__S16_TYPE"] = "short";
        defines["__U16_TYPE"] = "unsigned short";
        defines["__S32_TYPE"] = "int";
        defines["__U32_TYPE"] = "unsigned int";
        defines["__SLONGWORD_TYPE"] = "long";
        defines["__ULONGWORD_TYPE"] = "unsigned long";
        defines["__SQUAD_TYPE"] = "long long";
        defines["__UQUAD_TYPE"] = "unsigned long long";
        defines["__SWORD_TYPE"] = "long";
        defines["__UWORD_TYPE"] = "unsigned long";
        defines["__SLONG32_TYPE"] = "int";
        defines["__ULONG32_TYPE"] = "unsigned int";
        defines["__S64_TYPE"] = "long long";
        defines["__U64_TYPE"] = "unsigned long long";

        // Type aliases used in bits/types.h
        defines["__DEV_T_TYPE"] = "unsigned long long";
        defines["__UID_T_TYPE"] = "unsigned int";
        defines["__GID_T_TYPE"] = "unsigned int";
        defines["__INO_T_TYPE"] = "unsigned long";
        defines["__INO64_T_TYPE"] = "unsigned long long";
        defines["__MODE_T_TYPE"] = "unsigned int";
        defines["__NLINK_T_TYPE"] = "unsigned long";
        defines["__OFF_T_TYPE"] = "long";
        defines["__OFF64_T_TYPE"] = "long long";
        defines["__PID_T_TYPE"] = "int";
        defines["__RLIM_T_TYPE"] = "unsigned long";
        defines["__RLIM64_T_TYPE"] = "unsigned long long";
        defines["__BLKCNT_T_TYPE"] = "long";
        defines["__BLKCNT64_T_TYPE"] = "long long";
        defines["__FSBLKCNT_T_TYPE"] = "unsigned long";
        defines["__FSBLKCNT64_T_TYPE"] = "unsigned long long";
        defines["__FSFILCNT_T_TYPE"] = "unsigned long";
        defines["__FSFILCNT64_T_TYPE"] = "unsigned long long";
        defines["__ID_T_TYPE"] = "unsigned int";
        defines["__CLOCK_T_TYPE"] = "long";
        defines["__TIME_T_TYPE"] = "long";
        defines["__USECONDS_T_TYPE"] = "unsigned int";
        defines["__SUSECONDS_T_TYPE"] = "long";
        defines["__DADDR_T_TYPE"] = "int";
        defines["__KEY_T_TYPE"] = "int";
        defines["__CLOCKID_T_TYPE"] = "int";
        defines["__TIMER_T_TYPE"] = "void*";
        defines["__BLKSIZE_T_TYPE"] = "long";
        defines["__SSIZE_T_TYPE"] = "long";
        defines["__SYSCALL_SLONG_TYPE"] = "long";
        defines["__SYSCALL_ULONG_TYPE"] = "unsigned long";
        defines["__CPU_MASK_TYPE"] = "unsigned long";

    }

    // Check if we're in an active preprocessor block
    bool is_active() {
        if (cond_stack.count == 0) return true;
        return cond_stack[cond_stack.count - 1].currently_active;
    }

    // Check if parent block is active (for nested conditionals)
    bool is_parent_active() {
        if (cond_stack.count <= 1) return true;
        return cond_stack[cond_stack.count - 2].currently_active;
    }

    void error(str message) {
        ERROR_OCCURRED = true;
        if (current_file.length > 0) {
            fprintf(stderr, "%.*s:%d:%d: Tokenize Error: %.*s\n",
                    cast(int)current_file.length, current_file.ptr,
                    line, column, cast(int)message.length, message.ptr);
        } else {
            fprintf(stderr, "[line %d, col %d]: Tokenize Error: %.*s\n",
                    line, column, cast(int)message.length, message.ptr);
        }
    }

    bool at_end() {
        if (current >= source.length) {
            // Try to pop include stack
            if (include_stack.count > 0) {
                pop_include_frame();
                return at_end();  // Recurse to check parent
            }
            return true;
        }
        return false;
    }

    void push_include_frame() {
        IncludeFrame frame;
        frame.source = source;
        frame.current = current;
        frame.start = start;
        frame.line = line;
        frame.column = column;
        frame.start_column = start_column;
        frame.current_file = current_file;
        include_stack ~= frame;
    }

    void pop_include_frame() {
        if (include_stack.count == 0) return;
        auto frame = include_stack[include_stack.count - 1];
        include_stack.pop();
        source = frame.source;
        current = frame.current;
        start = frame.start;
        line = frame.line;
        column = frame.column;
        start_column = frame.start_column;
        current_file = frame.current_file;
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

            // In inactive preprocessor block, skip everything except preprocessor directives
            if (!is_active() && c != '#') {
                // Still need to track newlines for line numbers
                if (c == '\n') {
                    line++;
                    column = 1;
                } else if (c == '/' && peek == '*') {
                    // Still need to skip block comments to avoid nested issues
                    advance();
                    do_block_comment();
                } else if (c == '/' && peek == '/') {
                    // Skip line comments
                    while (peek != '\n' && !at_end) advance();
                }
                return;
            }

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
                case '?':
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
                case 0:  // NUL bytes (can appear as padding from allocator)
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
        while (peek.is_alpha || peek == '_') advance();
        str directive = cast(str)source[directive_start .. current];

        // Conditional directives must be processed even in inactive blocks
        // to properly track nesting
        if (directive == "ifdef" || directive == "ifndef" ||
            directive == "if" || directive == "elif" ||
            directive == "else" || directive == "endif") {
            handle_conditional(directive);
            return;
        }

        // All other directives only processed in active blocks
        if (!is_active()) {
            skip_to_eol();
            return;
        }

        if (directive == "pragma") {
            // Skip to end of line, store as pragma token
            while (peek != '\n' && !at_end) advance();
            add_token(CTokenType.PRAGMA);
        } else if (directive == "define") {
            do_define();
        } else if (directive == "undef") {
            do_undef();
        } else if (directive == "include") {
            do_include();
        } else {
            // Skip other preprocessor directives (error, warning, line, etc.)
            skip_to_eol();
        }
    }

    void do_include() {
        while (peek == ' ' || peek == '\t') advance();

        bool is_system = false;
        str filename;
        int filename_start;

        if (peek == '<') {
            is_system = true;
            advance();
            filename_start = current;
            while (peek != '>' && peek != '\n' && !at_end) advance();
            filename = cast(str)source[filename_start .. current];
            if (peek == '>') advance();
        }
        else if (peek == '"') {
            advance();
            filename_start = current;
            while (peek != '"' && peek != '\n' && !at_end) advance();
            filename = cast(str)source[filename_start .. current];
            if (peek == '"') advance();
        }
        else {
            error("Expected '<' or '\"' after #include");
            skip_to_eol();
            return;
        }

        skip_to_eol();

        if (filename.length == 0) {
            error("Empty include filename");
            return;
        }

        // Try to resolve the include path
        str full_path = resolve_include_path(filename, is_system);
        if (full_path.length == 0) {
            // For now, just warn and skip - some system headers we can't/won't process
            // fprintf(stderr, "Warning: Cannot find include file: %.*s\n",
            //         cast(int)filename.length, filename.ptr);
            return;
        }

        // Check if already included (include guard)
        if (full_path in included_files) {
            return;  // Already included
        }

        // Mark as included
        included_files[full_path] = true;

        // Read the file
        import dlib.stringbuilder : StringBuilder;
        StringBuilder path_buf;
        path_buf.allocator = allocator;
        path_buf.write(full_path);
        path_buf.nul_terminate();

        auto file_result = read_file(path_buf.borrow().ptr, allocator);
        if (file_result.errored) {
            // fprintf(stderr, "Warning: Cannot read include file: %.*s\n",
            //         cast(int)full_path.length, full_path.ptr);
            return;
        }
        // Save current state and switch to new file
        push_include_frame();

        source = cast(const(ubyte)[])file_result.value.data;
        current = 0;
        start = 0;
        line = 1;
        column = 1;
        start_column = 1;
        current_file = full_path;
    }

    str resolve_include_path(str filename, bool is_system) {
        import dlib.stringbuilder : StringBuilder;
        import core.sys.posix.sys.stat : stat, stat_t;

        StringBuilder path_buf;
        path_buf.allocator = allocator;

        // For quoted includes, first try relative to current file's directory
        if (!is_system && current_file.length > 0) {
            // Find last '/' in current_file
            size_t last_slash = 0;
            for (size_t i = 0; i < current_file.length; i++) {
                if (current_file[i] == '/') last_slash = i;
            }
            if (last_slash > 0) {
                path_buf.reset();
                path_buf.write(current_file[0 .. last_slash + 1]);
                path_buf.write(filename);
                path_buf.nul_terminate();

                stat_t s;
                if (stat(path_buf.borrow().ptr, &s) == 0) {
                    return path_buf.borrow();
                }
            }
        }

        // Search system paths
        foreach (search_path; INCLUDE_PATHS) {
            path_buf.reset();
            path_buf.write(search_path);
            path_buf.write('/');
            path_buf.write(filename);
            path_buf.nul_terminate();

            stat_t s;
            if (stat(path_buf.borrow().ptr, &s) == 0) {
                return path_buf.borrow();
            }
        }

        return "";  // Not found
    }

    void skip_to_eol() {
        // Skip to end of line, handling line continuations
        while (!at_end) {
            if (peek == '\\' && peekNext == '\n') {
                advance(); // skip backslash
                advance(); // skip newline
                line++;
                column = 1;
                continue;
            }
            if (peek == '\n') break;
            advance();
        }
    }

    void handle_conditional(str directive) {
        if (directive == "ifdef") {
            // Skip whitespace
            while (peek == ' ' || peek == '\t') advance();
            // Get macro name
            int name_start = current;
            while (peek.is_ident_char) advance();
            str macro_name = cast(str)source[name_start .. current];
            skip_to_eol();

            bool defined = (macro_name in defines) !is null;
            push_cond(defined);
        }
        else if (directive == "ifndef") {
            while (peek == ' ' || peek == '\t') advance();
            int name_start = current;
            while (peek.is_ident_char) advance();
            str macro_name = cast(str)source[name_start .. current];
            skip_to_eol();

            bool defined = (macro_name in defines) !is null;
            push_cond(!defined);
        }
        else if (directive == "if") {
            bool result = evaluate_pp_expression();
            skip_to_eol();
            push_cond(result);
        }
        else if (directive == "elif") {
            skip_to_eol();  // For now, don't evaluate - just skip
            if (cond_stack.count == 0) {
                error("#elif without #if");
                return;
            }
            auto top = &cond_stack[cond_stack.count - 1];
            if (top.seen_else) {
                error("#elif after #else");
                return;
            }
            // If any previous branch was taken, skip this one
            if (top.condition_met) {
                top.currently_active = false;
            } else {
                // TODO: evaluate #elif expression
                // For now, treat as false
                top.currently_active = false;
            }
        }
        else if (directive == "else") {
            skip_to_eol();
            if (cond_stack.count == 0) {
                error("#else without #if");
                return;
            }
            auto top = &cond_stack[cond_stack.count - 1];
            if (top.seen_else) {
                error("duplicate #else");
                return;
            }
            top.seen_else = true;
            // Activate else branch only if no previous branch was taken and parent is active
            top.currently_active = !top.condition_met && is_parent_active();
        }
        else if (directive == "endif") {
            skip_to_eol();
            if (cond_stack.count == 0) {
                error("#endif without #if");
                return;
            }
            cond_stack.pop();
        }
    }

    void push_cond(bool active) {
        CondBlock block;
        block.currently_active = active && is_active();
        block.condition_met = block.currently_active;
        block.seen_else = false;
        cond_stack ~= block;
    }

    // Skip preprocessor whitespace (spaces, tabs, line continuations)
    void skip_pp_ws() {
        while (!at_end) {
            if (peek == ' ' || peek == '\t') {
                advance();
            } else if (peek == '\\' && peekNext == '\n') {
                advance(); // skip backslash
                advance(); // skip newline
                line++;
                column = 1;
            } else {
                break;
            }
        }
    }

    // Evaluate preprocessor expression (for #if)
    // Supports: defined(X), defined X, ||, &&, !, numbers, ()
    bool evaluate_pp_expression() {
        skip_pp_ws();
        return eval_pp_or();
    }

    bool eval_pp_or() {
        bool result = eval_pp_and();
        while (true) {
            skip_pp_ws();
            if (peek == '|' && peekNext == '|') {
                advance(); advance();
                bool right = eval_pp_and();
                result = result || right;
            } else {
                break;
            }
        }
        return result;
    }

    bool eval_pp_and() {
        bool result = eval_pp_unary();
        while (true) {
            skip_pp_ws();
            if (peek == '&' && peekNext == '&') {
                advance(); advance();
                bool right = eval_pp_unary();
                result = result && right;
            } else {
                break;
            }
        }
        return result;
    }

    bool eval_pp_unary() {
        skip_pp_ws();
        if (peek == '!') {
            advance();
            return !eval_pp_unary();
        }
        return eval_pp_primary();
    }

    bool eval_pp_primary() {
        skip_pp_ws();

        // defined(X) or defined X
        if (match_pp_word("defined")) {
            skip_pp_ws();
            bool has_paren = false;
            if (peek == '(') {
                advance();
                has_paren = true;
                skip_pp_ws();
            }
            int name_start = current;
            while (peek.is_ident_char) advance();
            str name = cast(str)source[name_start .. current];
            if (has_paren) {
                skip_pp_ws();
                if (peek == ')') advance();
            }
            return (name in defines) !is null;
        }

        // Parenthesized expression
        if (peek == '(') {
            advance();
            bool result = eval_pp_or();
            skip_pp_ws();
            if (peek == ')') advance();
            return result;
        }

        // Number literal (0 = false, non-zero = true)
        if (peek.is_digit) {
            long value = 0;
            while (peek.is_digit) {
                value = value * 10 + (advance() - '0');
            }
            // Skip suffixes
            while (peek == 'L' || peek == 'l' || peek == 'U' || peek == 'u') advance();
            return value != 0;
        }

        // Identifier - undefined = 0
        if (peek.is_alpha || peek == '_') {
            int name_start = current;
            while (peek.is_ident_char) advance();
            str name = cast(str)source[name_start .. current];

            // Skip function-like macro arguments if present
            skip_pp_ws();
            if (peek == '(') {
                // Skip balanced parentheses
                advance();  // consume '('
                int depth = 1;
                while (depth > 0 && !at_end) {
                    if (peek == '(') depth++;
                    else if (peek == ')') depth--;
                    advance();
                }
            }

            // Check if it's a defined macro with a numeric value
            if (str* val = name in defines) {
                // Try to parse as number
                return parse_number_from_str(*val) != 0;
            }
            return false;  // Undefined identifier = 0
        }

        return false;
    }

    bool match_pp_word(str word) {
        if (current + word.length > source.length) return false;
        if (cast(str)source[current .. current + word.length] != word) return false;
        // Make sure it's not part of a longer identifier
        if (current + word.length < source.length && source[current + word.length].is_ident_char) return false;
        current += cast(int)word.length;
        column += cast(int)word.length;
        return true;
    }

    long parse_number_from_str(str s) {
        long value = 0;
        bool negative = false;
        size_t i = 0;
        // Skip whitespace
        while (i < s.length && (s[i] == ' ' || s[i] == '\t')) i++;
        if (i < s.length && s[i] == '-') {
            negative = true;
            i++;
        }
        // Handle hex
        if (i + 1 < s.length && s[i] == '0' && (s[i + 1] == 'x' || s[i + 1] == 'X')) {
            i += 2;
            while (i < s.length && s[i].is_hex_digit) {
                ubyte c = s[i] | 0x20;
                if (c >= 'a') value = value * 16 + (c - 'a' + 10);
                else value = value * 16 + (c - '0');
                i++;
            }
        } else {
            while (i < s.length && s[i].is_digit) {
                value = value * 10 + (s[i] - '0');
                i++;
            }
        }
        return negative ? -value : value;
    }

    void do_define() {
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

        // Check for function-like macro: NAME( with no space before (
        if (peek == '(') {
            // This is a function-like macro - skip it entirely for now
            // (Phase 4 will implement proper function-like macro support)
            skip_to_eol();
            return;
        }

        // Skip whitespace after name
        while (peek == ' ' || peek == '\t') advance();

        // Get replacement text (everything until end of line)
        // Handle line continuations with backslash and multi-line comments
        int value_start = current;
        int value_end = current;  // Track end of actual value (before comments)
        bool in_comment = false;

        while (!at_end) {
            // Check for start of block comment
            if (!in_comment && peek == '/' && peekNext == '*') {
                if (!in_comment) {
                    value_end = current;  // Mark end of value before comment
                }
                in_comment = true;
                advance(); advance();  // skip /*
                continue;
            }

            // Check for end of block comment
            if (in_comment && peek == '*' && peekNext == '/') {
                in_comment = false;
                advance(); advance();  // skip */
                continue;
            }

            // If we're in a comment, keep going (even past newlines)
            if (in_comment) {
                if (peek == '\n') {
                    line++;
                    column = 0;
                }
                advance();
                continue;
            }

            // Normal end of line (not in comment)
            if (peek == '\n') break;

            // Line continuation
            if (peek == '\\' && peekNext == '\n') {
                advance(); // skip backslash
                advance(); // skip newline
                line++;
                column = 1;
                value_end = current;  // Include continued lines
                continue;
            }

            advance();
            value_end = current;  // Update value end as we go
        }

        str value = cast(str)source[value_start .. value_end];

        // Trim trailing whitespace from value
        while (value.length > 0 && (value[$ - 1] == ' ' || value[$ - 1] == '\t')) {
            value = value[0 .. $ - 1];
        }

        // Store in defines table - don't overwrite predefined macros
        if ((macro_name in defines) is null) {
            defines[macro_name] = value;
        }
    }

    void do_undef() {
        while (peek == ' ' || peek == '\t') advance();
        int name_start = current;
        while (peek.is_ident_char) advance();
        str macro_name = cast(str)source[name_start .. current];
        skip_to_eol();
        // Note: Table doesn't have remove, but we can set to a special marker
        // For now, just skip - #undef is rarely needed for SDL headers
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
            case "volatile": type = CTokenType.VOLATILE; break;
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
                *tokens ~= CToken(cast(CTokenType) c, text[i .. i + 1], line, start_column, current_file);
                i++;
                continue;
            }

            // Hex number
            if (c == '0' && i + 1 < text.length && (text[i + 1] == 'x' || text[i + 1] == 'X')) {
                size_t num_start = i;
                i += 2;  // skip 0x
                while (i < text.length && text[i].is_hex_digit) i++;
                // Skip suffixes like L, U, UL
                while (i < text.length && (text[i] == 'L' || text[i] == 'l' ||
                                            text[i] == 'U' || text[i] == 'u')) i++;
                *tokens ~= CToken(CTokenType.HEX, text[num_start .. i], line, start_column, current_file);
                continue;
            }

            // Decimal number
            if (c.is_digit) {
                size_t num_start = i;
                while (i < text.length && text[i].is_digit) i++;
                // Skip suffixes like L, U, UL
                while (i < text.length && (text[i] == 'L' || text[i] == 'l' ||
                                            text[i] == 'U' || text[i] == 'u')) i++;
                *tokens ~= CToken(CTokenType.NUMBER, text[num_start .. i], line, start_column, current_file);
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
                *tokens ~= CToken(CTokenType.STRING, text[str_start .. i], line, start_column, current_file);
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
                *tokens ~= CToken(CTokenType.CHAR_LITERAL, text[chr_start .. i], line, start_column, current_file);
                continue;
            }

            // Identifier or keyword
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
                    // Check for keywords
                    CTokenType type = CTokenType.IDENTIFIER;
                    switch (ident) {
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
                        case "volatile": type = CTokenType.VOLATILE; break;
                        case "static":   type = CTokenType.STATIC; break;
                        case "extern":   type = CTokenType.EXTERN; break;
                        case "struct":   type = CTokenType.STRUCT; break;
                        case "union":    type = CTokenType.UNION; break;
                        case "enum":     type = CTokenType.ENUM; break;
                        case "typedef":  type = CTokenType.TYPEDEF; break;
                        case "sizeof":   type = CTokenType.SIZEOF; break;
                        default: break;
                    }
                    *tokens ~= CToken(type, ident, line, start_column, current_file);
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
                // Handle suffixes like L, U, UL, etc.
                while (peek == 'l' || peek == 'L' || peek == 'u' || peek == 'U') {
                    advance();
                }
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
        *tokens ~= CToken(type, lex, line, start_column, current_file);
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
