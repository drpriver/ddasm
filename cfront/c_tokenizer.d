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
import dlib.file_util : read_file, FileResult, FileFlags;

// Helper for string comparison (betterC slice == may not work correctly)
bool str_eq(str a, str b) {
    if (a.length != b.length) return false;
    for (size_t i = 0; i < a.length; i++) {
        if (a[i] != b[i]) return false;
    }
    return true;
}

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
    ASM = 619,
    FLOAT16 = 620,

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

    // Macro definition
    struct MacroDef {
        str replacement;           // Replacement text
        str[] params;              // Parameter names (empty for object-like macros)
        bool is_function_like;     // True if macro has parameters
        bool is_variadic;          // True if last param is ...
        bool is_undefined;         // True if #undef'd (treat as not defined)
    }

    // Preprocessor defines: name -> macro definition
    Table!(str, MacroDef) defines;

    // Conditional compilation stack
    Barray!CondBlock cond_stack;

    // Include system
    Barray!IncludeFrame include_stack;
    Table!(str, bool) included_files;  // Files already included (for include guards)
    str current_file;                   // Current file being processed

    // Macro expansion tracking (prevent recursive expansion)
    Table!(str, bool) expanding_macros;

    // Include search paths
    static immutable str[] INCLUDE_PATHS = [
        "/usr/include/SDL2",
        "/usr/include/x86_64-linux-gnu",  // glibc arch-specific headers
        "/usr/include",
        "/usr/local/include",
        "/usr/lib/gcc/x86_64-linux-gnu/10/include",  // GCC's stddef.h etc
        "/usr/lib/gcc/x86_64-linux-gnu/7/include",   // Fallback for older GCC
        "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/",
        "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/17/include/",
        "",
    ];

    bool ERROR_OCCURRED = false;
    bool in_macro_evaluation = false;  // True during eval_macro_replacement to prevent include pops

    // Helper to define an object-like macro
    void define_macro(str name, str replacement) {
        MacroDef def = {};
        def.replacement = replacement;
        def.params = null;
        def.is_function_like = false;
        defines[name] = def;
    }

    // Check if a macro is defined (and not #undef'd)
    bool is_macro_defined(str name) {
        if (MacroDef* def = name in defines) {
            return !def.is_undefined;
        }
        return false;
    }

    // Helper to define a function-like macro
    void define_func_macro(str name, str[] params, str replacement, bool variadic = false) {
        MacroDef def;
        def.replacement = replacement;
        def.params = params;
        def.is_function_like = true;
        def.is_variadic = variadic;
        defines[name] = def;
    }

    void init() {
        defines.data.allocator = allocator;
        cond_stack.bdata.allocator = allocator;
        include_stack.bdata.allocator = allocator;
        included_files.data.allocator = allocator;
        expanding_macros.data.allocator = allocator;

        // Predefined macros for SDL headers compatibility
        // Platform macros
        version(linux) {
            define_macro("__linux__", "1");
            define_macro("__unix__", "1");
        }
        version(Windows) {
            define_macro("_WIN32", "1");
        }
        version(OSX) {
            define_macro("__APPLE__", "1");
            define_macro("__MACH__", "1");
        }

        // Byte order macros (x86 is little-endian)
        define_macro("__LITTLE_ENDIAN", "1234");
        define_macro("__BIG_ENDIAN", "4321");
        define_macro("__BYTE_ORDER", "1234");  // Little-endian

        // Note: __cplusplus is NOT defined (we're C, not C++)

        // Pretend to be GCC for SDL's compiler detection
        define_macro("__GNUC__", "4");
        define_macro("__GNUC_MINOR__", "0");
        define_macro("__STDC__", "1");
        define_macro("__STDC_VERSION__", "199901L");

        // Size/type macros
        define_macro("__SIZEOF_POINTER__", "8");  // 64-bit
        define_macro("__x86_64__", "1");
        define_macro("__LP64__", "1");  // Long and Pointer are 64-bit (standard 64-bit Linux data model)

        // Disable complex number types (not needed for SDL)
        define_macro("__HAVE_FLOAT128", "0");
        define_macro("__HAVE_FLOAT64X", "0");
        define_macro("__HAVE_FLOAT32", "0");
        define_macro("__HAVE_FLOAT64", "0");

        // glibc compatibility (C++ extern "C" wrappers)
        define_macro("__BEGIN_DECLS", "");
        define_macro("__END_DECLS", "");
        define_macro("__THROW", "");
        define_macro("__THROWNL", "");
        define_macro("__attribute_pure__", "");
        define_macro("__attribute_malloc__", "");
        define_macro("__wur", "");  // warn_unused_result

        // glibc compatibility - va_list support
        // __builtin_va_list is a GCC built-in, maps to void* for simplicity
        define_macro("__builtin_va_list", "void*");

        // glibc bits/types.h support
        define_macro("__STD_TYPE", "typedef");
        define_macro("__extension__", "");
        define_macro("inline", "");
        define_macro("__inline", "");
        define_macro("__inline__", "");
        define_macro("__restrict", "");
        define_macro("__restrict__", "");

        // glibc __REDIRECT macros - expand to just name proto, skip __asm__ alias
        if(0){
            __gshared str[3] redirect_params = ["name", "proto", "alias"];
            define_func_macro("__REDIRECT", redirect_params[], "name proto");
            define_func_macro("__REDIRECT_NTH", redirect_params[], "name proto");
            define_func_macro("__REDIRECT_NTHNL", redirect_params[], "name proto");
        }

        // Python API macros
        if(0){
            __gshared str[1] rtype_param = ["RTYPE"];
            define_func_macro("PyAPI_FUNC", rtype_param[], "extern RTYPE");
            define_func_macro("PyAPI_DATA", rtype_param[], "extern RTYPE");
            define_macro("PyMODINIT_FUNC", "void");
            __gshared str[1] version_param = ["VERSION"];
            define_func_macro("Py_DEPRECATED", version_param[], "");  // Expands to nothing
            __gshared str[1] attr_param = ["x"];
            define_func_macro("Py_GCC_ATTRIBUTE", attr_param[], "");  // Expands to nothing
            define_macro("_Py_NO_RETURN", "");  // __attribute__((noreturn))
        }

        // Skip math.h entirely (can't handle __CONCAT with ## token pasting)
        define_macro("_MATH_H", "1");
        // Skip ctype.h (complex ternary macros in enum values not worth the effort)
        define_macro("_CTYPE_H", "1");
        // Skip MMX/SSE intrinsic headers (can't handle __attribute__((vector_size)))
        define_macro("_MMINTRIN_H_INCLUDED", "1");
        define_macro("_XMMINTRIN_H_INCLUDED", "1");
        define_macro("_EMMINTRIN_H_INCLUDED", "1");
        define_macro("_PMMINTRIN_H_INCLUDED", "1");
        define_macro("_TMMINTRIN_H_INCLUDED", "1");
        define_macro("_SMMINTRIN_H_INCLUDED", "1");
        define_macro("_NMMINTRIN_H_INCLUDED", "1");
        define_macro("_IMMINTRIN_H_INCLUDED", "1");
        define_macro("_AVX512FINTRIN_H_INCLUDED", "1");

        // glibc type macros (from bits/typesizes.h)
        define_macro("__S16_TYPE", "short");
        define_macro("__U16_TYPE", "unsigned short");
        define_macro("__S32_TYPE", "int");
        define_macro("__U32_TYPE", "unsigned int");
        define_macro("__SLONGWORD_TYPE", "long");
        define_macro("__ULONGWORD_TYPE", "unsigned long");
        define_macro("__SQUAD_TYPE", "long long");
        define_macro("__UQUAD_TYPE", "unsigned long long");
        define_macro("__SWORD_TYPE", "long");
        define_macro("__UWORD_TYPE", "unsigned long");
        define_macro("__SLONG32_TYPE", "int");
        define_macro("__ULONG32_TYPE", "unsigned int");
        define_macro("__S64_TYPE", "long long");
        define_macro("__U64_TYPE", "unsigned long long");

        // Type aliases used in bits/types.h
        define_macro("__DEV_T_TYPE", "unsigned long long");
        define_macro("__UID_T_TYPE", "unsigned int");
        define_macro("__GID_T_TYPE", "unsigned int");
        define_macro("__INO_T_TYPE", "unsigned long");
        define_macro("__INO64_T_TYPE", "unsigned long long");
        define_macro("__MODE_T_TYPE", "unsigned int");
        define_macro("__NLINK_T_TYPE", "unsigned long");
        define_macro("__OFF_T_TYPE", "long");
        define_macro("__OFF64_T_TYPE", "long long");
        define_macro("__PID_T_TYPE", "int");
        define_macro("__RLIM_T_TYPE", "unsigned long");
        define_macro("__RLIM64_T_TYPE", "unsigned long long");
        define_macro("__BLKCNT_T_TYPE", "long");
        define_macro("__BLKCNT64_T_TYPE", "long long");
        define_macro("__FSBLKCNT_T_TYPE", "unsigned long");
        define_macro("__FSBLKCNT64_T_TYPE", "unsigned long long");
        define_macro("__FSFILCNT_T_TYPE", "unsigned long");
        define_macro("__FSFILCNT64_T_TYPE", "unsigned long long");
        define_macro("__ID_T_TYPE", "unsigned int");
        define_macro("__CLOCK_T_TYPE", "long");
        define_macro("__TIME_T_TYPE", "long");
        define_macro("__USECONDS_T_TYPE", "unsigned int");
        define_macro("__SUSECONDS_T_TYPE", "long");
        define_macro("__DADDR_T_TYPE", "int");
        define_macro("__KEY_T_TYPE", "int");
        define_macro("__CLOCKID_T_TYPE", "int");
        define_macro("__TIMER_T_TYPE", "void*");
        define_macro("__BLKSIZE_T_TYPE", "long");
        define_macro("__SSIZE_T_TYPE", "long");
        define_macro("__SYSCALL_SLONG_TYPE", "long");
        define_macro("__SYSCALL_ULONG_TYPE", "unsigned long");
        define_macro("__CPU_MASK_TYPE", "unsigned long");

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
        fprintf(stderr, "%.*s:%d:%d: Tokenize Error: %.*s\n",
                cast(int)current_file.length, current_file.ptr,
                line, column, cast(int)message.length, message.ptr);
    }

    bool at_end() {
        if (current >= source.length) {
            // Don't pop include stack during macro evaluation
            if (in_macro_evaluation) {
                return true;
            }
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
                case '\f':  // Form feed
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
                fprintf(stderr, "%.*s:%d:%d: c = 0x%x\n",
                        cast(int)current_file.length, current_file.ptr,
                        line, column, cast(uint)c);
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
        } else if (directive == "error") {
            // Get the error message and report it
            while (peek == ' ' || peek == '\t') advance();
            int msg_start = current;
            skip_to_eol();
            str msg = cast(str)source[msg_start .. current];
            ERROR_OCCURRED = true;
            fprintf(stderr, "%.*s:%d: #error %.*s\n",
                    cast(int)current_file.length, current_file.ptr,
                    line, cast(int)msg.length, msg.ptr);
        } else {
            // Skip other preprocessor directives (warning, line, etc.)
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
            fprintf(stderr, "Warning: Cannot find include file: %.*s\n", cast(int)filename.length, filename.ptr);
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

        auto file_result = read_file(path_buf.borrow().ptr, allocator, FileFlags.NUL_TERMINATE | FileFlags.ZERO_PAD_TO_16);
        if (file_result.errored) {
            fprintf(stderr, "Warning: Cannot read include file: %.*s\n", cast(int)full_path.length, full_path.ptr);
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
        // Skip to end of line, handling line continuations and block comments
        while (!at_end) {
            if (peek == '\\' && peekNext == '\n') {
                advance(); // skip backslash
                advance(); // skip newline
                line++;
                column = 1;
                continue;
            }
            // Handle block comments that may span multiple lines
            if (peek == '/' && peekNext == '*') {
                advance(); advance();  // skip /*
                while (!at_end) {
                    if (peek == '*' && peekNext == '/') {
                        advance(); advance();  // skip */
                        break;
                    }
                    if (peek == '\n') {
                        line++;
                        column = 0;
                    }
                    advance();
                }
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

            bool defined = is_macro_defined(macro_name);
            push_cond(defined);
        }
        else if (directive == "ifndef") {
            while (peek == ' ' || peek == '\t') advance();
            int name_start = current;
            while (peek.is_ident_char) advance();
            str macro_name = cast(str)source[name_start .. current];
            skip_to_eol();

            bool defined = is_macro_defined(macro_name);
            push_cond(!defined);
        }
        else if (directive == "if") {
            bool result = evaluate_pp_expression();
            skip_to_eol();
            push_cond(result);
        }
        else if (directive == "elif") {
            if (cond_stack.count == 0) {
                skip_to_eol();
                error("#elif without #if");
                return;
            }
            auto top = &cond_stack[cond_stack.count - 1];
            if (top.seen_else) {
                skip_to_eol();
                error("#elif after #else");
                return;
            }
            // If any previous branch was taken, skip this one
            if (top.condition_met) {
                skip_to_eol();
                top.currently_active = false;
            } else {
                // Evaluate #elif expression
                bool result = evaluate_pp_expression();
                skip_to_eol();
                if (result && is_parent_active()) {
                    top.currently_active = true;
                    top.condition_met = true;
                } else {
                    top.currently_active = false;
                }
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
    // Supports: defined(X), defined X, ||, &&, ==, !=, <, <=, >, >=, !, numbers, ()
    bool evaluate_pp_expression() {
        skip_pp_ws();
        return eval_pp_or() != 0;
    }

    long eval_pp_or() {
        long result = eval_pp_and();
        while (true) {
            skip_pp_ws();
            if (peek == '|' && peekNext == '|') {
                advance(); advance();
                long right = eval_pp_and();
                result = (result != 0 || right != 0) ? 1 : 0;
            } else {
                break;
            }
        }
        return result;
    }

    long eval_pp_and() {
        long result = eval_pp_equality();
        while (true) {
            skip_pp_ws();
            if (peek == '&' && peekNext == '&') {
                advance(); advance();
                long right = eval_pp_equality();
                result = (result != 0 && right != 0) ? 1 : 0;
            } else {
                break;
            }
        }
        return result;
    }

    long eval_pp_equality() {
        long result = eval_pp_relational();
        while (true) {
            skip_pp_ws();
            if (peek == '=' && peekNext == '=') {
                advance(); advance();
                long right = eval_pp_relational();
                result = (result == right) ? 1 : 0;
            } else if (peek == '!' && peekNext == '=') {
                advance(); advance();
                long right = eval_pp_relational();
                result = (result != right) ? 1 : 0;
            } else {
                break;
            }
        }
        return result;
    }

    long eval_pp_relational() {
        long result = eval_pp_additive();
        while (true) {
            skip_pp_ws();
            if (peek == '<' && peekNext == '=') {
                advance(); advance();
                long right = eval_pp_additive();
                result = (result <= right) ? 1 : 0;
            } else if (peek == '>' && peekNext == '=') {
                advance(); advance();
                long right = eval_pp_additive();
                result = (result >= right) ? 1 : 0;
            } else if (peek == '<') {
                advance();
                long right = eval_pp_additive();
                result = (result < right) ? 1 : 0;
            } else if (peek == '>') {
                advance();
                long right = eval_pp_additive();
                result = (result > right) ? 1 : 0;
            } else {
                break;
            }
        }
        return result;
    }

    long eval_pp_additive() {
        long result = eval_pp_multiplicative();
        while (true) {
            skip_pp_ws();
            if (peek == '+') {
                advance();
                result = result + eval_pp_multiplicative();
            } else if (peek == '-') {
                advance();
                result = result - eval_pp_multiplicative();
            } else {
                break;
            }
        }
        return result;
    }

    long eval_pp_multiplicative() {
        long result = eval_pp_unary();
        while (true) {
            skip_pp_ws();
            if (peek == '*') {
                advance();
                result = result * eval_pp_unary();
            } else if (peek == '/') {
                advance();
                long right = eval_pp_unary();
                result = (right != 0) ? result / right : 0;
            } else if (peek == '%') {
                advance();
                long right = eval_pp_unary();
                result = (right != 0) ? result % right : 0;
            } else {
                break;
            }
        }
        return result;
    }

    long eval_pp_unary() {
        skip_pp_ws();
        if (peek == '!') {
            advance();
            return eval_pp_unary() == 0 ? 1 : 0;
        }
        return eval_pp_primary();
    }

    long eval_pp_primary() {
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
            return is_macro_defined(name) ? 1 : 0;
        }

        // Parenthesized expression
        if (peek == '(') {
            advance();
            long result = eval_pp_or();
            skip_pp_ws();
            if (peek == ')') advance();
            return result;
        }

        // Number literal
        if (peek.is_digit) {
            long value = 0;
            while (peek.is_digit) {
                value = value * 10 + (advance() - '0');
            }
            // Skip suffixes
            while (peek == 'L' || peek == 'l' || peek == 'U' || peek == 'u') advance();
            return value;
        }

        // Identifier - check for defined macro value, otherwise 0
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
            // Note: Using iteration with str_eq due to betterC slice comparison bug
            foreach (item; defines.items) {
                if (str_eq(item.key, name) && !item.value.is_undefined) {
                    return eval_macro_replacement(item.value.replacement);
                }
            }
            return 0;  // Undefined identifier = 0
        }

        return 0;
    }

    // Evaluate a macro replacement string as a preprocessor expression
    long eval_macro_replacement(str replacement) {
        // Save current position
        int saved_current = current;
        const(ubyte)[] saved_source = source;
        bool saved_in_macro_evaluation = in_macro_evaluation;

        // Temporarily switch to evaluating the replacement
        source = cast(const(ubyte)[])replacement;
        current = 0;
        in_macro_evaluation = true;  // Prevent include stack pops

        long result = eval_pp_or();

        // Restore
        source = saved_source;
        current = saved_current;
        in_macro_evaluation = saved_in_macro_evaluation;

        return result;
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
            advance();  // consume (

            // Parse parameter list
            Barray!str params;
            params.bdata.allocator = allocator;
            bool is_variadic = false;

            while (!at_end && peek != ')') {
                // Skip whitespace and line continuations
                while (peek == ' ' || peek == '\t' || (peek == '\\' && peekNext == '\n')) {
                    if (peek == '\\') {
                        advance(); advance();
                        line++; column = 1;
                    } else {
                        advance();
                    }
                }

                if (peek == ')') break;

                // Bare newline in macro params - malformed, bail out
                if (peek == '\n') break;

                // Handle variadic ...
                if (peek == '.' && peekNext == '.') {
                    advance(); advance();
                    if (peek == '.') advance();
                    is_variadic = true;
                    // Skip whitespace
                    while (peek == ' ' || peek == '\t') advance();
                    if (peek == ',') advance();
                    continue;
                }

                // Get parameter name
                int param_start = current;
                while (peek.is_ident_char) advance();
                str param = cast(str)source[param_start .. current];
                if (param.length > 0) {
                    params ~= param;
                }

                // Skip whitespace after param
                while (peek == ' ' || peek == '\t' || (peek == '\\' && peekNext == '\n')) {
                    if (peek == '\\') {
                        advance(); advance();
                        line++; column = 1;
                    } else {
                        advance();
                    }
                }

                // Skip comma if present
                if (peek == ',') advance();
            }

            if (peek == ')') advance();  // consume )

            // Skip whitespace before replacement
            while (peek == ' ' || peek == '\t') advance();

            // Get replacement text
            int value_start = current;
            int value_end = current;
            bool in_comment = false;

            while (!at_end) {
                if (!in_comment && peek == '/' && peekNext == '*') {
                    if (!in_comment) value_end = current;
                    in_comment = true;
                    advance(); advance();
                    continue;
                }
                if (in_comment && peek == '*' && peekNext == '/') {
                    in_comment = false;
                    advance(); advance();
                    continue;
                }
                if (in_comment) {
                    if (peek == '\n') { line++; column = 0; }
                    advance();
                    continue;
                }
                if (peek == '\n') break;
                if (peek == '\\' && peekNext == '\n') {
                    advance(); advance();
                    line++; column = 1;
                    value_end = current;
                    continue;
                }
                advance();
                value_end = current;
            }

            str value = cast(str)source[value_start .. value_end];
            while (value.length > 0 && (value[$ - 1] == ' ' || value[$ - 1] == '\t')) {
                value = value[0 .. $ - 1];
            }

            // Store function-like macro - don't overwrite predefined
            if ((macro_name in defines) is null) {
                define_func_macro(macro_name, params[], value, is_variadic);
            }
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
            define_macro(macro_name, value);
        }
    }

    void do_undef() {
        while (peek == ' ' || peek == '\t') advance();
        int name_start = current;
        while (peek.is_ident_char) advance();
        str macro_name = cast(str)source[name_start .. current];
        skip_to_eol();
        // Mark the macro as undefined
        if (MacroDef* def = macro_name in defines) {
            def.is_undefined = true;
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

        // Check for macro substitution (prevent recursive expansion)
        if (MacroDef* macro_def = text in defines) {
            // Skip if macro was #undef'd
            if (macro_def.is_undefined) {
                // Not a macro anymore, fall through to keyword/identifier handling
            } else {
                if (bool* expanding = text in expanding_macros) {
                    if (*expanding) {
                        // Macro is currently being expanded - emit as identifier to prevent recursion
                        add_token(CTokenType.IDENTIFIER);
                        return;
                    }
                }

                // Handle function-like macros
            if (macro_def.is_function_like) {
                // Must have ( after identifier
                // Skip whitespace first
                while (peek == ' ' || peek == '\t') advance();
                if (peek != '(') {
                    // Not a macro invocation, just the name
                    add_token(CTokenType.IDENTIFIER);
                    return;
                }
                advance();  // consume (

                // Parse arguments
                str[] args = parse_macro_args();

                // Substitute parameters in replacement text
                str expanded = substitute_macro_params(macro_def.replacement, macro_def.params, args, macro_def.is_variadic);

                // Track that we're expanding this macro
                expanding_macros[text] = true;
                scope(exit) expanding_macros[text] = false;
                // Tokenize the expansion
                tokenize_macro_replacement(expanded);
                return;
            }

            str value = macro_def.replacement;
            if (value.length == 0) {
                // Macro expands to nothing - don't emit any token
                return;
            }
            // Track that we're expanding this macro
            expanding_macros[text] = true;
            scope(exit) expanding_macros[text] = false;
            // Tokenize the replacement text and emit those tokens
            tokenize_macro_replacement(value);
            return;
            }
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
            case "__signed":   type = CTokenType.SIGNED; break;
            case "__signed__":   type = CTokenType.SIGNED; break;
            case "const":    type = CTokenType.CONST; break;
            case "__const":    type = CTokenType.CONST; break;
            case "__const__":    type = CTokenType.CONST; break;
            case "volatile": type = CTokenType.VOLATILE; break;
            case "__volatile": type = CTokenType.VOLATILE; break;
            case "__volatile__": type = CTokenType.VOLATILE; break;
            case "static":   type = CTokenType.STATIC; break;
            case "extern":   type = CTokenType.EXTERN; break;
            case "struct":   type = CTokenType.STRUCT; break;
            case "union":    type = CTokenType.UNION; break;
            case "enum":     type = CTokenType.ENUM; break;
            case "typedef":  type = CTokenType.TYPEDEF; break;
            case "sizeof":          type = CTokenType.SIZEOF; break;
            case "_Static_assert":  type = CTokenType.STATIC_ASSERT; break;
            case "asm":             type = CTokenType.ASM; break;
            case "__asm":           type = CTokenType.ASM; break;
            case "__asm__":         type = CTokenType.ASM; break;
            case "_Float16":        type = CTokenType.FLOAT16; break;
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

    // Parse macro arguments (after the opening paren has been consumed)
    str[] parse_macro_args() {
        import dlib.stringbuilder : StringBuilder;
        import core.stdc.string : memcpy;

        Barray!str args;
        args.bdata.allocator = allocator;

        StringBuilder current_arg;
        current_arg.allocator = allocator;

        int paren_depth = 1;

        while (!at_end && paren_depth > 0) {
            ubyte c = peek;

            if (c == '(') {
                paren_depth++;
                current_arg.write(cast(char)c);
                advance();
            } else if (c == ')') {
                paren_depth--;
                if (paren_depth == 0) {
                    // End of arguments
                    advance();  // consume )
                    break;
                }
                current_arg.write(cast(char)c);
                advance();
            } else if (c == ',' && paren_depth == 1) {
                // Argument separator - copy the string before reset
                str arg = current_arg.borrow();
                // Trim leading/trailing whitespace
                while (arg.length > 0 && (arg[0] == ' ' || arg[0] == '\t')) arg = arg[1 .. $];
                while (arg.length > 0 && (arg[$ - 1] == ' ' || arg[$ - 1] == '\t')) arg = arg[0 .. $ - 1];
                // Allocate a copy
                char* copy = cast(char*)allocator.alloc(arg.length).ptr;
                memcpy(copy, arg.ptr, arg.length);
                args ~= cast(str)copy[0 .. arg.length];
                current_arg.reset();
                advance();
            } else if (c == '\n') {
                line++;
                column = 0;
                advance();
            } else {
                current_arg.write(cast(char)c);
                advance();
            }
        }

        // Add the last argument
        str last = current_arg.borrow();
        // Trim leading/trailing whitespace
        while (last.length > 0 && (last[0] == ' ' || last[0] == '\t')) last = last[1 .. $];
        while (last.length > 0 && (last[$ - 1] == ' ' || last[$ - 1] == '\t')) last = last[0 .. $ - 1];
        if (last.length > 0 || args.count > 0) {
            // Allocate a copy for the last arg too
            char* copy = cast(char*)allocator.alloc(last.length).ptr;
            memcpy(copy, last.ptr, last.length);
            args ~= cast(str)copy[0 .. last.length];
        }

        return args[];
    }

    // Parse macro arguments from a text slice (for nested macro expansion)
    // Returns args array and updates idx to point past the closing paren
    str[] parse_macro_args_from_text(str text, ref size_t idx) {
        import dlib.stringbuilder : StringBuilder;
        import core.stdc.string : memcpy;

        Barray!str args;
        args.bdata.allocator = allocator;

        StringBuilder current_arg;
        current_arg.allocator = allocator;

        int paren_depth = 1;

        while (idx < text.length && paren_depth > 0) {
            ubyte c = text[idx];

            if (c == '(') {
                paren_depth++;
                current_arg.write(cast(char)c);
                idx++;
            } else if (c == ')') {
                paren_depth--;
                if (paren_depth == 0) {
                    idx++;  // consume )
                    break;
                }
                current_arg.write(cast(char)c);
                idx++;
            } else if (c == ',' && paren_depth == 1) {
                // Argument separator
                str arg = current_arg.borrow();
                while (arg.length > 0 && (arg[0] == ' ' || arg[0] == '\t')) arg = arg[1 .. $];
                while (arg.length > 0 && (arg[$ - 1] == ' ' || arg[$ - 1] == '\t')) arg = arg[0 .. $ - 1];
                char* copy = cast(char*)allocator.alloc(arg.length).ptr;
                memcpy(copy, arg.ptr, arg.length);
                args ~= cast(str)copy[0 .. arg.length];
                current_arg.reset();
                idx++;
            } else {
                current_arg.write(cast(char)c);
                idx++;
            }
        }

        // Add the last argument
        str last = current_arg.borrow();
        while (last.length > 0 && (last[0] == ' ' || last[0] == '\t')) last = last[1 .. $];
        while (last.length > 0 && (last[$ - 1] == ' ' || last[$ - 1] == '\t')) last = last[0 .. $ - 1];
        if (last.length > 0 || args.count > 0) {
            char* copy = cast(char*)allocator.alloc(last.length).ptr;
            memcpy(copy, last.ptr, last.length);
            args ~= cast(str)copy[0 .. last.length];
        }

        return args[];
    }

    // Substitute macro parameters in replacement text
    str substitute_macro_params(str replacement, str[] params, str[] args, bool is_variadic = false) {
        import dlib.stringbuilder : StringBuilder;

        if (params.length == 0 && !is_variadic) return replacement;

        StringBuilder result;
        result.allocator = allocator;

        size_t i = 0;
        while (i < replacement.length) {
            // Check for token pasting ## - just copy it through, apply_token_pasting handles it later
            if (replacement[i] == '#' && i + 1 < replacement.length && replacement[i + 1] == '#') {
                result.write('#');
                result.write('#');
                i += 2;
                continue;
            }

            // Check for stringification operator #
            if (replacement[i] == '#') {
                i++;  // skip #
                // Skip whitespace after #
                while (i < replacement.length && (replacement[i] == ' ' || replacement[i] == '\t')) i++;
                // Get the parameter name
                if (i < replacement.length && (replacement[i].is_alpha || replacement[i] == '_')) {
                    size_t id_start = i;
                    while (i < replacement.length && replacement[i].is_ident_char) i++;
                    str ident = replacement[id_start .. i];

                    // Check for __VA_ARGS__ stringification
                    if (is_variadic && ident == "__VA_ARGS__") {
                        result.write('"');
                        size_t num_named = params.length;
                        bool first = true;
                        for (size_t ai = num_named; ai < args.length; ai++) {
                            if (!first) result.write(", ");
                            // TODO: properly escape the arg
                            result.write(args[ai]);
                            first = false;
                        }
                        result.write('"');
                        continue;
                    }

                    // Find matching parameter and stringify
                    bool found = false;
                    foreach (pi, param; params) {
                        if (param == ident && pi < args.length) {
                            result.write('"');
                            // TODO: properly escape the arg (quotes, backslashes)
                            result.write(args[pi]);
                            result.write('"');
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        // Not a parameter, emit # and identifier as-is
                        result.write('#');
                        result.write(ident);
                    }
                } else {
                    result.write('#');
                }
                continue;
            }

            // Check if this is the start of an identifier
            if (replacement[i].is_alpha || replacement[i] == '_') {
                size_t id_start = i;
                while (i < replacement.length && replacement[i].is_ident_char) i++;
                str ident = replacement[id_start .. i];

                // Check for __VA_ARGS__ in variadic macros
                if (is_variadic && ident == "__VA_ARGS__") {
                    // Concatenate all extra arguments (after named params) with commas
                    size_t num_named = params.length;
                    bool first = true;
                    for (size_t ai = num_named; ai < args.length; ai++) {
                        if (!first) result.write(", ");
                        result.write(args[ai]);
                        first = false;
                    }
                    continue;
                }

                // Check if this is a parameter
                bool found = false;
                foreach (pi, param; params) {
                    if (param == ident && pi < args.length) {
                        result.write(args[pi]);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    result.write(ident);
                }
            } else {
                result.write(cast(char)replacement[i]);
                i++;
            }
        }

        // Handle token pasting (##) - do a post-processing pass
        str intermediate = result.borrow();
        return apply_token_pasting(intermediate);
    }

    // Apply ## token pasting operator
    str apply_token_pasting(str text) {
        import dlib.stringbuilder : StringBuilder;

        // Look for ## in the text
        bool has_paste = false;
        for (size_t j = 0; j + 1 < text.length; j++) {
            if (text[j] == '#' && text[j + 1] == '#') {
                has_paste = true;
                break;
            }
        }
        if (!has_paste) return text;

        StringBuilder result;
        result.allocator = allocator;

        size_t i = 0;
        while (i < text.length) {
            // Check for ##
            if (i + 1 < text.length && text[i] == '#' && text[i + 1] == '#') {
                // Remove trailing whitespace from result
                while (result.cursor > 0) {
                    ubyte last = cast(ubyte)result.data[result.cursor - 1];
                    if (last == ' ' || last == '\t') {
                        result.erase(1);
                    } else {
                        break;
                    }
                }
                i += 2;  // skip ##
                // Skip leading whitespace after ##
                while (i < text.length && (text[i] == ' ' || text[i] == '\t')) i++;
                continue;
            }
            result.write(cast(char)text[i]);
            i++;
        }

        return result.borrow();
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

            // Check for ellipsis
            if (c == '.' && i + 2 < text.length && text[i + 1] == '.' && text[i + 2] == '.') {
                *tokens ~= CToken(CTokenType.ELLIPSIS, text[i .. i + 3], line, start_column, current_file);
                i += 3;
                continue;
            }

            // Two-character operators
            if (c == '<' && i + 1 < text.length && text[i + 1] == '<') {
                *tokens ~= CToken(CTokenType.LESS_LESS, text[i .. i + 2], line, start_column, current_file);
                i += 2;
                continue;
            }
            if (c == '>' && i + 1 < text.length && text[i + 1] == '>') {
                *tokens ~= CToken(CTokenType.GREATER_GREATER, text[i .. i + 2], line, start_column, current_file);
                i += 2;
                continue;
            }
            if (c == '=' && i + 1 < text.length && text[i + 1] == '=') {
                *tokens ~= CToken(CTokenType.EQUAL_EQUAL, text[i .. i + 2], line, start_column, current_file);
                i += 2;
                continue;
            }
            if (c == '!' && i + 1 < text.length && text[i + 1] == '=') {
                *tokens ~= CToken(CTokenType.BANG_EQUAL, text[i .. i + 2], line, start_column, current_file);
                i += 2;
                continue;
            }
            if (c == '<' && i + 1 < text.length && text[i + 1] == '=') {
                *tokens ~= CToken(CTokenType.LESS_EQUAL, text[i .. i + 2], line, start_column, current_file);
                i += 2;
                continue;
            }
            if (c == '>' && i + 1 < text.length && text[i + 1] == '=') {
                *tokens ~= CToken(CTokenType.GREATER_EQUAL, text[i .. i + 2], line, start_column, current_file);
                i += 2;
                continue;
            }
            if (c == '&' && i + 1 < text.length && text[i + 1] == '&') {
                *tokens ~= CToken(CTokenType.AMP_AMP, text[i .. i + 2], line, start_column, current_file);
                i += 2;
                continue;
            }
            if (c == '|' && i + 1 < text.length && text[i + 1] == '|') {
                *tokens ~= CToken(CTokenType.PIPE_PIPE, text[i .. i + 2], line, start_column, current_file);
                i += 2;
                continue;
            }

            // Single-character tokens
            if (c == '(' || c == ')' || c == '[' || c == ']' ||
                c == '{' || c == '}' || c == ',' || c == ';' ||
                c == '+' || c == '-' || c == '*' || c == '/' ||
                c == '%' || c == '&' || c == '|' || c == '^' ||
                c == '~' || c == '<' || c == '>' || c == '!' ||
                c == '=' || c == '.' || c == '?' || c == ':') {
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

                // Check for nested macro (prevent recursive expansion)
                if (MacroDef* nested = ident in defines) {
                    // Skip if macro was #undef'd
                    if (nested.is_undefined) {
                        // Not a macro anymore, emit as identifier
                        *tokens ~= CToken(CTokenType.IDENTIFIER, ident, line, start_column, current_file);
                        continue;
                    }
                    bool is_expanding = false;
                    if (bool* exp = ident in expanding_macros) {
                        is_expanding = *exp;
                    }
                    if (is_expanding) {
                        // Macro is currently being expanded - emit as identifier
                        *tokens ~= CToken(CTokenType.IDENTIFIER, ident, line, start_column, current_file);
                    } else if (!nested.is_function_like && nested.replacement.length > 0) {
                        expanding_macros[ident] = true;
                        scope(exit) expanding_macros[ident] = false;
                        tokenize_macro_replacement(nested.replacement);
                    } else if (!nested.is_function_like) {
                        // empty object-like macro - emit nothing
                    } else {
                        // Function-like macro - check for ( after
                        // Skip whitespace in text
                        while (i < text.length && (text[i] == ' ' || text[i] == '\t')) i++;
                        if (i < text.length && text[i] == '(') {
                            // Parse args from replacement text
                            i++;  // consume (
                            str[] func_args = parse_macro_args_from_text(text, i);
                            str expanded = substitute_macro_params(nested.replacement, nested.params, func_args, nested.is_variadic);
                            expanding_macros[ident] = true;
                            scope(exit) expanding_macros[ident] = false;
                            tokenize_macro_replacement(expanded);
                        } else {
                            // No parens - just emit as identifier
                            *tokens ~= CToken(CTokenType.IDENTIFIER, ident, line, start_column, current_file);
                        }
                    }
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
                        case "__signed":   type = CTokenType.SIGNED; break;
                        case "__signed__":   type = CTokenType.SIGNED; break;
                        case "const":    type = CTokenType.CONST; break;
                        case "__const":    type = CTokenType.CONST; break;
                        case "__const__":    type = CTokenType.CONST; break;
                        case "volatile": type = CTokenType.VOLATILE; break;
                        case "__volatile": type = CTokenType.VOLATILE; break;
                        case "__volatile__": type = CTokenType.VOLATILE; break;
                        case "static":   type = CTokenType.STATIC; break;
                        case "extern":   type = CTokenType.EXTERN; break;
                        case "struct":   type = CTokenType.STRUCT; break;
                        case "union":    type = CTokenType.UNION; break;
                        case "enum":     type = CTokenType.ENUM; break;
                        case "typedef":  type = CTokenType.TYPEDEF; break;
                        case "sizeof":   type = CTokenType.SIZEOF; break;
                        case "asm":      type = CTokenType.ASM; break;
                        case "__asm":    type = CTokenType.ASM; break;
                        case "__asm__":  type = CTokenType.ASM; break;
                        case "_Float16": type = CTokenType.FLOAT16; break;
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
