/*
 * C Preprocessor (Phase 4)
 * Processes directives and expands macros on token stream
 * Copyright 2025, David Priver
 */
module cfront.c_preprocessor;

import core.stdc.stdlib : getenv;
import core.stdc.time : time, localtime, tm;
import dlib.aliases;
import dlib.allocator : Allocator;
import dlib.barray : Barray, make_barray;
import dlib.table : Table;
import dlib.stringbuilder : StringBuilder, mwritef, E, Pad;
import dlib.file_util : FileFlags;
import dlib.box: Box, boxed;
import dlib.logger;
import dlib.file_cache : FileCache, FileError;
import cfront.c_pp_token;
import cfront.c_pp_lexer : pp_tokenize;

struct CPreprocessor {
    Allocator allocator;
    Logger* logger;
    FileCache* file_cache;

    // Macro table
    Table!(str, PPMacroDef) macros;

    // Conditional compilation stack
    Barray!PPCondBlock cond_stack;

    // Include handling
    Barray!PPIncludeFrame include_stack;
    Table!(str, bool) pragma_once_files;
    Table!(str, str) include_guards;  // filename -> guard macro name
    str[] include_paths;
    str[] framework_paths;  // -F paths for framework lookup
    str[] force_includes;  // -include files to process before main source
    Barray!str pushed_include_paths;  // paths pushed via #pragma include_path
    str current_file;
    str base_file;  // The root file being compiled (for __BASE_FILE__)

    bool error_occurred = false;

    // Magic macro state
    int counter_value = 0;  // For __COUNTER__
    Table!(str, int) named_counters;  // For __COUNTER__(name)
    uint random_state = 12345;  // For __RANDOM__ (simple LCG)
    str date_string;  // For __DATE__ - "Mmm dd yyyy"
    str time_string;  // For __TIME__ - "hh:mm:ss"

    // #line directive state
    int line_offset = 0;      // Added to token line numbers for __LINE__
    int line_offset_base = 0; // Token line where #line was issued
    str file_override;        // Override for __FILE__ (null = use token's file)

    // #pragma watch state
    enum WatchFlags : ubyte {
        NONE   = 0,
        DEFINE = 1 << 0,
        UNDEF  = 1 << 1,
        EXPAND = 1 << 2,
        ALL    = DEFINE | UNDEF | EXPAND,
    }
    Table!(str, ubyte) watched_macros;  // Macros being watched (flags indicate which events)

    // Current directive line (for error messages)
    int directive_line = 0;

    // Initialize with predefined macros
    void initialize(){
        macros.data.allocator = allocator;
        cond_stack = make_barray!PPCondBlock(allocator);
        include_stack = make_barray!PPIncludeFrame(allocator);
        pushed_include_paths = make_barray!str(allocator);
        pragma_once_files.data.allocator = allocator;
        include_guards.data.allocator = allocator;
        named_counters.data.allocator = allocator;
        watched_macros.data.allocator = allocator;

        // Set base_file from current_file (must be set before initialize())
        base_file = current_file;

        // TODO: time is a weak seed (second resolution, predictable)
        // Consider using /dev/urandom or mixing in pid/address
        auto now = time(null);
        random_state = cast(uint)now;

        // Initialize __DATE__ and __TIME__ strings
        auto tm_ptr = localtime(&now);
        if(tm_ptr !is null){
            static immutable string[12] months = [
                "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
            ];
            // Format: "Mmm dd yyyy" - note space before single-digit day
            int day = tm_ptr.tm_mday;
            int year = tm_ptr.tm_year + 1900;
            str month = months[tm_ptr.tm_mon];
            if(day < 10){
                date_string = mwritef(allocator, "\"%  % %\"", month, day, year)[];
            } else {
                date_string = mwritef(allocator, "\"% % %\"", month, day, year)[];
            }
            // Format: "hh:mm:ss"
            int hour = tm_ptr.tm_hour;
            int min = tm_ptr.tm_min;
            int sec = tm_ptr.tm_sec;
            time_string = mwritef(allocator, "\"%:%:%\"", Pad(hour, 2, '0'), Pad(min, 2, '0'), Pad(sec, 2, '0'))[];
        } else {
            // Fallback if localtime fails
            date_string = "\"??? ?? ????\"";
            time_string = "\"??:??:??\"";
        }

        // Define built-in macros
        define_builtin_macros();
    }

    void define_builtin_macros(){
        // Platform macros
        define_object_macro("__STDC__", "1");
        define_object_macro("__STDC_VERSION__", "201710L");
        // C23 embed result macros
        define_object_macro("__STDC_EMBED_NOT_FOUND__", "0");
        define_object_macro("__STDC_EMBED_FOUND__", "1");
        define_object_macro("__STDC_EMBED_EMPTY__", "2");
        define_object_macro("__FILE__", builtin:true);
        define_object_macro("__LINE__", builtin:true);
        define_object_macro("__COUNTER__", builtin:true);
        define_object_macro("__INCLUDE_DEPTH__", builtin:true);
        define_object_macro("__BASE_FILE__", builtin:true);
        define_object_macro("__DIR__", builtin:true);
        define_object_macro("__RANDOM__", builtin:true);
        define_object_macro("__DATE__", builtin:true);
        define_object_macro("__TIME__", builtin:true);

        // Detect LP64 (Unix-style 64-bit where long is 64 bits)
        // vs LLP64 (Windows 64-bit where long is 32 bits)
        version(Windows){
            // Windows uses LLP64: long is 32 bits, long long is 64 bits
            version(X86_64){
                enum IS_64BIT = true;
                enum IS_LP64 = false;  // Windows LLP64
            } else {
                enum IS_64BIT = false;
                enum IS_LP64 = false;
            }
        } else {
            // Unix-like systems use LP64: long is 64 bits on 64-bit platforms
            version(AArch64){
                enum IS_64BIT = true;
                enum IS_LP64 = true;
            } else version(X86_64){
                enum IS_64BIT = true;
                enum IS_LP64 = true;
            } else {
                enum IS_64BIT = false;
                enum IS_LP64 = false;
            }
        }

        // TODO: make this an arg so we can pretend to be various compilers.
        // GCC and Clang define these type macros; MSVC does not.
        // On Unix-like systems (Linux, macOS), we pretend to be GCC/Clang.
        version(Windows)
            enum DEFAULT_COMPILER_IS_GNU = false;  // MSVC doesn't define these
        else
            enum DEFAULT_COMPILER_IS_GNU = true;   // GCC on Linux, Clang on macOS

        if(DEFAULT_COMPILER_IS_GNU){
            // GCC/Clang type macros needed by system headers
            static if(IS_LP64){
                // LP64: long is 64 bits
                define_object_macro("__SIZE_TYPE__", "long unsigned int");
                define_object_macro("__PTRDIFF_TYPE__", "long int");
                define_object_macro("__SIZEOF_LONG__", "8");
                define_object_macro("__LP64__", "1");
                define_object_macro("__INTPTR_TYPE__", "long");
                define_object_macro("__UINTPTR_TYPE__", "unsigned long");
            } else static if(IS_64BIT){
                // LLP64 (Windows 64-bit): long is 32 bits, use long long for 64-bit types
                define_object_macro("__SIZE_TYPE__", "long long unsigned int");
                define_object_macro("__PTRDIFF_TYPE__", "long long int");
                define_object_macro("__SIZEOF_LONG__", "4");
                define_object_macro("__INTPTR_TYPE__", "long long");
                define_object_macro("__UINTPTR_TYPE__", "unsigned long long");
            } else {
                // 32-bit platforms
                define_object_macro("__SIZE_TYPE__", "unsigned int");
                define_object_macro("__PTRDIFF_TYPE__", "int");
                define_object_macro("__SIZEOF_LONG__", "4");
                define_object_macro("__INTPTR_TYPE__", "int");
                define_object_macro("__UINTPTR_TYPE__", "unsigned int");
            }
            static if(IS_64BIT){
                define_object_macro("__SIZEOF_POINTER__", "8");
            } else {
                define_object_macro("__SIZEOF_POINTER__", "4");
            }
            define_object_macro("__SIZEOF_INT__", "4");
            define_object_macro("__SIZEOF_SHORT__", "2");
            define_object_macro("__SIZEOF_LONG_LONG__", "8");

            // macros for stdint.h - exact-width types
            define_object_macro("__INT8_TYPE__", "signed char");
            define_object_macro("__INT16_TYPE__", "short int");
            define_object_macro("__INT32_TYPE__", "int");
            define_object_macro("__UINT8_TYPE__", "unsigned char");
            define_object_macro("__UINT16_TYPE__", "short unsigned int");
            define_object_macro("__UINT32_TYPE__", "unsigned int");

            // Character type macros (for stdatomic.h, wchar.h)
            define_object_macro("__CHAR16_TYPE__", "short unsigned int");
            define_object_macro("__CHAR32_TYPE__", "unsigned int");
            define_object_macro("__WCHAR_TYPE__", "int");
            define_object_macro("__WINT_TYPE__", "unsigned int");

            // Other type macros
            define_object_macro("__SIG_ATOMIC_TYPE__", "int");

            static if(IS_LP64){
                define_object_macro("__INT64_TYPE__", "long int");
                define_object_macro("__UINT64_TYPE__", "long unsigned int");
            }
            else{
                define_object_macro("__INT64_TYPE__", "long long int");
                define_object_macro("__UINT64_TYPE__", "long long unsigned int");
            }

            // least-width types
            define_object_macro("__INT_LEAST8_TYPE__", "signed char");
            define_object_macro("__INT_FAST8_TYPE__", "signed char");
            define_object_macro("__INT_LEAST16_TYPE__", "short");
            define_object_macro("__INT_FAST16_TYPE__", "short");
            define_object_macro("__INT_LEAST32_TYPE__", "int");
            define_object_macro("__INT_FAST32_TYPE__", "int");
            static if(IS_LP64){
                define_object_macro("__INT_LEAST64_TYPE__", "long");
                define_object_macro("__INT_FAST64_TYPE__", "long");
            }
            else{
                define_object_macro("__INT_LEAST64_TYPE__", "long long");
                define_object_macro("__INT_FAST64_TYPE__", "long long");
            }
            define_object_macro("__UINT_LEAST8_TYPE__", "unsigned char");
            define_object_macro("__UINT_FAST8_TYPE__", "unsigned char");
            define_object_macro("__UINT_LEAST16_TYPE__", "unsigned short");
            define_object_macro("__UINT_FAST16_TYPE__", "unsigned short");
            define_object_macro("__UINT_LEAST32_TYPE__", "unsigned");
            define_object_macro("__UINT_FAST32_TYPE__", "unsigned");
            static if(IS_LP64){
                define_object_macro("__UINT_LEAST64_TYPE__", "unsigned long");
                define_object_macro("__UINT_FAST64_TYPE__", "unsigned long");
            }
            else {
                define_object_macro("__UINT_LEAST64_TYPE__", "unsigned long long");
                define_object_macro("__UINT_FAST64_TYPE__", "unsigned long long");
            }
            define_object_macro("__INTMAX_TYPE__", "long long");
            define_object_macro("__UINTMAX_TYPE__", "unsigned long long");

            // TODO: the other ones
            define_object_macro("__INT_MAX__", "2147483648");
            define_object_macro("__FLT_MIN__", "1.17549435e-38F");
            define_object_macro("__DBL_MIN__", "2.2250738585072014e-308");
            define_object_macro("__LDBL_MIN__", "2.2250738585072014e-308");
            define_object_macro("__LONG_LONG_MAX__", "9223372036854775808ll");
        }

        // OS detection
        version(OSX){
            define_object_macro("__APPLE__", "1");
            define_object_macro("__MACH__", "1");
            define_object_macro("__APPLE_CC__", "1");  // For TargetConditionals.h
            define_object_macro("MAC_OS_X_VERSION_MIN_REQUIRED", "110000");  // macOS 11.0
            define_object_macro("MAC_OS_X_VERSION_MAX_ALLOWED", "150000");  // macOS 15.0
            define_object_macro("__DARWIN_CTYPE_NO_INLINE", "1");
            define_object_macro("_DONT_USE_CTYPE_INLINE_", "1");
            define_object_macro("__FINITE_MATH_ONLY__", "1");
        } else version(linux){
            define_object_macro("__linux__", "1");
            define_object_macro("__linux", "1");
            define_object_macro("__unix__", "1");
        } else version(Windows){
            define_object_macro("_WIN32", "1");
            define_object_macro("_WIN64", "1");
        }

        // Architecture detection
        version(X86_64){
            define_object_macro("__x86_64__", "1");
            define_object_macro("__amd64__", "1");
        } else version(AArch64){
            define_object_macro("__aarch64__", "1");
            define_object_macro("__arm64__", "1");
        } else version(X86){
            define_object_macro("__i386__", "1");
        } else version(ARM){
            define_object_macro("__arm__", "1");
        }

        // GCC compatibility (claim GCC 9.4 for better header support)
        define_object_macro("__GNUC__", "9");
        define_object_macro("__GNUC_MINOR__", "4");
        define_object_macro("__GNUC_PATCHLEVEL__", "0");

        // Floating-point evaluation method (C99 5.2.4.2.2)
        // 0 = evaluate each type at its own precision
        define_object_macro("__FLT_EVAL_METHOD__", "0");

        // DDASM compiler identification
        define_object_macro("__DDASM__", "1");

        // Common empty macros
        define_object_macro("__extension__", "");
        define_object_macro("volatile", "");
        define_object_macro("__volatile__", "");

        // User label prefix - empty on Linux/ELF, underscore on macOS
        version(OSX){
            define_object_macro("__USER_LABEL_PREFIX__", "_");
        } else {
            define_object_macro("__USER_LABEL_PREFIX__", "");
        }

        // Compiler feature checks - always return 0 (not supported)
        define_func_macro_const("__has_feature", 1, "0");
        define_func_macro_const("__has_extension", 1, "0");
        define_func_macro_const("__has_builtin", 1, "0");
        define_func_macro_const("__has_attribute", 1, "0");
        // These are supported actually, but we dont' have builtin
        // function macro.
        define_func_macro_const("__has_include", 1, "0");
        define_func_macro_const("__has_include_next", 1, "0");
        {
            static immutable str[2] params = ["cond", "p"];
            define_func_macro("__builtin_expect", cast(str[])params[], "(cond)");
        }
        define_func_macro("__builtin_debug_trap", null, "__dasm{debug}");
        define_func_macro("__builtin_trap", null, "__dasm{debug}");

        // Compiler built-in types
        define_object_macro("__builtin_va_list", "void*");

        // Atomic memory order constants
        define_object_macro("__ATOMIC_RELAXED", "0");
        define_object_macro("__ATOMIC_CONSUME", "1");
        define_object_macro("__ATOMIC_ACQUIRE", "2");
        define_object_macro("__ATOMIC_RELEASE", "3");
        define_object_macro("__ATOMIC_ACQ_REL", "4");
        define_object_macro("__ATOMIC_SEQ_CST", "5");

    }

    // Define a simple object-like macro from a string
    void define_object_macro(str name, str value="", bool builtin=false){
        PPMacroDef def;
        def.name = name;
        def.is_function_like = false;
        def.is_variadic = false;
        def.is_undefined = false;
        def.is_builtin = builtin;

        // Tokenize the value
        if(value.length > 0){
            Barray!PPToken value_tokens = make_barray!PPToken(allocator);
            pp_tokenize(cast(const(ubyte)[])value, "<builtin>", &value_tokens, allocator);
            // Remove EOF and newline tokens, but keep whitespace for proper spacing
            Barray!PPToken filtered = make_barray!PPToken(allocator);
            foreach(tok; value_tokens[]){
                if(tok.type != PPTokenType.PP_EOF && tok.type != PPTokenType.PP_NEWLINE){
                    filtered ~= tok;
                }
            }
            def.replacement = filtered[];
        } else {
            def.replacement = null;
        }

        macros[name] = def;
    }

    // Define a function-like macro that expands to nothing
    void define_empty_func_macro(str name, int num_params){
        PPMacroDef def;
        def.name = name;
        def.is_function_like = true;
        def.is_variadic = false;
        def.is_undefined = false;
        def.replacement = null;  // Empty expansion

        // Create dummy parameter names
        Barray!str params = make_barray!str(allocator);
        for(int i = 0; i < num_params; i++){
            params ~= "_";  // Dummy param name
        }
        def.params = params[];

        macros[name] = def;
    }

    void define_func_macro(str name, str[] params, str value){
        PPMacroDef def;
        def.name = name;
        def.is_function_like = true;
        def.is_variadic = false;
        def.is_undefined = false;
        def.params = params;
        // Tokenize the value
        if(value.length > 0){
            Barray!PPToken value_tokens = make_barray!PPToken(allocator);
            pp_tokenize(cast(const(ubyte)[])value, "<builtin>", &value_tokens, allocator);
            // Remove EOF and newline tokens, but keep whitespace for proper spacing
            Barray!PPToken filtered = make_barray!PPToken(allocator);
            foreach(tok; value_tokens[]){
                if(tok.type != PPTokenType.PP_EOF && tok.type != PPTokenType.PP_NEWLINE){
                    filtered ~= tok;
                }
            }
            def.replacement = filtered[];
        } else {
            def.replacement = null;
        }

        macros[name] = def;
    }

    // Define a function-like macro that expands to a constant value
    void define_func_macro_const(str name, int num_params, str value){
        PPMacroDef def;
        def.name = name;
        def.is_function_like = true;
        def.is_variadic = false;
        def.is_undefined = false;

        // Create dummy parameter names
        Barray!str params = make_barray!str(allocator);
        for(int i = 0; i < num_params; i++){
            params ~= "_";  // Dummy param name
        }
        def.params = params[];

        // Create replacement token
        Barray!PPToken repl = make_barray!PPToken(allocator);
        PPToken tok;
        tok.type = PPTokenType.PP_NUMBER;
        tok.lexeme = value;
        repl ~= tok;
        def.replacement = repl[];

        macros[name] = def;
    }

    // Check if a macro is defined and not #undef'd
    bool is_defined(str name){
        if(auto p = name in macros){
            return !p.is_undefined;
        }
        return false;
    }
    PPMacroDef get_file_macro(PPToken tok){
        PPToken file_tok = tok;
        file_tok.type = PPTokenType.PP_STRING;
        // Use expansion_file if from macro expansion, else file_override if set, else current_file
        str filename;
        if(tok.expansion_file.length > 0)
            filename = tok.expansion_file;
        else if(file_override.length > 0)
            filename = file_override;
        else
            filename = current_file;
        file_tok.lexeme = mwritef(allocator, "\"%\"", E(filename))[];
        Box!PPToken box = boxed(allocator, &file_tok);
        return PPMacroDef(tok.lexeme, box[], is_builtin:true);
    }
    PPMacroDef get_line_macro(PPToken tok){
        PPToken line_tok = tok;
        line_tok.type = PPTokenType.PP_NUMBER;
        // Use expansion_line if from macro expansion, else tok.line
        int base_line = tok.expansion_file.length > 0 ? tok.expansion_line : tok.line;
        int effective_line = base_line + line_offset;
        line_tok.lexeme = mwritef(allocator, "%", effective_line)[];
        Box!PPToken box = boxed(allocator, &line_tok);
        return PPMacroDef(tok.lexeme, box[], is_builtin:true);
    }

    PPMacroDef get_counter_macro(PPToken tok){
        PPToken counter_tok = tok;
        counter_tok.type = PPTokenType.PP_NUMBER;
        counter_tok.lexeme = mwritef(allocator, "%", counter_value)[];
        counter_value++;
        Box!PPToken box = boxed(allocator, &counter_tok);
        return PPMacroDef(tok.lexeme, box[], is_builtin:true);
    }

    PPMacroDef get_include_depth_macro(PPToken tok){
        PPToken depth_tok = tok;
        depth_tok.type = PPTokenType.PP_NUMBER;
        depth_tok.lexeme = mwritef(allocator, "%", include_stack.count)[];
        Box!PPToken box = boxed(allocator, &depth_tok);
        return PPMacroDef(tok.lexeme, box[], is_builtin:true);
    }

    PPMacroDef get_base_file_macro(PPToken tok){
        PPToken file_tok = tok;
        file_tok.type = PPTokenType.PP_STRING;
        file_tok.lexeme = mwritef(allocator, "\"%\"", E(base_file))[];
        Box!PPToken box = boxed(allocator, &file_tok);
        return PPMacroDef(tok.lexeme, box[], is_builtin:true);
    }

    PPMacroDef get_dir_macro(PPToken tok){
        PPToken dir_tok = tok;
        dir_tok.type = PPTokenType.PP_STRING;
        str dir = get_directory(current_file);
        if(dir.length == 0) dir = ".";
        dir_tok.lexeme = mwritef(allocator, "\"%\"", E(dir))[];
        Box!PPToken box = boxed(allocator, &dir_tok);
        return PPMacroDef(tok.lexeme, box[], is_builtin:true);
    }

    PPMacroDef get_random_macro(PPToken tok){
        PPToken rand_tok = tok;
        rand_tok.type = PPTokenType.PP_NUMBER;
        // Simple LCG random number generator
        random_state = random_state * 1103515245 + 12345;
        uint rand_val = (random_state >> 16) & 0x7FFF;
        rand_tok.lexeme = mwritef(allocator, "%", rand_val)[];
        Box!PPToken box = boxed(allocator, &rand_tok);
        return PPMacroDef(tok.lexeme, box[], is_builtin:true);
    }

    PPMacroDef get_date_macro(PPToken tok){
        PPToken date_tok = tok;
        date_tok.type = PPTokenType.PP_STRING;
        date_tok.lexeme = date_string;
        Box!PPToken box = boxed(allocator, &date_tok);
        return PPMacroDef(tok.lexeme, box[], is_builtin:true);
    }

    PPMacroDef get_time_macro(PPToken tok){
        PPToken time_tok = tok;
        time_tok.type = PPTokenType.PP_STRING;
        time_tok.lexeme = time_string;
        Box!PPToken box = boxed(allocator, &time_tok);
        return PPMacroDef(tok.lexeme, box[], is_builtin:true);
    }

    // Handle __EXPAND__(string-literal) - destringify and tokenize
    size_t handle_expand_builtin(PPToken[] tokens, size_t start, Barray!PPToken* output){
        size_t i = start + 1;  // Skip __EXPAND__

        // Skip whitespace
        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

        // Expect (
        if(i >= tokens.length || !tokens[i].is_punct("(")){
            error("__EXPAND__ requires parentheses");
            return start + 1;
        }
        i++;

        // Skip whitespace
        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

        // Collect argument tokens until )
        Barray!PPToken arg_tokens = make_barray!PPToken(allocator);
        int paren_depth = 1;
        while(i < tokens.length && paren_depth > 0){
            if(tokens[i].is_punct("(")) paren_depth++;
            else if(tokens[i].is_punct(")")){
                paren_depth--;
                if(paren_depth == 0) break;
            }
            arg_tokens ~= tokens[i];
            i++;
        }

        // Skip closing )
        if(i < tokens.length && tokens[i].is_punct(")")) i++;

        // Expand macros and builtins in argument first
        Barray!PPToken expanded_arg = make_barray!PPToken(allocator);
        HideSet env_hs = HideSet.create(allocator);
        expand_tokens(arg_tokens[], &expanded_arg, env_hs);

        // Find the string literal in expanded argument
        str string_content = "";
        foreach(tok; expanded_arg[]){
            if(tok.type == PPTokenType.PP_STRING){
                str s = tok.lexeme;
                // Remove quotes
                if(s.length >= 2 && s[0] == '"' && s[$-1] == '"'){
                    string_content = s[1..$-1];
                }
                break;
            }
        }

        if(string_content.length == 0){
            // No string found, nothing to expand
            return i;
        }

        // Destringify: handle escape sequences
        StringBuilder destringified;
        destringified.allocator = allocator;
        size_t j = 0;
        while(j < string_content.length){
            if(string_content[j] == '\\' && j + 1 < string_content.length){
                char next = string_content[j + 1];
                if(next == '"'){
                    destringified.write("\"");
                    j += 2;
                } else if(next == '\\'){
                    destringified.write("\\");
                    j += 2;
                } else if(next == 'n'){
                    destringified.write("\n");
                    j += 2;
                } else if(next == 't'){
                    destringified.write("\t");
                    j += 2;
                } else {
                    destringified.write(string_content[j..j+1]);
                    j++;
                }
            } else {
                destringified.write(string_content[j..j+1]);
                j++;
            }
        }

        // Tokenize the destringified content
        str content = destringified.borrow();
        Barray!PPToken new_tokens = make_barray!PPToken(allocator);
        pp_tokenize(cast(const(ubyte)[])content, "<expand>", &new_tokens, allocator);

        // Output the tokens (skip EOF if present)
        foreach(tok; new_tokens[]){
            if(tok.type != PPTokenType.PP_EOF &&
               tok.type != PPTokenType.PP_NEWLINE){
                *output ~= tok;
            }
        }

        return i;
    }

    // Handle __ENV__(name) or __ENV__(name, "default") - get environment variable as string
    size_t handle_env_builtin(PPToken[] tokens, size_t start, Barray!PPToken* output){
        size_t i = start + 1;  // Skip __ENV__

        // Skip whitespace
        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

        // Expect (
        if(i >= tokens.length || !tokens[i].is_punct("(")){
            error("__ENV__ requires parentheses");
            return start + 1;
        }
        i++;

        // Skip whitespace
        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

        // Get the environment variable name (string)
        str env_name = "";
        if(i < tokens.length && tokens[i].type == PPTokenType.PP_STRING){
            str s = tokens[i].lexeme;
            // Remove quotes
            if(s.length >= 2 && s[0] == '"' && s[$-1] == '"'){
                env_name = s[1..$-1];
            }
            i++;
        } else {
            error("__ENV__ requires string as first argument");
            return i;
        }

        // Skip whitespace
        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

        // Check for optional default value (comma followed by string)
        str default_value = "";
        bool has_default = false;
        if(i < tokens.length && tokens[i].is_punct(",")){
            i++;
            while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

            if(i < tokens.length && tokens[i].type == PPTokenType.PP_STRING){
                str s = tokens[i].lexeme;
                // Remove quotes
                if(s.length >= 2 && s[0] == '"' && s[$-1] == '"'){
                    default_value = s[1..$-1];
                }
                has_default = true;
                i++;
            }
        }

        // Skip whitespace and expect )
        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;
        if(i < tokens.length && tokens[i].is_punct(")")) i++;

        // Get the environment variable value
        // Need to null-terminate the name for getenv
        StringBuilder name_buf;
        name_buf.allocator = allocator;
        name_buf.write(env_name);
        name_buf.nul_terminate();

        const(char)* env_val = getenv(name_buf.borrow().ptr);

        str result_value;
        if(env_val !is null){
            // Convert C string to D string
            size_t len = 0;
            while(env_val[len] != 0) len++;
            result_value = cast(str)env_val[0..len];
        } else if(has_default){
            result_value = default_value;
        } else {
            result_value = "";
        }

        // Output as string literal token
        PPToken str_tok;
        str_tok.type = PPTokenType.PP_STRING;
        str_tok.lexeme = mwritef(allocator, "\"%\"", E(result_value))[];
        str_tok.file = current_file;
        *output ~= str_tok;

        return i;
    }

    // Handle __COUNTER__(name) - named counter streams
    size_t handle_counter_builtin(PPToken[] tokens, size_t start, Barray!PPToken* output){
        size_t i = start + 1;  // Skip __COUNTER__

        // Must have opening paren
        if(i >= tokens.length || !tokens[i].is_punct("(")){
            // No paren - fall back to normal __COUNTER__ expansion
            // Return start to let normal macro expansion handle it
            return start;
        }
        i++;  // Skip (

        // Skip whitespace
        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

        // Get the counter name (must be identifier)
        if(i >= tokens.length || tokens[i].type != PPTokenType.PP_IDENTIFIER){
            error("__COUNTER__(name) requires identifier");
            return i;
        }
        str counter_name = tokens[i].lexeme;
        i++;

        // Skip whitespace
        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

        // Must have closing paren
        if(i >= tokens.length || !tokens[i].is_punct(")")){
            error("__COUNTER__(name) missing closing paren");
            return i;
        }
        i++;  // Skip )

        // Get or create the named counter
        int value;
        if(auto p = counter_name in named_counters){
            value = *p;
            *p = value + 1;
        } else {
            value = 0;
            named_counters[counter_name] = 1;
        }

        // Output as number token
        PPToken num_tok;
        num_tok.type = PPTokenType.PP_NUMBER;
        num_tok.lexeme = mwritef(allocator, "%", value)[];
        num_tok.file = current_file;
        *output ~= num_tok;

        return i;
    }

    // Get macro definition
    PPMacroDef get_macro(PPToken tok){
        // Handle special predefined macros
        str name = tok.lexeme;

        // Quick check: builtin macros all start with "__" and are 7+ chars
        // This avoids string comparisons for most identifiers
        if(name.length >= 7 && name[0] == '_' && name[1] == '_'){
            // Switch on third char and length for fast dispatch
            switch(name[2]){
                case 'F':
                    if(name == "__FILE__") return get_file_macro(tok);
                    break;
                case 'L':
                    if(name == "__LINE__") return get_line_macro(tok);
                    break;
                case 'C':
                    if(name == "__COUNTER__") return get_counter_macro(tok);
                    break;
                case 'I':
                    if(name == "__INCLUDE_DEPTH__") return get_include_depth_macro(tok);
                    break;
                case 'B':
                    if(name == "__BASE_FILE__") return get_base_file_macro(tok);
                    break;
                case 'D':
                    if(name == "__DIR__") return get_dir_macro(tok);
                    if(name == "__DATE__") return get_date_macro(tok);
                    break;
                case 'R':
                    if(name == "__RANDOM__") return get_random_macro(tok);
                    break;
                case 'T':
                    if(name == "__TIME__") return get_time_macro(tok);
                    break;
                default:
                    break;
            }
        }

        if(auto value = name in macros){
            if(value.is_undefined) return PPMacroDef(is_null:true);
            return *value;
        }
        return PPMacroDef(is_null:true);
    }

    // Check if current block is active
    bool is_active(){
        if(cond_stack.count == 0) return true;
        return cond_stack[cond_stack.count - 1].currently_active;
    }

    // Check if parent block is active
    bool is_parent_active(){
        if(cond_stack.count <= 1) return true;
        return cond_stack[cond_stack.count - 2].currently_active;
    }

    // Push conditional block
    void push_cond(bool active){
        PPCondBlock block;
        block.condition_met = active;
        block.currently_active = active && is_active();
        block.seen_else = false;
        cond_stack ~= block;
    }

    // Report error
    void error(A...)(A msg){
        error_occurred = true;
        logger.error(current_file, ':', directive_line, ": Preprocessor error: ", current_file, directive_line, msg, '\n');
    }

    void error_at(A...)(PPToken tok, A msg){
        error_occurred = true;
        logger.error(tok.file, ':', tok.line, ':', tok.column, ": Preprocessor error: ", msg, '\n');
    }

    // Main entry point: process PPTokens
    int process(PPToken[] input, Barray!PPToken* output){
        // Process forced includes first (like -include flag)
        // Clear immediately to prevent re-processing in recursive calls
        str[] pending_includes = force_includes;
        force_includes = [];
        foreach(inc_file; pending_includes){
            process_force_include(inc_file, output);
            if(error_occurred) return 1;
        }

        size_t i = 0;
        bool at_line_start = true;  // Track line-start state as we iterate

        while(i < input.length){
            PPToken tok = input[i];

            // Update line-start tracking based on token type
            // (do this before processing so we have correct state for directive check)
            bool current_at_line_start = at_line_start;
            if(tok.type == PPTokenType.PP_NEWLINE){
                at_line_start = true;
            } else if(tok.type != PPTokenType.PP_WHITESPACE){
                at_line_start = false;
            }
            // Whitespace doesn't change line-start state

            // Check for directive at start of line (# as first non-whitespace)
            if(current_at_line_start && is_hash(input, i)){
                i = process_directive(input, i, output);
                at_line_start = true;  // After directive, we're at a new line start
                if(error_occurred) return 1;
                continue;
            }

            tok = input[i];

            // Skip tokens in inactive blocks
            if(!is_active()){
                i++;
                continue;
            }

            // Handle identifiers - check for macro expansion
            if(tok.type == PPTokenType.PP_IDENTIFIER){
                // Handle __EXPAND__(string-literal) - destringify
                if(tok.lexeme == "__EXPAND__"){
                    i = handle_expand_builtin(input, i, output);
                    continue;
                }

                // Handle __ENV__(name) or __ENV__(name, "default")
                if(tok.lexeme == "__ENV__"){
                    i = handle_env_builtin(input, i, output);
                    continue;
                }

                // Handle __COUNTER__(name) - named counter streams
                // Falls through to normal __COUNTER__ if no parens
                if(tok.lexeme == "__COUNTER__"){
                    size_t new_i = handle_counter_builtin(input, i, output);
                    if(new_i != i){
                        i = new_i;
                        continue;
                    }
                    // No parens - fall through to normal macro expansion
                }

                // Handle _Pragma operator
                if(tok.lexeme == "_Pragma"){
                    size_t new_i = handle_pragma_operator(input, i, output);
                    if(new_i != i){
                        i = new_i;
                        continue;
                    }
                }

                PPMacroDef macro_def = get_macro(tok);
                if(!macro_def.is_null){
                    HideSet hs = HideSet.create(allocator);
                    // Expand to temp buffer, then process for builtins
                    Barray!PPToken expanded = make_barray!PPToken(allocator);
                    i = expand_macro_invocation(input, i, macro_def, hs, &expanded);
                    // Pass hide set with macro name to prevent re-expansion of hidden macros
                    expand_tokens(expanded[], output, hs.with_hidden(macro_def.name));
                    continue;
                }
            }

            // Pass through other tokens
            *output ~= tok;
            i++;
        }

        return 0;
    }

    // Check if we're at the start of a line (only whitespace before this point on the line)
    bool is_at_line_start(PPToken[] tokens, size_t pos){
        if(pos == 0) return true;
        // Walk backwards to see if there's only whitespace since last newline
        for(size_t j = pos; j > 0; j--){
            PPToken prev = tokens[j - 1];
            if(prev.type == PPTokenType.PP_NEWLINE) return true;
            if(prev.type != PPTokenType.PP_WHITESPACE) return false;
        }
        return true;  // Start of file
    }

    // Check if current token is #
    bool is_hash(PPToken[] tokens, size_t pos){
        // Skip leading whitespace
        while(pos < tokens.length && tokens[pos].type == PPTokenType.PP_WHITESPACE){
            pos++;
        }
        if(pos >= tokens.length) return false;
        return tokens[pos].type == PPTokenType.PP_PUNCTUATOR && tokens[pos].matches("#");
    }

    // Find end of current line
    size_t find_line_end(PPToken[] tokens, size_t pos){
        while(pos < tokens.length && tokens[pos].type != PPTokenType.PP_NEWLINE){
            pos++;
        }
        return pos;
    }

    // Process a preprocessor directive
    size_t process_directive(PPToken[] tokens, size_t start, Barray!PPToken* output){
        size_t i = start;

        // Track directive line for error messages
        directive_line = tokens.length > start ? tokens[start].line : 0;

        // Skip whitespace
        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

        // Skip #
        if(i >= tokens.length || !tokens[i].is_punct("#")){
            return find_line_end(tokens, start) + 1;
        }
        i++;

        // Skip whitespace after #
        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

        // Empty directive (just #)
        if(i >= tokens.length || tokens[i].type == PPTokenType.PP_NEWLINE){
            return i + 1;
        }

        // Get directive name
        if(tokens[i].type != PPTokenType.PP_IDENTIFIER){
            // Not a known directive, skip line
            return find_line_end(tokens, start) + 1;
        }

        str directive = tokens[i].lexeme;
        i++;

        // Find end of line
        size_t line_end = find_line_end(tokens, i);

        // Extract tokens for this directive (excluding newline)
        PPToken[] line_tokens = tokens[i .. line_end];

        // Conditional directives must be processed even in inactive blocks
        if(directive == "if" || directive == "ifdef" ||
            directive == "ifndef" || directive == "elif" ||
            directive == "elifdef" || directive == "elifndef" ||
            directive == "else" || directive == "endif"){
            handle_conditional(directive, line_tokens);
            return line_end + 1;
        }

        // Other directives only processed in active blocks
        if(!is_active()){
            return line_end + 1;
        }

        if(directive == "define"){
            handle_define(line_tokens);
        } else if(directive == "undef"){
            handle_undef(line_tokens);
        } else if(directive == "include"){
            return handle_include(tokens, i, output);
        } else if(directive == "pragma"){
            handle_pragma(line_tokens, output);
        } else if(directive == "error"){
            handle_error(line_tokens);
        } else if(directive == "warning"){
            handle_warning(line_tokens);
        } else if(directive == "line"){
            handle_line(line_tokens);
        } else if(directive == "embed"){
            handle_embed(line_tokens, output);
        } else {
            // Unknown directive - ignore
        }

        return line_end + 1;
    }

    // Handle conditional directives
    void handle_conditional(str directive, PPToken[] line){
        if(directive == "ifdef"){
            // Skip whitespace
            size_t i = 0;
            while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;
            if(i >= line.length || line[i].type != PPTokenType.PP_IDENTIFIER){
                error("#ifdef requires identifier");
                push_cond(false);
                return;
            }
            bool defined = is_defined(line[i].lexeme);
            push_cond(defined);
        }
        else if(directive == "ifndef"){
            size_t i = 0;
            while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;
            if(i >= line.length || line[i].type != PPTokenType.PP_IDENTIFIER){
                error("#ifndef requires identifier");
                push_cond(false);
                return;
            }
            bool defined = is_defined(line[i].lexeme);
            push_cond(!defined);
        }
        else if(directive == "if"){
            if(!is_active()){
                // Parent not active, just push inactive
                push_cond(false);
                return;
            }
            long value = evaluate_expression(line);
            push_cond(value != 0);
        }
        else if(directive == "elif"){
            if(cond_stack.count == 0){
                error("#elif without #if");
                return;
            }
            auto top = &cond_stack[cond_stack.count - 1];
            if(top.seen_else){
                error("#elif after #else");
                return;
            }
            if(top.condition_met){
                // A previous branch was taken
                top.currently_active = false;
            } else {
                // Evaluate this branch
                if(is_parent_active()){
                    long value = evaluate_expression(line);
                    if(value != 0){
                        top.currently_active = true;
                        top.condition_met = true;
                    } else {
                        top.currently_active = false;
                    }
                } else {
                    top.currently_active = false;
                }
            }
        }
        else if(directive == "elifdef"){
            if(cond_stack.count == 0){
                error("#elifdef without #if");
                return;
            }
            auto top = &cond_stack[cond_stack.count - 1];
            if(top.seen_else){
                error("#elifdef after #else");
                return;
            }
            if(top.condition_met){
                top.currently_active = false;
            } else {
                if(is_parent_active()){
                    // Get identifier
                    size_t i = 0;
                    while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;
                    if(i >= line.length || line[i].type != PPTokenType.PP_IDENTIFIER){
                        error("#elifdef requires identifier");
                        return;
                    }
                    bool defined = is_defined(line[i].lexeme);
                    if(defined){
                        top.currently_active = true;
                        top.condition_met = true;
                    } else {
                        top.currently_active = false;
                    }
                } else {
                    top.currently_active = false;
                }
            }
        }
        else if(directive == "elifndef"){
            if(cond_stack.count == 0){
                error("#elifndef without #if");
                return;
            }
            auto top = &cond_stack[cond_stack.count - 1];
            if(top.seen_else){
                error("#elifndef after #else");
                return;
            }
            if(top.condition_met){
                top.currently_active = false;
            } else {
                if(is_parent_active()){
                    // Get identifier
                    size_t i = 0;
                    while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;
                    if(i >= line.length || line[i].type != PPTokenType.PP_IDENTIFIER){
                        error("#elifndef requires identifier");
                        return;
                    }
                    bool defined = is_defined(line[i].lexeme);
                    if(!defined){
                        top.currently_active = true;
                        top.condition_met = true;
                    } else {
                        top.currently_active = false;
                    }
                } else {
                    top.currently_active = false;
                }
            }
        }
        else if(directive == "else"){
            if(cond_stack.count == 0){
                error("#else without #if");
                return;
            }
            auto top = &cond_stack[cond_stack.count - 1];
            if(top.seen_else){
                error("duplicate #else");
                return;
            }
            top.seen_else = true;
            if(top.condition_met){
                top.currently_active = false;
            } else {
                top.currently_active = is_parent_active();
                top.condition_met = true;
            }
        }
        else if(directive == "endif"){
            if(cond_stack.count == 0){
                error("#endif without #if");
                return;
            }
            cond_stack.count--;
        }
    }

    // Handle #define
    void handle_define(PPToken[] line){
        size_t i = 0;

        // Skip whitespace
        while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;

        if(i >= line.length || line[i].type != PPTokenType.PP_IDENTIFIER){
            error("#define requires macro name");
            return;
        }

        str name = line[i].lexeme;
        i++;

        PPMacroDef def;
        def.name = name;
        def.is_undefined = false;
        def.def_file = current_file;
        def.def_line = line.length > 0 ? line[0].line : 0;

        // Check for function-like macro (immediately followed by '(')
        if(i < line.length && line[i].type == PPTokenType.PP_PUNCTUATOR && line[i].matches("(")){
            def.is_function_like = true;
            i++;  // skip (

            // Parse parameters
            Barray!str params = make_barray!str(allocator);

            while(i < line.length){
                // Skip whitespace
                while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;
                if(i >= line.length) break;

                // Check for )
                if(line[i].is_punct(")")){
                    i++;
                    break;
                }

                // Check for ... (ellipsis - tokenized as single "..." punctuator)
                if(line[i].is_punct("...")){
                    def.is_variadic = true;
                    i++;
                    // Skip whitespace and expect )
                    while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;
                    if(i < line.length && line[i].is_punct(")")) i++;
                    break;
                }

                // Parameter name
                if(line[i].type == PPTokenType.PP_IDENTIFIER){
                    params ~= line[i].lexeme;
                    i++;

                    // Skip whitespace
                    while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;

                    // Check for comma or )
                    if(i < line.length && line[i].is_punct(",")){
                        i++;
                    }
                } else {
                    break;
                }
            }
            def.params = params[];
        } else {
            def.is_function_like = false;
            def.params = null;
            // Skip leading whitespace for object-like macro
            while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;
        }

        // Rest is replacement list
        Barray!PPToken replacement = make_barray!PPToken(allocator);
        while(i < line.length){
            replacement ~= line[i];
            i++;
        }

        // Trim trailing whitespace
        while(replacement.count > 0 &&
               replacement[replacement.count - 1].type == PPTokenType.PP_WHITESPACE){
            replacement.count--;
        }

        def.replacement = replacement[];
        macros[name] = def;

        // Log if this macro is being watched for define
        if(auto p = name in watched_macros){ if(*p & WatchFlags.DEFINE){
            int def_line = line.length > 0 ? line[0].line : 0;
            logger.buff.FORMAT(current_file, ':', def_line, ": [watch] #define ", name);
            if(def.is_function_like){
                logger.buff.write("(");
                foreach(pi, param; def.params){
                    if(pi > 0) logger.buff.write(", ");
                    logger.buff.write(param);
                }
                if(def.is_variadic) logger.buff.write(", ...");
                logger.buff.write(")");
            }
            logger.buff.write(" ");
            foreach(tok; def.replacement){
                logger.buff.write(tok.lexeme);
            }
            logger.buff.write('\n');
            logger.flush(LogLevel.INFO);
        }}
    }

    // Handle #undef
    void handle_undef(PPToken[] line){
        size_t i = 0;
        while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;
        if(i >= line.length || line[i].type != PPTokenType.PP_IDENTIFIER){
            error("#undef requires macro name");
            return;
        }
        str name = line[i].lexeme;
        int undef_line = line.length > 0 ? line[0].line : 0;

        // Log if this macro is being watched for undef
        if(ubyte* p = name in watched_macros){ if(*p & WatchFlags.UNDEF){
            logger.info(current_file, ':', undef_line, ": [watch] #undef ", name, '\n');
        }}

        // Mark as undefined
        if(PPMacroDef* p = name in macros){
            p.is_undefined = true;
        }
    }

    // Process a forced include file (like -include flag)
    void process_force_include(str filename, Barray!PPToken* output){
        // Read file
        const(ubyte)[] file_data;
        if(file_cache.read_file(filename, file_data) != FileError.OK){
            logger.error("-include: error reading '", E(filename), "'\n");
            error_occurred = true;
            return;
        }

        Barray!PPToken include_tokens = make_barray!PPToken(allocator);
        int err = pp_tokenize(file_data, filename, &include_tokens, allocator);
        if(err){
            error_occurred = true;
            return;
        }

        // Remove EOF token from included file (if present)
        if(include_tokens.count > 0 && include_tokens[include_tokens.count - 1].type == PPTokenType.PP_EOF){
            include_tokens.count--;
        }

        // Detect and record include guard pattern
        str guard = detect_include_guard(include_tokens[]);
        if(guard.length > 0){
            include_guards[filename] = guard;
        }

        // Save current file and push include stack
        str saved_file = current_file;
        current_file = filename;
        PPIncludeFrame frame;
        frame.filename = filename;
        include_stack ~= frame;

        // Process included tokens recursively
        process(include_tokens[], output);

        // Restore current file and pop include stack
        include_stack.count--;
        current_file = saved_file;
    }

    // Handle #include
    size_t handle_include(PPToken[] tokens, size_t start, Barray!PPToken* output){
        size_t i = start;

        // Skip whitespace
        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

        if(i >= tokens.length){
            error("#include requires filename");
            return find_line_end(tokens, start) + 1;
        }

        str filename;
        bool is_system = false;

        // Check for <...> or "..."
        if(tokens[i].type == PPTokenType.PP_PUNCTUATOR && tokens[i].matches("<")){
            // <header>
            is_system = true;
            i++;
            StringBuilder sb;
            sb.allocator = allocator;
            while(i < tokens.length && !tokens[i].is_punct(">") &&
                   tokens[i].type != PPTokenType.PP_NEWLINE){
                sb.write(tokens[i].lexeme);
                i++;
            }
            if(i < tokens.length && tokens[i].is_punct(">")) i++;
            filename = sb.borrow();
        } else if(tokens[i].type == PPTokenType.PP_STRING){
            // "header"
            str s = tokens[i].lexeme;
            if(s.length >= 2){
                filename = s[1 .. $ - 1];  // Remove quotes
            }
            i++;
        } else {
            // Might be macro-expanded
            // For now, skip
            return find_line_end(tokens, start) + 1;
        }

        size_t line_end = find_line_end(tokens, i);

        // Resolve and process include
        str full_path = resolve_include(filename, is_system);
        if(full_path.length == 0){
            // Header not found
            logger.error(current_file, ':', tokens[start].line, ": error: '", E(filename), "' file not found'\n");
            error_occurred = true;
            return line_end + 1;
        }

        // Check for already included (pragma once)
        if(full_path in pragma_once_files){
            if(0)logger.debugf("Skipping due to pragma once: '%'\n", E(full_path));
            return line_end + 1;
        }

        // Check for include guard - if we know the guard macro and it's defined, skip
        if(str* guard = full_path in include_guards){
            if(is_defined(*guard)){
                if(0)logger.debugf("Skipping '%' due to include guard '%'\n", E(full_path), *guard);
                return line_end + 1;
            }
        }

        // Read file
        const(ubyte)[] file_data;
        if(file_cache.read_file(full_path, file_data) != FileError.OK){
            logger.error(current_file, ':', tokens[start].line, ": error: cannot read '", E(full_path), "'\n");
            error_occurred = true;
            return line_end + 1;
        }

        Barray!PPToken include_tokens = make_barray!PPToken(allocator);
        int err = pp_tokenize(file_data, full_path, &include_tokens, allocator);
        if(err){
            return line_end + 1;
        }

        // Remove EOF token from included file (if present)
        if(include_tokens.count > 0 && include_tokens[include_tokens.count - 1].type == PPTokenType.PP_EOF){
            include_tokens.count--;
        }

        // Detect and record include guard pattern before processing
        // (must detect before processing since #define will modify macro table)
        str guard = detect_include_guard(include_tokens[]);
        if(guard.length > 0){
            include_guards[full_path] = guard;
        }

        // Save current file and push include stack
        str saved_file = current_file;
        current_file = full_path;
        PPIncludeFrame frame;
        frame.filename = full_path;
        include_stack ~= frame;

        // Process included tokens recursively
        process(include_tokens[], output);

        // Restore current file and pop include stack
        include_stack.count--;
        current_file = saved_file;

        return line_end + 1;
    }

    // Resolve include path
    str resolve_include(str filename, bool is_system){
        // Handle absolute paths
        if(filename.length > 0 && filename[0] == '/'){
            if(path_exists(filename)) return filename;
            return "";
        }

        // For non-system includes, try relative to current file first
        if(!is_system && current_file.length > 0){
            // Extract directory from current file
            str dir = get_directory(current_file);
            if(dir.length > 0){
                str path = concat_path(dir, filename);
                if(path_exists(path)) return path;
            }
        }

        // Try pushed include paths first (in reverse order - last pushed = highest priority)
        for(size_t i = pushed_include_paths.count; i > 0; i--){
            str path = concat_path(pushed_include_paths[i-1], filename);
            if(path_exists(path)) return path;
        }

        version(OSX){
            // Try framework paths (e.g., Foo/Bar.h -> Foo.framework/Headers/Bar.h)
            str framework_result = resolve_framework_include(filename);
            if(framework_result.length > 0) return framework_result;
        }

        // Try configured include paths
        foreach(base; include_paths){
            str path = concat_path(base, filename);
            if(path_exists(path)) return path;
        }

        return "";
    }

    // Try to resolve an include as a framework include
    // e.g., "CoreFoundation/CFBase.h" -> "/System/Library/Frameworks/CoreFoundation.framework/Headers/CFBase.h"
    str resolve_framework_include(str filename){
        // Find first '/' to split framework name from rest of path
        size_t slash_pos = 0;
        bool found_slash = false;
        foreach(i, c; filename){
            if(c == '/'){
                slash_pos = i;
                found_slash = true;
                break;
            }
        }
        if(!found_slash || slash_pos == 0) return "";

        str framework_name = filename[0 .. slash_pos];
        str rest_of_path = filename[slash_pos + 1 .. $];

        // Try each framework path
        foreach(base; framework_paths){
            // Build: <base>/<framework_name>.framework/Headers/<rest_of_path>
            StringBuilder sb;
            sb.allocator = allocator;
            sb.write(base);
            sb.write('/');
            sb.write(framework_name);
            sb.write(".framework/Headers/");
            sb.write(rest_of_path);
            str path = sb.borrow();
            if(path_exists(path)) return path;
        }

        return "";
    }

    // Get directory part of path
    str get_directory(str path){
        for(size_t i = path.length; i > 0; i--){
            if(path[i-1] == '/'){
                return path[0 .. i-1];
            }
        }
        return "";
    }

    // Concatenate path components
    str concat_path(str dir, str file){
        StringBuilder sb;
        sb.allocator = allocator;
        sb.write(dir);
        sb.write('/');
        sb.write(file);
        return sb.borrow();
    }

    // Check if file exists (cached)
    bool path_exists(str path){
        return file_cache.is_file(path) == FileError.OK;
    }

    // Detect include guard pattern: #ifndef GUARD / #define GUARD ... #endif
    // Returns the guard macro name if detected, or empty string if not
    str detect_include_guard(PPToken[] tokens){
        if(tokens.length == 0) return "";

        size_t i = 0;

        // Skip leading whitespace/newlines
        while(i < tokens.length && (tokens[i].type == PPTokenType.PP_WHITESPACE ||
                                      tokens[i].type == PPTokenType.PP_NEWLINE)) i++;

        // Must start with #
        if(i >= tokens.length || tokens[i].type != PPTokenType.PP_PUNCTUATOR ||
            tokens[i].lexeme != "#") return "";
        i++;

        // Skip whitespace
        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

        // Must be "ifndef"
        if(i >= tokens.length || tokens[i].type != PPTokenType.PP_IDENTIFIER || tokens[i].lexeme != "ifndef") return "";
        i++;

        // Skip whitespace
        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

        // Get guard macro name
        if(i >= tokens.length || tokens[i].type != PPTokenType.PP_IDENTIFIER) return "";
        str guard_name = tokens[i].lexeme;
        i++;

        // Skip to next line
        while(i < tokens.length && tokens[i].type != PPTokenType.PP_NEWLINE) i++;
        if(i < tokens.length) i++; // skip newline

        // Skip whitespace
        while(i < tokens.length && (tokens[i].type == PPTokenType.PP_WHITESPACE ||
                                      tokens[i].type == PPTokenType.PP_NEWLINE)) i++;

        // Must have # define GUARD_NAME
        if(i >= tokens.length || tokens[i].type != PPTokenType.PP_PUNCTUATOR ||
            tokens[i].lexeme != "#") return "";
        i++;

        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

        if(i >= tokens.length || tokens[i].type != PPTokenType.PP_IDENTIFIER || tokens[i].lexeme != "define") return "";
        i++;

        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

        if(i >= tokens.length || tokens[i].type != PPTokenType.PP_IDENTIFIER || tokens[i].lexeme != guard_name) return "";

        // Now check that file ends with #endif(possibly with trailing whitespace/newlines)
        size_t j = tokens.length;
        while(j > 0){
            j--;
            if(tokens[j].type != PPTokenType.PP_WHITESPACE &&
                tokens[j].type != PPTokenType.PP_NEWLINE &&
                tokens[j].type != PPTokenType.PP_EOF) break;
        }

        // Should be "endif"
        if(tokens[j].type != PPTokenType.PP_IDENTIFIER || tokens[j].lexeme != "endif") return "";
        if(j == 0) return "";
        j--;

        // Skip whitespace before endif
        while(j > 0 && tokens[j].type == PPTokenType.PP_WHITESPACE) j--;

        // Should be #
        if(tokens[j].type != PPTokenType.PP_PUNCTUATOR || tokens[j].lexeme != "#") return "";

        return guard_name;
    }

    // Handle #pragma
    void handle_pragma(PPToken[] line, Barray!PPToken* output){
        size_t i = 0;
        while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;

        if(i < line.length && line[i].type == PPTokenType.PP_IDENTIFIER){
            if(line[i].lexeme == "once"){
                pragma_once_files[current_file] = true;
            } else if(line[i].lexeme == "expand"){
                // #pragma expand(tokens) - print macro expansion for debugging
                i++;
                while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;

                // Collect remaining tokens
                Barray!PPToken tokens_to_expand = make_barray!PPToken(allocator);
                while(i < line.length && line[i].type != PPTokenType.PP_NEWLINE){
                    if(line[i].type != PPTokenType.PP_WHITESPACE || tokens_to_expand.count > 0){
                        tokens_to_expand ~= line[i];
                    }
                    i++;
                }

                int pragma_line = line.length > 0 ? line[0].line : 0;
                logger.buff.FORMAT(current_file, ':', pragma_line, ": ");

                // Write original tokens
                foreach(tok; tokens_to_expand[]){
                    logger.buff.write(tok.lexeme);
                }

                logger.buff.write(" -> ");

                // Expand macros and write expanded tokens
                Barray!PPToken expanded = make_barray!PPToken(allocator);
                HideSet pragma_hs = HideSet.create(allocator);
                expand_tokens(tokens_to_expand[], &expanded, pragma_hs);
                foreach(tok; expanded[]){
                    logger.buff.write(tok.lexeme);
                }

                logger.buff.write('\n');
                logger.flush(LogLevel.INFO);
            } else if(line[i].lexeme == "eval"){
                // #pragma eval(tokens) - evaluate expression for debugging
                i++;
                while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;

                // Collect remaining tokens
                Barray!PPToken tokens_to_eval = make_barray!PPToken(allocator);
                while(i < line.length && line[i].type != PPTokenType.PP_NEWLINE){
                    if(line[i].type != PPTokenType.PP_WHITESPACE || tokens_to_eval.count > 0){
                        tokens_to_eval ~= line[i];
                    }
                    i++;
                }

                // Evaluate expression
                long value = evaluate_expression(tokens_to_eval[]);

                int pragma_line = line.length > 0 ? line[0].line : 0;
                logger.buff.FORMAT(current_file, ":", pragma_line, ": ");
                foreach(tok; tokens_to_eval[]){
                    logger.buff.write(tok.lexeme);
                }
                logger.buff.FORMAT(" = ", value, "\n");
                logger.flush(LogLevel.INFO);

            } else if(line[i].lexeme == "reveal"){
                // #pragma reveal(macroname) - show macro definition
                i++;
                while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;

                // Skip optional (
                if(i < line.length && line[i].is_punct("(")) i++;
                while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;

                if(i < line.length && line[i].type == PPTokenType.PP_IDENTIFIER){
                    str macro_name = line[i].lexeme;
                    PPMacroDef macro_def = get_macro(line[i]);

                    int pragma_line = line.length > 0 ? line[0].line : 0;

                    if(macro_def.is_null || macro_def.is_undefined){
                        logger.info(current_file, ":", pragma_line, ": ", macro_name, " is not defined\n");
                    } else if(macro_def.is_builtin){
                        logger.info(current_file, ":", pragma_line, ": ", macro_name, " = <builtin>\n");
                    } else {
                        // Build #define string directly to logger buffer
                        logger.buff.FORMAT(macro_def.def_file, ":", macro_def.def_line, ": #define ", macro_name);

                        if(macro_def.is_function_like){
                            logger.buff.write("(");
                            foreach(pi, param; macro_def.params){
                                if(pi > 0) logger.buff.write(", ");
                                logger.buff.write(param);
                            }
                            if(macro_def.is_variadic) logger.buff.write(", ...");
                            logger.buff.write(")");
                        }

                        logger.buff.write(" ");
                        foreach(tok; macro_def.replacement){
                            logger.buff.write(tok.lexeme);
                        }

                        logger.buff.write("\n");
                        logger.flush(LogLevel.INFO);
                    }
                }

            } else if(line[i].lexeme == "message"){
                // #pragma message "text" - print message (GCC/Clang compatible, but with macro expansion)
                i++;
                while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;

                // Collect tokens to expand
                Barray!PPToken msg_tokens = make_barray!PPToken(allocator);
                while(i < line.length && line[i].type != PPTokenType.PP_NEWLINE){
                    if(line[i].type != PPTokenType.PP_WHITESPACE || msg_tokens.count > 0){
                        msg_tokens ~= line[i];
                    }
                    i++;
                }

                // Expand macros
                Barray!PPToken expanded = make_barray!PPToken(allocator);
                HideSet msg_hs = HideSet.create(allocator);
                expand_tokens(msg_tokens[], &expanded, msg_hs);

                // Build message from expanded tokens directly to logger buffer
                int pragma_line = line.length > 0 ? line[0].line : 0;
                logger.buff.FORMAT(current_file, ":", pragma_line, ": note: ");
                bool has_content = false;
                foreach(tok; expanded[]){
                    if(tok.type == PPTokenType.PP_STRING){
                        // Remove quotes from string literal
                        str s = tok.lexeme;
                        if(s.length >= 2 && s[0] == '"' && s[$-1] == '"'){
                            logger.buff.write(s[1..$-1]);
                        } else {
                            logger.buff.write(s);
                        }
                        has_content = true;
                    } else if(tok.type != PPTokenType.PP_WHITESPACE || has_content){
                        logger.buff.write(tok.lexeme);
                        has_content = true;
                    }
                }
                logger.buff.write("\n");
                logger.flush(LogLevel.INFO);

            } else if(line[i].lexeme == "library"){
                // #pragma library("libname") - emit tokens for parser
                // Emit: #pragma library ( "libname" )
                PPToken hash_tok;
                hash_tok.type = PPTokenType.PP_PUNCTUATOR;
                hash_tok.lexeme = "#";
                hash_tok.file = current_file;
                *output ~= hash_tok;

                PPToken pragma_tok;
                pragma_tok.type = PPTokenType.PP_IDENTIFIER;
                pragma_tok.lexeme = "pragma";
                pragma_tok.file = current_file;
                *output ~= pragma_tok;

                // Emit rest of line (library("..."))
                while(i < line.length){
                    if(line[i].type != PPTokenType.PP_WHITESPACE &&
                        line[i].type != PPTokenType.PP_NEWLINE){
                        *output ~= line[i];
                    }
                    i++;
                }

                // Add newline
                PPToken nl;
                nl.type = PPTokenType.PP_NEWLINE;
                nl.lexeme = "\n";
                *output ~= nl;

            } else if(line[i].lexeme == "include_path"){
                // #pragma include_path push/pop/reveal
                i++;
                while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;

                int pragma_line = line.length > 0 ? line[0].line : 0;

                if(i < line.length && line[i].type == PPTokenType.PP_IDENTIFIER){
                    str subcommand = line[i].lexeme;
                    i++;

                    if(subcommand == "push"){
                        // #pragma include_path push "path" or <path>
                        while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;

                        str path_str = "";
                        if(i < line.length && line[i].type == PPTokenType.PP_STRING){
                            // "path" form
                            str s = line[i].lexeme;
                            if(s.length >= 2 && s[0] == '"' && s[$-1] == '"'){
                                path_str = s[1..$-1];
                            }
                        } else if(i < line.length && line[i].is_punct("<")){
                            // <path> form - collect until >
                            i++;
                            StringBuilder path_sb;
                            path_sb.allocator = allocator;
                            while(i < line.length && !line[i].is_punct(">") &&
                                  line[i].type != PPTokenType.PP_NEWLINE){
                                path_sb.write(line[i].lexeme);
                                i++;
                            }
                            path_str = path_sb.borrow();
                        }

                        if(path_str.length > 0){
                            // Resolve relative paths against current file's directory
                            str resolved_path;
                            if(path_str[0] != '/'){
                                str dir = get_directory(current_file);
                                if(dir.length > 0){
                                    resolved_path = concat_path(dir, path_str);
                                } else {
                                    resolved_path = path_str;
                                }
                            } else {
                                resolved_path = path_str;
                            }
                            pushed_include_paths ~= resolved_path;
                        }

                    } else if(subcommand == "pop"){
                        // #pragma include_path pop
                        if(pushed_include_paths.count == 0){
                            logger.warn(current_file, ":", pragma_line, ": warning: #pragma include_path pop with nothing pushed\n");
                        } else {
                            pushed_include_paths.count--;
                        }

                    } else if(subcommand == "reveal"){
                        // #pragma include_path reveal - show all include paths
                        logger.info(current_file, ":", pragma_line, ": include paths:\n");

                        // Show pushed paths first (in search order - reverse)
                        for(size_t j = pushed_include_paths.count; j > 0; j--){
                            str p = pushed_include_paths[j-1];
                            logger.info("  [pushed] ", p, "\n");
                        }

                        // Show configured paths
                        foreach(p; include_paths){
                            logger.info("  ", p, "\n");
                        }
                    }
                }

            } else if(line[i].lexeme == "watch"){
                // #pragma watch[(define, undef, expand)] MACRO
                i++;

                // Parse optional flags: (define, undef, expand)
                ubyte flags = WatchFlags.ALL;  // Default: watch everything
                if(i < line.length && line[i].is_punct("(")){
                    flags = WatchFlags.NONE;
                    i++;
                    while(i < line.length && !line[i].is_punct(")")){
                        if(line[i].type == PPTokenType.PP_IDENTIFIER){
                            if(line[i].lexeme == "define") flags |= WatchFlags.DEFINE;
                            else if(line[i].lexeme == "undef") flags |= WatchFlags.UNDEF;
                            else if(line[i].lexeme == "expand") flags |= WatchFlags.EXPAND;
                        }
                        i++;
                    }
                    if(i < line.length) i++;  // skip )
                }

                while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;

                if(i < line.length && line[i].type == PPTokenType.PP_IDENTIFIER){
                    watched_macros[line[i].lexeme] = flags;
                }

            } else if(line[i].lexeme == "unwatch"){
                // #pragma unwatch MACRO - stop tracing macro
                i++;
                while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;

                if(i < line.length && line[i].type == PPTokenType.PP_IDENTIFIER){
                    if(auto p = line[i].lexeme in watched_macros){
                        *p = WatchFlags.NONE;  // Mark as unwatched
                    }
                }
            }
            // Ignore other pragmas
        }
    }

    // Handle #error
    void handle_error(PPToken[] line){
        StringBuilder sb;
        sb.allocator = allocator;
        foreach(tok; line){
            if(tok.type != PPTokenType.PP_WHITESPACE || sb.cursor > 0){
                sb.write(tok.lexeme);
            }
        }
        error_occurred = true;
        int error_line = line.length > 0 ? line[0].line : 0;
        logger.error(current_file, ":", error_line, ": #error ", sb.borrow(), "\n");
    }

    // Handle #warning
    void handle_warning(PPToken[] line){
        StringBuilder sb;
        sb.allocator = allocator;
        foreach(tok; line){
            if(tok.type != PPTokenType.PP_WHITESPACE || sb.cursor > 0){
                sb.write(tok.lexeme);
            }
        }
        int warning_line = line.length > 0 ? line[0].line : 0;
        logger.warn(current_file, ":", warning_line, ": warning: #warning ", sb.borrow(), "\n");
    }

    // Handle #line directive
    void handle_line(PPToken[] line){
        size_t i = 0;
        while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;

        if(i >= line.length || line[i].type != PPTokenType.PP_NUMBER){
            error("#line requires a line number");
            return;
        }

        // Parse line number
        int new_line = 0;
        foreach(c; line[i].lexeme){
            if(c >= '0' && c <= '9'){
                new_line = new_line * 10 + (c - '0');
            } else {
                error("#line requires a positive integer");
                return;
            }
        }

        // Compute offset: we want __LINE__ on the NEXT line to be new_line
        // Current token is on line[i].line, next line will be line[i].line + 1
        // We want: (line[i].line + 1) + offset = new_line
        // So: offset = new_line - line[i].line - 1
        line_offset = new_line - line[i].line - 1;
        i++;

        // Check for optional filename
        while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;

        if(i < line.length && line[i].type == PPTokenType.PP_STRING){
            // Extract filename from string literal (remove quotes)
            str s = line[i].lexeme;
            if(s.length >= 2 && s[0] == '"' && s[$-1] == '"'){
                file_override = s[1..$-1];
            }
        }
    }

    // Handle #embed directive
    // Emits: __embed("resolved_path", offset, length)
    void handle_embed(PPToken[] line, Barray!PPToken* output){
        size_t i = 0;

        // Skip whitespace
        while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;

        if(i >= line.length){
            error("#embed requires filename");
            return;
        }

        str filename;
        bool is_system = false;
        PPToken first_tok = line[i];

        // Parse filename - similar to #include
        if(line[i].type == PPTokenType.PP_PUNCTUATOR && line[i].matches("<")){
            // <resource> style
            is_system = true;
            i++;
            StringBuilder sb;
            sb.allocator = allocator;
            while(i < line.length && !line[i].is_punct(">") &&
                   line[i].type != PPTokenType.PP_NEWLINE){
                sb.write(line[i].lexeme);
                i++;
            }
            if(i < line.length && line[i].is_punct(">")) i++;
            filename = sb.borrow();
        } else if(line[i].type == PPTokenType.PP_STRING){
            // "resource" style
            str s = line[i].lexeme;
            if(s.length >= 2){
                filename = s[1 .. $ - 1];  // Remove quotes
            }
            i++;
        } else {
            error("#embed requires filename");
            return;
        }

        // Parse embed parameters
        long limit_value = -1;  // -1 means no limit
        PPToken[] prefix_tokens;
        PPToken[] suffix_tokens;
        PPToken[] if_empty_tokens;
        bool has_prefix = false;
        bool has_suffix = false;
        bool has_if_empty = false;

        while(i < line.length){
            // Skip whitespace
            while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;
            if(i >= line.length) break;

            if(line[i].type != PPTokenType.PP_IDENTIFIER){
                // Unknown token - skip
                i++;
                continue;
            }

            str param_name = line[i].lexeme;
            i++;

            // Skip whitespace
            while(i < line.length && line[i].type == PPTokenType.PP_WHITESPACE) i++;

            // Check for parameter clause (...)
            if(i >= line.length || !line[i].is_punct("(")){
                // Parameter without clause - warn and ignore
                logger.warn(current_file, ":", first_tok.line, ": warning: unknown embed parameter '", param_name, "'\n");
                continue;
            }

            // Parse balanced token sequence
            i++;  // Skip (
            size_t clause_start = i;
            int depth = 1;
            while(i < line.length && depth > 0){
                if(line[i].is_punct("(")) depth++;
                else if(line[i].is_punct(")")) depth--;
                if(depth > 0) i++;
            }
            PPToken[] clause_tokens = line[clause_start .. i];
            if(i < line.length) i++;  // Skip )

            // Handle known parameters
            if(param_name == "limit" || param_name == "__limit__"){
                // Parse integer constant from clause
                limit_value = parse_embed_limit(clause_tokens);
                if(limit_value < 0){
                    error("#embed limit must be non-negative integer");
                    return;
                }
            } else if(param_name == "prefix" || param_name == "__prefix__"){
                prefix_tokens = clause_tokens;
                has_prefix = true;
            } else if(param_name == "suffix" || param_name == "__suffix__"){
                suffix_tokens = clause_tokens;
                has_suffix = true;
            } else if(param_name == "if_empty" || param_name == "__if_empty__"){
                if_empty_tokens = clause_tokens;
                has_if_empty = true;
            } else {
                // Unknown parameter - warn per spec (implementation-defined params)
                logger.warn(current_file, ":", first_tok.line, ": warning: unknown embed parameter '", param_name, "'\n");
            }
        }

        // Resolve path
        str full_path = resolve_include(filename, is_system);
        if(full_path.length == 0){
            logger.error(current_file, ":", first_tok.line, ": error: '", filename, "' file not found for #embed\n");
            error_occurred = true;
            return;
        }

        // Get file size
        size_t file_size;
        if(file_cache.get_size(full_path, file_size) != FileError.OK){
            logger.error(current_file, ":", first_tok.line, ": error: cannot stat '", full_path, "'\n");
            error_occurred = true;
            return;
        }

        // Apply limit
        long byte_count = file_size;
        if(limit_value >= 0 && limit_value < byte_count){
            byte_count = limit_value;
        }

        // Handle empty case
        if(byte_count == 0){
            if(has_if_empty){
                // Emit if_empty tokens
                foreach(tok; if_empty_tokens){
                    if(tok.type != PPTokenType.PP_WHITESPACE){
                        *output ~= tok;
                    }
                }
            }
            // Empty embed with no if_empty produces nothing
            return;
        }

        // Non-empty: emit prefix, __embed(...), suffix

        // Emit prefix tokens (without leading/trailing whitespace)
        if(has_prefix){
            foreach(tok; prefix_tokens){
                if(tok.type != PPTokenType.PP_WHITESPACE){
                    *output ~= tok;
                }
            }
        }

        // Emit __embed("path", 0, length)
        emit_embed_token(output, full_path, 0, byte_count, first_tok);

        // Emit suffix tokens
        if(has_suffix){
            foreach(tok; suffix_tokens){
                if(tok.type != PPTokenType.PP_WHITESPACE){
                    *output ~= tok;
                }
            }
        }
    }

    // Parse limit parameter value
    long parse_embed_limit(PPToken[] tokens){
        size_t i = 0;
        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;
        if(i >= tokens.length) return -1;

        if(tokens[i].type != PPTokenType.PP_NUMBER) return -1;

        // Simple integer parsing
        long value = 0;
        foreach(c; tokens[i].lexeme){
            if(c >= '0' && c <= '9'){
                value = value * 10 + (c - '0');
            } else if(c == '\''){
                // C23 digit separator - ignore
            } else {
                return -1;  // Not a simple integer
            }
        }
        return value;
    }

    // Emit __embed("path", offset, length) tokens
    void emit_embed_token(Barray!PPToken* output, str path, long offset, long length, PPToken loc){
        PPToken tok;
        tok.line = loc.line;
        tok.column = loc.column;
        tok.file = loc.file;

        // __embed
        tok.type = PPTokenType.PP_IDENTIFIER;
        tok.lexeme = "__embed";
        *output ~= tok;

        // (
        tok.type = PPTokenType.PP_PUNCTUATOR;
        tok.lexeme = "(";
        *output ~= tok;

        // "path"
        tok.type = PPTokenType.PP_STRING;
        tok.lexeme = mwritef(allocator, "\"%\"", E(path))[];
        *output ~= tok;

        // ,
        tok.type = PPTokenType.PP_PUNCTUATOR;
        tok.lexeme = ",";
        *output ~= tok;

        // offset
        tok.type = PPTokenType.PP_NUMBER;
        tok.lexeme = mwritef(allocator, "%", offset)[];
        *output ~= tok;

        // ,
        tok.type = PPTokenType.PP_PUNCTUATOR;
        tok.lexeme = ",";
        *output ~= tok;

        // length
        tok.type = PPTokenType.PP_NUMBER;
        tok.lexeme = mwritef(allocator, "%", length)[];
        *output ~= tok;

        // )
        tok.type = PPTokenType.PP_PUNCTUATOR;
        tok.lexeme = ")";
        *output ~= tok;
    }

    // Handle _Pragma("string") operator
    size_t handle_pragma_operator(PPToken[] tokens, size_t start, Barray!PPToken* output){
        size_t i = start + 1;  // Skip _Pragma

        // Skip whitespace
        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

        // Expect (
        if(i >= tokens.length || !tokens[i].is_punct("(")){
            return start;  // Not a valid _Pragma invocation
        }
        i++;

        // Skip whitespace
        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

        // Expect string literal
        if(i >= tokens.length || tokens[i].type != PPTokenType.PP_STRING){
            return start;
        }

        str pragma_str = tokens[i].lexeme;
        i++;

        // Skip whitespace
        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

        // Expect )
        if(i >= tokens.length || !tokens[i].is_punct(")")){
            return start;
        }
        i++;

        // Destringify: remove quotes and process escapes
        if(pragma_str.length < 2 || pragma_str[0] != '"' || pragma_str[$-1] != '"'){
            return i;  // Invalid string, skip
        }

        StringBuilder sb;
        sb.allocator = allocator;
        size_t j = 1;  // Skip opening quote
        while(j < pragma_str.length - 1){
            char c = pragma_str[j];
            if(c == '\\' && j + 1 < pragma_str.length - 1){
                j++;
                char next = pragma_str[j];
                switch(next){
                    case 'n': sb.write('\n'); break;
                    case 't': sb.write('\t'); break;
                    case 'r': sb.write('\r'); break;
                    case '\\': sb.write('\\'); break;
                    case '"': sb.write('"'); break;
                    default: sb.write(next); break;
                }
            } else {
                sb.write(c);
            }
            j++;
        }

        // Tokenize the destringified content
        str pragma_content = sb.borrow();
        Barray!PPToken pragma_tokens = make_barray!PPToken(allocator);
        pp_tokenize(cast(const(ubyte)[])pragma_content, current_file, &pragma_tokens, allocator);

        // Execute as pragma (without the #pragma prefix)
        handle_pragma(pragma_tokens[], output);

        return i;
    }

    // Evaluate preprocessor expression
    long evaluate_expression(PPToken[] tokens){
        // First expand macros (except in defined())
        Barray!PPToken expanded = make_barray!PPToken(allocator);
        HideSet expr_hs = HideSet.create(allocator);
        expand_tokens(tokens, &expanded, expr_hs, true);

        // Then evaluate
        ExprEvaluator eval;
        eval.tokens = expanded[];
        eval.pos = 0;
        eval.pp = &this;
        return eval.eval_conditional();
    }

    // Expand macros and builtins. If for_if=true, also handles defined(), __has_include, etc.
    // hs tracks macros currently being expanded to prevent infinite recursion.
    void expand_tokens(PPToken[] tokens, Barray!PPToken* output, HideSet hs, bool for_if = false){
        size_t i = 0;
        while(i < tokens.length){
            PPToken tok = tokens[i];

            // Skip whitespace
            if(tok.type == PPTokenType.PP_WHITESPACE){
                i++;
                continue;
            }

            // Handle __EXPAND__ in #if context
            if(tok.type == PPTokenType.PP_IDENTIFIER && tok.matches("__EXPAND__")){
                i = handle_expand_builtin(tokens, i, output);
                continue;
            }

            // Handle __ENV__ in #if context
            if(tok.type == PPTokenType.PP_IDENTIFIER && tok.matches("__ENV__")){
                i = handle_env_builtin(tokens, i, output);
                continue;
            }

            // Handle defined operator (only in #if context)
            if(for_if && tok.type == PPTokenType.PP_IDENTIFIER && tok.matches("defined")){
                i++;
                // Skip whitespace
                while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

                bool has_paren = false;
                if(i < tokens.length && tokens[i].is_punct("(")){
                    has_paren = true;
                    i++;
                    while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;
                }

                if(i < tokens.length && tokens[i].type == PPTokenType.PP_IDENTIFIER){
                    str name = tokens[i].lexeme;
                    bool def = is_defined(name);
                    i++;

                    // Emit 1 or 0
                    PPToken result;
                    result.type = PPTokenType.PP_NUMBER;
                    result.lexeme = def ? "1" : "0";
                    *output ~= result;

                    if(has_paren){
                        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;
                        if(i < tokens.length && tokens[i].is_punct(")")) i++;
                    }
                    continue;
                }
            }

            // Handle __has_include specially (only in #if context, takes header-name args)
            if(for_if && tok.type == PPTokenType.PP_IDENTIFIER && tok.matches("__has_include")){
                i++;
                // Skip whitespace
                while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;
                // Expect (
                if(i >= tokens.length || !tokens[i].is_punct("(")){
                    PPToken result;
                    result.type = PPTokenType.PP_NUMBER;
                    result.lexeme = "0";
                    *output ~= result;
                    continue;
                }
                i++;
                while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

                // Parse header name
                str filename;
                bool is_system = false;
                bool found = false;

                if(i < tokens.length && tokens[i].type == PPTokenType.PP_STRING){
                    // "header.h" form
                    str s = tokens[i].lexeme;
                    if(s.length >= 2 && s[0] == '"' && s[$-1] == '"'){
                        filename = s[1..$-1];
                        is_system = false;
                        found = true;
                    }
                    i++;
                } else if(i < tokens.length && tokens[i].is_punct("<")){
                    // <header.h> form - collect tokens until >
                    i++;
                    StringBuilder sb;
                    sb.allocator = allocator;
                    while(i < tokens.length && !tokens[i].is_punct(">")){
                        sb.write(tokens[i].lexeme);
                        i++;
                    }
                    if(i < tokens.length && tokens[i].is_punct(">")) i++;
                    filename = sb.borrow();
                    is_system = true;
                    found = true;
                }

                // Skip to closing )
                while(i < tokens.length && !tokens[i].is_punct(")")) i++;
                if(i < tokens.length) i++;

                // Check if file exists
                bool exists = false;
                if(found){
                    str resolved = resolve_include(filename, is_system);
                    exists = resolved.length > 0;
                }

                PPToken result;
                result.type = PPTokenType.PP_NUMBER;
                result.lexeme = exists ? "1" : "0";
                *output ~= result;
                continue;
            }

            // Handle __has_include_next (only in #if context, stub - always 0)
            if(for_if && tok.type == PPTokenType.PP_IDENTIFIER && tok.matches("__has_include_next")){
                i++;
                while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;
                if(i < tokens.length && tokens[i].is_punct("(")){
                    i++;
                    int depth = 1;
                    while(i < tokens.length && depth > 0){
                        if(tokens[i].is_punct("(")) depth++;
                        else if(tokens[i].is_punct(")")) depth--;
                        i++;
                    }
                }
                PPToken result;
                result.type = PPTokenType.PP_NUMBER;
                result.lexeme = "0";
                *output ~= result;
                continue;
            }

            // Handle __has_embed (only in #if context, C23 - check if embed would succeed)
            // Returns: 0 = not found, 1 = found and non-empty, 2 = found but empty
            if(for_if && tok.type == PPTokenType.PP_IDENTIFIER && tok.matches("__has_embed")){
                i++;
                while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;
                if(i >= tokens.length || !tokens[i].is_punct("(")){
                    PPToken result;
                    result.type = PPTokenType.PP_NUMBER;
                    result.lexeme = "0";  // __STDC_EMBED_NOT_FOUND__
                    *output ~= result;
                    continue;
                }
                i++;
                while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

                // Parse header name
                str filename;
                bool is_system = false;
                bool found_name = false;

                if(i < tokens.length && tokens[i].type == PPTokenType.PP_STRING){
                    // "resource" form
                    str s = tokens[i].lexeme;
                    if(s.length >= 2 && s[0] == '"' && s[$-1] == '"'){
                        filename = s[1..$-1];
                        is_system = false;
                        found_name = true;
                    }
                    i++;
                } else if(i < tokens.length && tokens[i].is_punct("<")){
                    // <resource> form
                    i++;
                    StringBuilder sb;
                    sb.allocator = allocator;
                    while(i < tokens.length && !tokens[i].is_punct(">")){
                        sb.write(tokens[i].lexeme);
                        i++;
                    }
                    if(i < tokens.length && tokens[i].is_punct(">")) i++;
                    filename = sb.borrow();
                    is_system = true;
                    found_name = true;
                }

                // Parse optional embed parameters (to check limit(0) makes it empty)
                long limit_value = -1;
                while(i < tokens.length && !tokens[i].is_punct(")")){
                    if(tokens[i].type == PPTokenType.PP_IDENTIFIER){
                        str param_name = tokens[i].lexeme;
                        i++;
                        while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;
                        if(i < tokens.length && tokens[i].is_punct("(")){
                            i++;
                            size_t clause_start = i;
                            int depth = 1;
                            while(i < tokens.length && depth > 0){
                                if(tokens[i].is_punct("(")) depth++;
                                else if(tokens[i].is_punct(")")) depth--;
                                if(depth > 0) i++;
                            }
                            PPToken[] clause = tokens[clause_start .. i];
                            if(i < tokens.length) i++;

                            if(param_name == "limit" || param_name == "__limit__"){
                                limit_value = parse_embed_limit(clause);
                            }
                        }
                    } else {
                        i++;
                    }
                }
                if(i < tokens.length) i++;  // Skip )

                // Determine result
                int has_embed_result = 0;  // __STDC_EMBED_NOT_FOUND__
                if(found_name){
                    str resolved = resolve_include(filename, is_system);
                    if(resolved.length > 0){
                        // File exists - check if empty
                        size_t file_size;
                        if(file_cache.get_size(resolved, file_size) == FileError.OK){
                            long effective_size = file_size;
                            if(limit_value >= 0 && limit_value < effective_size){
                                effective_size = limit_value;
                            }
                            if(effective_size == 0){
                                has_embed_result = 2;  // __STDC_EMBED_EMPTY__
                            } else {
                                has_embed_result = 1;  // __STDC_EMBED_FOUND__
                            }
                        }
                    }
                }

                PPToken result;
                result.type = PPTokenType.PP_NUMBER;
                result.lexeme = has_embed_result == 0 ? "0" : (has_embed_result == 1 ? "1" : "2");
                *output ~= result;
                continue;
            }

            // Check for macro to expand
            if(tok.type == PPTokenType.PP_IDENTIFIER){
                PPMacroDef macro_def = get_macro(tok);
                if(!macro_def.is_null && !macro_def.is_function_like){
                    // Check hide set to prevent infinite recursion
                    if(hs.is_hidden(tok.lexeme)){
                        // Macro is hidden, pass through unexpanded
                        *output ~= tok;
                        i++;
                        continue;
                    }
                    // Expand object-like macro and recursively process
                    i++;
                    HideSet new_hs = hs.with_hidden(macro_def.name);
                    expand_tokens(macro_def.replacement, output, new_hs, for_if);
                    continue;
                }
                // Handle function-like macros
                if(!macro_def.is_null && macro_def.is_function_like){
                    // Check hide set to prevent infinite recursion
                    if(hs.is_hidden(tok.lexeme)){
                        // Macro is hidden, pass through unexpanded
                        *output ~= tok;
                        i++;
                        continue;
                    }
                    // Check for (
                    size_t j = i + 1;
                    while(j < tokens.length && tokens[j].type == PPTokenType.PP_WHITESPACE) j++;
                    if(j < tokens.length && tokens[j].is_punct("(")){
                        // Expand the macro to a temporary buffer
                        Barray!PPToken expanded = make_barray!PPToken(allocator);
                        HideSet new_hs = hs.with_hidden(macro_def.name);
                        i = expand_macro_invocation(tokens, i, macro_def, new_hs, &expanded);
                        // Recursively process the expanded tokens (pass the updated hide set)
                        expand_tokens(expanded[], output, new_hs, for_if);
                        continue;
                    }
                }
            }

            // Pass through
            *output ~= tok;
            i++;
        }
    }

    // Set expansion location on a token (where the macro was invoked)
    static PPToken with_expansion_loc(PPToken tok, PPToken invocation){
        // Only set if not already set (preserve innermost expansion point)
        if(tok.expansion_file.length == 0){
            // If invocation itself came from macro expansion, use its expansion location
            if(invocation.expansion_file.length > 0){
                tok.expansion_file = invocation.expansion_file;
                tok.expansion_line = invocation.expansion_line;
                tok.expansion_column = invocation.expansion_column;
            } else {
                tok.expansion_file = invocation.file;
                tok.expansion_line = invocation.line;
                tok.expansion_column = invocation.column;
            }
        }
        return tok;
    }

    // Expand a macro invocation
    size_t expand_macro_invocation(PPToken[] tokens, size_t start, ref PPMacroDef macro_def, HideSet hs, Barray!PPToken* output){
        size_t i = start + 1;  // Skip macro name
        PPToken invocation = tokens[start];  // Where the macro was invoked

        // Add macro to hide set
        hs = hs.with_hidden(macro_def.name);

        if(macro_def.is_function_like){
            // Skip whitespace
            while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;

            // Must have (
            if(i >= tokens.length || !tokens[i].is_punct("(")){
                // Not an invocation, just output the identifier
                *output ~= tokens[start];
                return start + 1;
            }
            i++;  // skip (

            // Parse arguments
            Barray!(PPToken[]) args = make_barray!(PPToken[])(allocator);
            Barray!PPToken current_arg = make_barray!PPToken(allocator);
            int depth = 1;

            while(i < tokens.length && depth > 0){
                PPToken tok = tokens[i];

                if(tok.is_punct("(")){
                    depth++;
                    current_arg ~= tok;
                } else if(tok.is_punct(")")){
                    depth--;
                    if(depth == 0){
                        // End of arguments - always add final arg if macro takes params
                        // STR() has one empty arg, STR(a,b) has two args
                        if(macro_def.params.length > 0 || current_arg.count > 0 || args.count > 0){
                            args ~= current_arg[];
                        }
                    } else {
                        current_arg ~= tok;
                    }
                } else if(tok.is_punct(",") && depth == 1){
                    // Argument separator
                    args ~= current_arg[];
                    current_arg = make_barray!PPToken(allocator);
                } else {
                    current_arg ~= tok;
                }
                i++;
            }

            // Substitute and expand
            Barray!PPToken substituted = make_barray!PPToken(allocator);
            substitute(macro_def, args[], hs, &substituted);

            // Log if this macro is being watched for expand
            if(auto p = macro_def.name in watched_macros){ if(*p & WatchFlags.EXPAND){
                logger.buff.FORMAT(invocation.file, ":", invocation.line, ": [watch] ", macro_def.name, "(");
                foreach(ai, arg; args[]){
                    if(ai > 0) logger.buff.write(", ");
                    foreach(tok; arg)
                        logger.buff.write(tok.lexeme);
                }
                logger.buff.write(") -> ");
                foreach(tok; substituted[])
                    logger.buff.write(tok.lexeme);
                logger.buff.write("\n");
                logger.flush(LogLevel.DEBUG);
            }}

            // Set expansion location on all substituted tokens so nested macros
            // have the right invocation point
            foreach(ref tok; substituted[]){
                tok = with_expansion_loc(tok, invocation);
            }

            // Rescan for more macros and _Pragma
            size_t j = 0;
            while(j < substituted.count){
                PPToken tok = substituted[j];
                if(tok.type == PPTokenType.PP_IDENTIFIER){
                    // Check for _Pragma operator
                    if(tok.lexeme == "_Pragma"){
                        size_t new_j = handle_pragma_operator(substituted[], j, output);
                        if(new_j != j){
                            j = new_j;
                            continue;
                        }
                    }
                    PPMacroDef nested = get_macro(tok);
                    if(!nested.is_null && !hs.is_hidden(tok.lexeme)){
                        j = expand_macro_invocation(substituted[], j, nested, hs, output);
                        continue;
                    }
                }
                *output ~= tok;  // Already has expansion_line set
                j++;
            }

            return i;
        } else {
            // Object-like macro - use substitute() to handle ## pasting
            Barray!PPToken substituted = make_barray!PPToken(allocator);
            PPToken[][] empty_args;
            substitute(macro_def, empty_args, hs, &substituted);

            // Log if this macro is being watched for expand
            if(auto p = macro_def.name in watched_macros){ if(*p & WatchFlags.EXPAND){
                logger.buff.FORMAT(invocation.file, ":", invocation.line, ": [watch] ", macro_def.name, " -> ");
                foreach(tok; substituted[])
                    logger.buff.write(tok.lexeme);
                logger.buff.write("\n");
                logger.flush(LogLevel.DEBUG);
            }}

            // Set expansion location on all substituted tokens so nested macros
            // have the right invocation point
            foreach(ref tok; substituted[]){
                tok = with_expansion_loc(tok, invocation);
            }

            // Rescan for more macros and _Pragma
            size_t j = 0;
            while(j < substituted.count){
                PPToken tok = substituted[j];
                if(tok.type == PPTokenType.PP_IDENTIFIER){
                    // Check for _Pragma operator
                    if(tok.lexeme == "_Pragma"){
                        size_t new_j = handle_pragma_operator(substituted[], j, output);
                        if(new_j != j){
                            j = new_j;
                            continue;
                        }
                    }
                    PPMacroDef nested = get_macro(tok);
                    if(!nested.is_null && !hs.is_hidden(tok.lexeme)){
                        j = expand_macro_invocation(substituted[], j, nested, hs, output);
                        continue;
                    }
                }
                *output ~= tok;  // Already has expansion_line set
                j++;
            }

            return i;
        }
    }

    // Substitute macro parameters
    void substitute(ref PPMacroDef macro_def, PPToken[][] args, HideSet hs, Barray!PPToken* output){
        PPToken[] repl = macro_def.replacement;
        size_t i = 0;

        while(i < repl.length){
            PPToken tok = repl[i];

            // Check for stringification: # param
            if(tok.is_punct("#") && i + 1 < repl.length){
                // Skip whitespace
                size_t j = i + 1;
                while(j < repl.length && repl[j].type == PPTokenType.PP_WHITESPACE) j++;

                if(j < repl.length && repl[j].type == PPTokenType.PP_IDENTIFIER){
                    int param_idx = macro_def.find_param(repl[j].lexeme);
                    if(param_idx >= 0 && param_idx < args.length){
                        // Stringify the argument
                        PPToken str_tok = stringify(args[param_idx]);
                        *output ~= str_tok;
                        i = j + 1;
                        continue;
                    }
                }
            }

            // Check for token pasting: X ## Y
            if(i + 1 < repl.length){
                // Look ahead for ##
                size_t j = i + 1;
                while(j < repl.length && repl[j].type == PPTokenType.PP_WHITESPACE) j++;

                if(j < repl.length && repl[j].is_punct("##")){
                    // Find right operand
                    size_t k = j + 1;
                    while(k < repl.length && repl[k].type == PPTokenType.PP_WHITESPACE) k++;

                    if(k < repl.length){
                        // Get left token (possibly parameter)
                        PPToken left = tok;
                        bool left_empty = false;
                        PPToken[] left_prefix;  // Tokens before the last one (for multi-token args)
                        if(tok.type == PPTokenType.PP_IDENTIFIER){
                            int param_idx = macro_def.find_param(tok.lexeme);
                            if(param_idx >= 0 && param_idx < args.length){
                                PPToken[] arg = args[param_idx];
                                // Find last non-whitespace token
                                bool found_nonws = false;
                                size_t last_idx = arg.length;
                                for(size_t ai = arg.length; ai > 0; ai--){
                                    if(arg[ai - 1].type != PPTokenType.PP_WHITESPACE){
                                        left = arg[ai - 1];
                                        last_idx = ai - 1;
                                        found_nonws = true;
                                        break;
                                    }
                                }
                                if(found_nonws){
                                    // Output all tokens before the last non-whitespace one
                                    if(last_idx > 0){
                                        left_prefix = arg[0 .. last_idx];
                                    }
                                } else {
                                    // Argument is empty or whitespace-only
                                    left_empty = true;
                                }
                            }
                        }

                        // Get right token (possibly parameter)
                        PPToken right = repl[k];
                        bool right_empty = false;
                        PPToken[] right_suffix;  // Tokens after the first one (for multi-token args)
                        if(right.type == PPTokenType.PP_IDENTIFIER){
                            int param_idx = macro_def.find_param(right.lexeme);
                            if(param_idx >= 0 && param_idx < args.length){
                                PPToken[] arg = args[param_idx];
                                // Find first non-whitespace token
                                bool found_nonws = false;
                                size_t first_idx = 0;
                                foreach(ai, argtok; arg){
                                    if(argtok.type != PPTokenType.PP_WHITESPACE){
                                        right = argtok;
                                        first_idx = ai;
                                        found_nonws = true;
                                        break;
                                    }
                                }
                                if(found_nonws){
                                    // Keep tokens after the first non-whitespace one
                                    if(first_idx + 1 < arg.length){
                                        right_suffix = arg[first_idx + 1 .. $];
                                    }
                                } else {
                                    // Argument is empty or whitespace-only
                                    right_empty = true;
                                }
                            }
                        }

                        // Output prefix tokens from left side
                        foreach(ptok; left_prefix){
                            *output ~= ptok;
                        }

                        // Paste tokens, handling empty sides
                        PPToken result;
                        bool have_result = false;
                        if(left_empty && right_empty){
                            // Both empty - produce nothing
                        } else if(left_empty){
                            // Only right side
                            result = right;
                            have_result = true;
                        } else if(right_empty){
                            // Only left side
                            result = left;
                            have_result = true;
                        } else {
                            // Paste both
                            result = paste_tokens(left, right);
                            have_result = true;
                        }

                        // Check for chained ## (e.g., a ## b ## c)
                        size_t next_k = k + 1;
                        while(have_result && next_k < repl.length){
                            // Skip whitespace
                            while(next_k < repl.length && repl[next_k].type == PPTokenType.PP_WHITESPACE) next_k++;

                            // Check for another ##
                            if(next_k < repl.length && repl[next_k].is_punct("##")){
                                // Find the next operand
                                size_t next_right_idx = next_k + 1;
                                while(next_right_idx < repl.length && repl[next_right_idx].type == PPTokenType.PP_WHITESPACE) next_right_idx++;

                                if(next_right_idx < repl.length){
                                    // Get the next right operand
                                    PPToken next_right = repl[next_right_idx];
                                    bool next_right_empty = false;

                                    if(next_right.type == PPTokenType.PP_IDENTIFIER){
                                        int param_idx2 = macro_def.find_param(next_right.lexeme);
                                        if(param_idx2 >= 0 && param_idx2 < args.length){
                                            PPToken[] arg2 = args[param_idx2];
                                            bool found2 = false;
                                            foreach(argtok2; arg2){
                                                if(argtok2.type != PPTokenType.PP_WHITESPACE){
                                                    next_right = argtok2;
                                                    found2 = true;
                                                    break;
                                                }
                                            }
                                            if(!found2){
                                                next_right_empty = true;
                                            }
                                        }
                                    }

                                    // Paste result with next_right
                                    if(!next_right_empty){
                                        result = paste_tokens(result, next_right);
                                    }
                                    // If next_right_empty, result stays the same

                                    next_k = next_right_idx + 1;
                                    continue;
                                }
                            }
                            break;
                        }

                        if(have_result){
                            *output ~= result;
                        }

                        // Output suffix tokens from right side (only if no chained ##)
                        // Note: with chained ##, suffixes would have been processed above
                        if(next_k == k + 1){
                            foreach(stok; right_suffix){
                                *output ~= stok;
                            }
                        }

                        i = next_k;
                        continue;
                    }
                }
            }

            // Regular parameter substitution
            if(tok.type == PPTokenType.PP_IDENTIFIER){
                int param_idx = macro_def.find_param(tok.lexeme);
                if(param_idx >= 0){
                    if(param_idx < args.length){
                        // Pre-expand argument (macros and builtins in args are expanded)
                        // Pass the hide set to prevent infinite recursion
                        Barray!PPToken expanded_arg = make_barray!PPToken(allocator);
                        expand_tokens(args[param_idx], &expanded_arg, hs);
                        // Output expanded argument
                        foreach(exp_tok; expanded_arg[]){
                            *output ~= exp_tok;
                        }
                    }
                    i++;
                    continue;
                }

                // Check for __VA_ARGS__
                if(macro_def.is_variadic && tok.matches("__VA_ARGS__")){
                    // Concatenate remaining arguments
                    for(size_t ai = macro_def.params.length; ai < args.length; ai++){
                        if(ai > macro_def.params.length){
                            PPToken comma;
                            comma.type = PPTokenType.PP_PUNCTUATOR;
                            comma.lexeme = ",";
                            *output ~= comma;
                        }
                        foreach(arg_tok; args[ai]){
                            *output ~= arg_tok;
                        }
                    }
                    i++;
                    continue;
                }

                // Check for __VA_OPT__(content)
                if(macro_def.is_variadic && tok.matches("__VA_OPT__")){
                    i++;
                    // Skip whitespace
                    while(i < repl.length && repl[i].type == PPTokenType.PP_WHITESPACE) i++;
                    // Expect (
                    if(i >= repl.length || !repl[i].is_punct("(")){
                        continue;
                    }
                    i++;

                    // Collect tokens until matching )
                    size_t opt_start = i;
                    int depth = 1;
                    while(i < repl.length && depth > 0){
                        if(repl[i].is_punct("(")) depth++;
                        else if(repl[i].is_punct(")")){
                            depth--;
                            if(depth == 0) break;
                        }
                        i++;
                    }
                    size_t opt_end = i;
                    if(i < repl.length) i++;  // Skip closing )

                    // Check if __VA_ARGS__ is non-empty
                    bool va_args_nonempty = args.length > macro_def.params.length;
                    if(va_args_nonempty){
                        // Process opt_content with parameter substitution
                        for(size_t oi = opt_start; oi < opt_end; oi++){
                            PPToken otok = repl[oi];
                            if(otok.type == PPTokenType.PP_IDENTIFIER){
                                // Check for parameter
                                int pidx = macro_def.find_param(otok.lexeme);
                                if(pidx >= 0 && pidx < args.length){
                                    foreach(arg_tok; args[pidx]){
                                        if(arg_tok.type != PPTokenType.PP_WHITESPACE){
                                            *output ~= arg_tok;
                                        }
                                    }
                                    continue;
                                }
                                // Check for __VA_ARGS__
                                if(otok.matches("__VA_ARGS__")){
                                    for(size_t ai = macro_def.params.length; ai < args.length; ai++){
                                        if(ai > macro_def.params.length){
                                            PPToken comma;
                                            comma.type = PPTokenType.PP_PUNCTUATOR;
                                            comma.lexeme = ",";
                                            *output ~= comma;
                                        }
                                        foreach(arg_tok; args[ai]){
                                            *output ~= arg_tok;
                                        }
                                    }
                                    continue;
                                }
                            }
                            *output ~= otok;
                        }
                    }
                    // If empty, emit nothing
                    continue;
                }
            }

            // Pass through
            *output ~= tok;
            i++;
        }
    }

    // Stringify a token sequence
    PPToken stringify(PPToken[] tokens){
        StringBuilder sb;
        sb.allocator = allocator;
        sb.write('"');

        bool prev_was_space = true;
        foreach(tok; tokens){
            // Treat newlines same as whitespace - collapse to single space
            if(tok.type == PPTokenType.PP_WHITESPACE || tok.type == PPTokenType.PP_NEWLINE){
                if(!prev_was_space){
                    sb.write(' ');
                    prev_was_space = true;
                }
                continue;
            }
            prev_was_space = false;

            // Escape quotes and backslashes in strings/chars
            if(tok.type == PPTokenType.PP_STRING || tok.type == PPTokenType.PP_CHAR){
                foreach(c; tok.lexeme){
                    if(c == '"' || c == '\\') sb.write('\\');
                    sb.write(c);
                }
            } else {
                sb.write(tok.lexeme);
            }
        }

        sb.write('"');

        PPToken result;
        result.type = PPTokenType.PP_STRING;
        result.lexeme = sb.borrow();
        return result;
    }

    // Paste two tokens together
    PPToken paste_tokens(PPToken left, PPToken right){
        // Handle placemarkers
        if(left.type == PPTokenType.PP_PLACEMARKER) return right;
        if(right.type == PPTokenType.PP_PLACEMARKER) return left;

        // Concatenate lexemes
        StringBuilder sb;
        sb.allocator = allocator;
        sb.write(left.lexeme);
        sb.write(right.lexeme);

        PPToken result;
        result.lexeme = sb.borrow();

        // Determine result type
        // For simplicity, if result looks like identifier, it's identifier
        // otherwise it's punctuator or number
        if(result.lexeme.length > 0 && is_ident_start(result.lexeme[0])){
            result.type = PPTokenType.PP_IDENTIFIER;
        } else if(result.lexeme.length > 0 && is_digit(result.lexeme[0])){
            result.type = PPTokenType.PP_NUMBER;
        } else {
            result.type = PPTokenType.PP_PUNCTUATOR;
        }

        return result;
    }
}

// Expression evaluator for #if
struct ExprEvaluator {
    PPToken[] tokens;
    size_t pos;
    CPreprocessor* pp;

    // Skip whitespace tokens
    void skip_ws(){
        while(pos < tokens.length && tokens[pos].type == PPTokenType.PP_WHITESPACE){
            pos++;
        }
    }

    // Get current token
    PPToken current(){
        skip_ws();
        if(pos >= tokens.length){
            PPToken eof;
            eof.type = PPTokenType.PP_EOF;
            return eof;
        }
        return tokens[pos];
    }

    // Advance to next token
    void advance(){
        pos++;
        skip_ws();
    }

    // Match and consume a punctuator
    bool match_punct(str p){
        skip_ws();
        if(pos < tokens.length && tokens[pos].is_punct(p)){
            pos++;
            return true;
        }
        return false;
    }

    // Conditional expression: a ? b : c
    long eval_conditional(){
        long result = eval_or();
        skip_ws();
        if(match_punct("?")){
            long then_val = eval_conditional();
            if(!match_punct(":")){
                pp.error_at(current(), "Expected ':' in conditional expression");
            }
            long else_val = eval_conditional();
            return result != 0 ? then_val : else_val;
        }
        return result;
    }

    // Logical OR: ||
    long eval_or(){
        long result = eval_and();
        while(match_punct("||")){
            long right = eval_and();
            result = (result != 0 || right != 0) ? 1 : 0;
        }
        return result;
    }

    // Logical AND: &&
    long eval_and(){
        long result = eval_bitor();
        while(match_punct("&&")){
            long right = eval_bitor();
            result = (result != 0 && right != 0) ? 1 : 0;
        }
        return result;
    }

    // Bitwise OR: |
    long eval_bitor(){
        long result = eval_xor();
        while(true){
            skip_ws();
            if(pos < tokens.length && tokens[pos].is_punct("|") &&
                (pos + 1 >= tokens.length || !tokens[pos + 1].is_punct("|"))){
                pos++;
                result = result | eval_xor();
            } else {
                break;
            }
        }
        return result;
    }

    // Bitwise XOR: ^
    long eval_xor(){
        long result = eval_bitand();
        while(match_punct("^")){
            result = result ^ eval_bitand();
        }
        return result;
    }

    // Bitwise AND: &
    long eval_bitand(){
        long result = eval_equality();
        while(true){
            skip_ws();
            if(pos < tokens.length && tokens[pos].is_punct("&") &&
                (pos + 1 >= tokens.length || !tokens[pos + 1].is_punct("&"))){
                pos++;
                result = result & eval_equality();
            } else {
                break;
            }
        }
        return result;
    }

    // Equality: == !=
    long eval_equality(){
        long result = eval_relational();
        while(true){
            if(match_punct("==")){
                result = (result == eval_relational()) ? 1 : 0;
            } else if(match_punct("!=")){
                result = (result != eval_relational()) ? 1 : 0;
            } else {
                break;
            }
        }
        return result;
    }

    // Relational: < > <= >=
    long eval_relational(){
        long result = eval_shift();
        while(true){
            if(match_punct("<=")){
                result = (result <= eval_shift()) ? 1 : 0;
            } else if(match_punct(">=")){
                result = (result >= eval_shift()) ? 1 : 0;
            } else if(match_punct("<")){
                result = (result < eval_shift()) ? 1 : 0;
            } else if(match_punct(">")){
                result = (result > eval_shift()) ? 1 : 0;
            } else {
                break;
            }
        }
        return result;
    }

    // Shift: << >>
    long eval_shift(){
        long result = eval_additive();
        while(true){
            if(match_punct("<<")){
                result = result << eval_additive();
            } else if(match_punct(">>")){
                result = result >> eval_additive();
            } else {
                break;
            }
        }
        return result;
    }

    // Additive: + -
    long eval_additive(){
        long result = eval_multiplicative();
        while(true){
            if(match_punct("+")){
                result = result + eval_multiplicative();
            } else if(match_punct("-")){
                result = result - eval_multiplicative();
            } else {
                break;
            }
        }
        return result;
    }

    // Multiplicative: * / %
    long eval_multiplicative(){
        long result = eval_unary();
        while(true){
            if(match_punct("*")){
                result = result * eval_unary();
            } else if(match_punct("/")){
                long divisor = eval_unary();
                if(divisor == 0){
                    pp.error_at(current(), "Division by zero in preprocessor expression");
                    return 0;
                }
                result = result / divisor;
            } else if(match_punct("%")){
                long divisor = eval_unary();
                if(divisor == 0){
                    pp.error_at(current(), "Modulo by zero in preprocessor expression");
                    return 0;
                }
                result = result % divisor;
            } else {
                break;
            }
        }
        return result;
    }

    // Unary: ! ~ - +
    long eval_unary(){
        if(match_punct("!")){
            return eval_unary() == 0 ? 1 : 0;
        }
        if(match_punct("~")){
            return ~eval_unary();
        }
        if(match_punct("-")){
            return -eval_unary();
        }
        if(match_punct("+")){
            return eval_unary();
        }
        return eval_primary();
    }

    // Primary: number, (expr), identifier
    long eval_primary(){
        skip_ws();

        // Parenthesized expression
        if(match_punct("(")){
            long result = eval_conditional();
            if(!match_punct(")")){
                pp.error_at(current(), "Expected ')' in preprocessor expression");
            }
            return result;
        }

        PPToken tok = current();

        // Number
        if(tok.type == PPTokenType.PP_NUMBER){
            advance();
            return parse_number(tok.lexeme);
        }

        // Identifier - undefined identifiers evaluate to 0
        if(tok.type == PPTokenType.PP_IDENTIFIER){
            advance();
            return 0;
        }

        // Character literal
        if(tok.type == PPTokenType.PP_CHAR){
            advance();
            return parse_char(tok.lexeme);
        }

        return 0;
    }

    // Parse a number literal
    long parse_number(str s){
        if(s.length == 0) return 0;

        // Handle hex
        if(s.length >= 2 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X')){
            long result = 0;
            for(size_t i = 2; i < s.length; i++){
                ubyte c = s[i];
                if(c >= '0' && c <= '9'){
                    result = result * 16 + (c - '0');
                } else if(c >= 'a' && c <= 'f'){
                    result = result * 16 + (c - 'a' + 10);
                } else if(c >= 'A' && c <= 'F'){
                    result = result * 16 + (c - 'A' + 10);
                } else {
                    break;  // Suffix like L, U, etc.
                }
            }
            return result;
        }

        // Handle octal
        if(s.length >= 1 && s[0] == '0'){
            long result = 0;
            for(size_t i = 1; i < s.length; i++){
                ubyte c = s[i];
                if(c >= '0' && c <= '7'){
                    result = result * 8 + (c - '0');
                } else {
                    break;
                }
            }
            return result;
        }

        // Decimal
        long result = 0;
        for(size_t i = 0; i < s.length; i++){
            ubyte c = s[i];
            if(c >= '0' && c <= '9'){
                result = result * 10 + (c - '0');
            } else {
                break;
            }
        }
        return result;
    }

    // Parse a character literal
    long parse_char(str s){
        if(s.length < 3) return 0;  // Need at least 'x'
        size_t i = 1;  // Skip opening quote

        if(s[i] == '\\' && i + 1 < s.length - 1){
            // Escape sequence
            switch(s[i + 1]){
                case 'n': return '\n';
                case 't': return '\t';
                case 'r': return '\r';
                case '0': return 0;
                case '\\': return '\\';
                case '\'': return '\'';
                case '"': return '"';
                default: return s[i + 1];
            }
        }

        return s[i];
    }
}
