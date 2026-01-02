/*
 * C Preprocessor (Phase 4)
 * Processes directives and expands macros on token stream
 * Copyright 2025, David Priver
 */
module cfront.c_preprocessor;

import core.stdc.stdio : fprintf, stderr;
import dlib.aliases;
import dlib.allocator : Allocator;
import dlib.barray : Barray, make_barray;
import dlib.table : Table;
import dlib.stringbuilder : StringBuilder, mwritef, E;
import dlib.file_util : read_file, FileResult, FileFlags;
import dlib.box: Box, boxed;
import cfront.c_pp_token;
import cfront.c_pp_lexer : pp_tokenize;

struct CPreprocessor {
    Allocator allocator;

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
    str current_file;

    bool error_occurred = false;

    // Initialize with predefined macros
    void init(){
        macros.data.allocator = allocator;
        cond_stack = make_barray!PPCondBlock(allocator);
        include_stack = make_barray!PPIncludeFrame(allocator);
        pragma_once_files.data.allocator = allocator;
        include_guards.data.allocator = allocator;

        // Define built-in macros
        define_builtin_macros();
    }

    void define_builtin_macros(){
        // Platform macros
        define_object_macro("__STDC__", "1");
        define_object_macro("__STDC_VERSION__", "201710L");
        define_object_macro("__FILE__", builtin:true);
        define_object_macro("__LINE__", builtin:true);

        // Size macros - platform dependent
        define_object_macro("__SIZEOF_INT__", "4");
        define_object_macro("__SIZEOF_SHORT__", "2");
        define_object_macro("__SIZEOF_LONG_LONG__", "8");
        version(D_LP64){
            define_object_macro("__SIZEOF_LONG__", "8");
            define_object_macro("__SIZEOF_POINTER__", "8");
            define_object_macro("__LP64__", "1");
        } else {
            define_object_macro("__SIZEOF_LONG__", "4");
            define_object_macro("__SIZEOF_POINTER__", "4");
        }

        // OS detection
        version(OSX){
            define_object_macro("__APPLE__", "1");
            define_object_macro("__MACH__", "1");
            define_object_macro("__APPLE_CC__", "1");  // For TargetConditionals.h
            define_object_macro("MAC_OS_X_VERSION_MIN_REQUIRED", "110000");  // macOS 11.0
            define_object_macro("MAC_OS_X_VERSION_MAX_ALLOWED", "150000");  // macOS 15.0
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

        // GCC compatibility
        define_object_macro("__GNUC__", "4");
        define_object_macro("__GNUC_MINOR__", "2");
        define_object_macro("__GNUC_PATCHLEVEL__", "1");

        // DDASM compiler identification
        define_object_macro("__DDASM__", "1");

        // Common empty macros
        define_object_macro("__extension__", "");
        define_object_macro("volatile", "");
        define_object_macro("__volatile__", "");

        // Function-like macros that expand to nothing
        define_empty_func_macro("__attribute__", 1);

        // Compiler feature checks - always return 0 (not supported)
        define_func_macro_const("__has_feature", 1, "0");
        define_func_macro_const("__has_extension", 1, "0");
        define_func_macro_const("__has_builtin", 1, "0");
        define_func_macro_const("__has_attribute", 1, "0");
        define_func_macro_const("__has_include", 1, "0");
        define_func_macro_const("__has_include_next", 1, "0");

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
            // Remove EOF token
            if(value_tokens.count > 0 && value_tokens[value_tokens.count - 1].type == PPTokenType.PP_EOF){
                value_tokens.count--;
            }
            // Remove whitespace tokens
            Barray!PPToken filtered = make_barray!PPToken(allocator);
            foreach(tok; value_tokens[]){
                if(tok.type != PPTokenType.PP_WHITESPACE && tok.type != PPTokenType.PP_NEWLINE){
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

    // Define a function-like macro that expands to its argument (identity macro)
    // e.g., _Atomic(x) -> x
    void define_identity_macro(str name){
        PPMacroDef def;
        def.name = name;
        def.is_function_like = true;
        def.is_variadic = false;
        def.is_undefined = false;

        // Single parameter named "x"
        static immutable str[1] param_names = ["x"];
        def.params = cast(str[])param_names;

        // Replacement is just the parameter reference
        Barray!PPToken repl = make_barray!PPToken(allocator);
        PPToken tok;
        tok.type = PPTokenType.PP_IDENTIFIER;
        tok.lexeme = "x";
        repl ~= tok;
        def.replacement = repl[];

        macros[name] = def;
    }

    // Check if a macro is defined and not #undef'd
    bool is_defined(str name){
        foreach(item; macros.items){
            if(item.key == name){
                return !item.value.is_undefined;
            }
        }
        return false;
    }
    PPMacroDef get_file_macro(PPToken tok){
        PPToken file_tok = tok;
        file_tok.type = PPTokenType.PP_STRING;
        // Build quoted string for filename: "filename"
        // FIXME: intern this string
        file_tok.lexeme = mwritef(allocator, "\"%\"", E(current_file))[];
        Box!PPToken box = boxed(allocator, &file_tok);
        return PPMacroDef(tok.lexeme, box[], is_builtin:true);
    }
    PPMacroDef get_line_macro(PPToken tok){
        PPToken line_tok = tok;
        line_tok.type = PPTokenType.PP_NUMBER;
        // FIXME: intern this string
        line_tok.lexeme = mwritef(allocator, "%", tok.line)[];
        Box!PPToken box = boxed(allocator, &line_tok);
        return PPMacroDef(tok.lexeme, box[], is_builtin:true);
    }

    // Get macro definition
    PPMacroDef get_macro(PPToken tok){
        // Handle special predefined macros
        str name = tok.lexeme;
        if(name == "__FILE__")
            return get_file_macro(tok);
        if(name == "__LINE__")
            return get_line_macro(tok);
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
    void error(str msg){
        error_occurred = true;
        fprintf(stderr, "%.*s: Preprocessor error: %.*s\n",
                cast(int)current_file.length, current_file.ptr,
                cast(int)msg.length, msg.ptr);
    }

    void error_at(PPToken tok, str msg){
        error_occurred = true;
        fprintf(stderr, "%.*s:%d:%d: Preprocessor error: %.*s\n",
                cast(int)tok.file.length, tok.file.ptr,
                tok.line, tok.column,
                cast(int)msg.length, msg.ptr);
    }

    // Main entry point: process PPTokens
    int process(PPToken[] input, Barray!PPToken* output){
        size_t i = 0;

        while(i < input.length){
            // Check for directive at start of line (# as first non-whitespace)
            if(is_at_line_start(input, i) && is_hash(input, i)){
                i = process_directive(input, i, output);
                if(error_occurred) return 1;
                continue;
            }

            PPToken tok = input[i];

            // Skip tokens in inactive blocks
            if(!is_active()){
                i++;
                continue;
            }

            // Handle identifiers - check for macro expansion
            if(tok.type == PPTokenType.PP_IDENTIFIER){
                PPMacroDef macro_def = get_macro(tok);
                if(!macro_def.is_null){
                    HideSet hs = HideSet.create(allocator);
                    i = expand_macro_invocation(input, i, macro_def, hs, output);
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
            // Ignore warnings
        } else if(directive == "line"){
            // Ignore #line
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

        // Mark as undefined
        foreach(ref item; macros.items){
            if(item.key == name){
                item.value.is_undefined = true;
                return;
            }
        }
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
            fprintf(stderr, "%.*s:%d: error: '%.*s' file not found\n",
                cast(int)current_file.length, current_file.ptr,
                tokens[start].line,
                cast(int)filename.length, filename.ptr);
            error_occurred = true;
            return line_end + 1;
        }

        // Check for already included (pragma once)
        foreach(item; pragma_once_files.items){
            if(item.key == full_path){
                if(0)fprintf(stderr, "Skipping due to pragma once: '%.*s'\n", cast(int)full_path.length, full_path.ptr);
                return line_end + 1;
            }
        }

        // Check for include guard - if we know the guard macro and it's defined, skip
        if(str* guard = full_path in include_guards){
            if(is_defined(*guard)){
                if(0)fprintf(stderr, "Skipping '%.*s' due to include guard '%.*s'\n", cast(int)full_path.length, full_path.ptr, cast(int)guard.length, guard.ptr);
                return line_end + 1;
            }
        }

        // Read file - need to null-terminate path for C API
        StringBuilder path_buf;
        path_buf.allocator = allocator;
        path_buf.write(full_path);
        path_buf.nul_terminate();

        FileResult fr = read_file(path_buf.borrow().ptr, allocator, FileFlags.NUL_TERMINATE | FileFlags.ZERO_PAD_TO_16);
        if(fr.errored){
            return line_end + 1;
        }

        // Tokenize included file - find actual content length (before NUL terminator)
        const(ubyte)[] file_data = cast(const(ubyte)[])fr.value.data;
        size_t actual_len = 0;
        while(actual_len < file_data.length && file_data[actual_len] != 0){
            actual_len++;
        }

        Barray!PPToken include_tokens = make_barray!PPToken(allocator);
        int err = pp_tokenize(file_data[0 .. actual_len], full_path, &include_tokens, allocator);
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

        // Save current file
        str saved_file = current_file;
        current_file = full_path;

        // Process included tokens recursively
        process(include_tokens[], output);

        // Restore current file
        current_file = saved_file;

        return line_end + 1;
    }

    // Resolve include path
    str resolve_include(str filename, bool is_system){
        // Handle absolute paths
        if(filename.length > 0 && filename[0] == '/'){
            if(file_exists(filename)) return filename;
            return "";
        }

        // For non-system includes, try relative to current file first
        if(!is_system && current_file.length > 0){
            // Extract directory from current file
            str dir = get_directory(current_file);
            if(dir.length > 0){
                str path = concat_path(dir, filename);
                if(file_exists(path)) return path;
            }
        }

        version(OSX){
            // Try framework paths (e.g., Foo/Bar.h -> Foo.framework/Headers/Bar.h)
            str framework_result = resolve_framework_include(filename);
            if(framework_result.length > 0) return framework_result;
        }

        // Try configured include paths
        foreach(base; include_paths){
            str path = concat_path(base, filename);
            if(file_exists(path)) return path;
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
            if(file_exists(path)) return path;
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

    // Check if file exists
    bool file_exists(str path){
        StringBuilder path_buf;
        path_buf.allocator = allocator;
        path_buf.write(path);
        path_buf.nul_terminate();
        FileResult fr = read_file(path_buf.borrow().ptr, allocator, FileFlags.NONE);
        return !fr.errored;
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
        fprintf(stderr, "%.*s: #error %.*s\n",
                cast(int)current_file.length, current_file.ptr,
                cast(int)sb.cursor, sb.borrow().ptr);
    }

    // Evaluate preprocessor expression
    long evaluate_expression(PPToken[] tokens){
        // First expand macros (except in defined())
        Barray!PPToken expanded = make_barray!PPToken(allocator);
        expand_for_if(tokens, &expanded);

        // Then evaluate
        ExprEvaluator eval;
        eval.tokens = expanded[];
        eval.pos = 0;
        eval.pp = &this;
        return eval.eval_conditional();
    }

    // Expand macros for #if evaluation (handles defined() specially)
    void expand_for_if(PPToken[] tokens, Barray!PPToken* output){
        size_t i = 0;
        while(i < tokens.length){
            PPToken tok = tokens[i];

            // Skip whitespace
            if(tok.type == PPTokenType.PP_WHITESPACE){
                i++;
                continue;
            }

            // Handle defined operator
            if(tok.type == PPTokenType.PP_IDENTIFIER && tok.matches("defined")){
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

            // Handle __has_include, __has_include_next specially (they take header-name args)
            if(tok.type == PPTokenType.PP_IDENTIFIER &&
                (tok.matches("__has_include") || tok.matches("__has_include_next"))){
                i++;
                // Skip whitespace
                while(i < tokens.length && tokens[i].type == PPTokenType.PP_WHITESPACE) i++;
                // Skip (
                if(i < tokens.length && tokens[i].is_punct("(")){
                    i++;
                    // Skip until matching )
                    int depth = 1;
                    while(i < tokens.length && depth > 0){
                        if(tokens[i].is_punct("(")) depth++;
                        else if(tokens[i].is_punct(")")) depth--;
                        i++;
                    }
                }
                // Emit 0 (we don't support __has_include)
                PPToken result;
                result.type = PPTokenType.PP_NUMBER;
                result.lexeme = "0";
                *output ~= result;
                continue;
            }

            // Check for macro to expand
            if(tok.type == PPTokenType.PP_IDENTIFIER){
                PPMacroDef macro_def = get_macro(tok);
                if(!macro_def.is_null && !macro_def.is_function_like){
                    // Expand object-like macro and recursively process
                    i++;
                    expand_for_if(macro_def.replacement, output);
                    continue;
                }
                // Handle function-like macros
                if(!macro_def.is_null && macro_def.is_function_like){
                    // Check for(
                    size_t j = i + 1;
                    while(j < tokens.length && tokens[j].type == PPTokenType.PP_WHITESPACE) j++;
                    if(j < tokens.length && tokens[j].is_punct("(")){
                        // Expand the macro to a temporary buffer
                        Barray!PPToken expanded = make_barray!PPToken(allocator);
                        HideSet hs = HideSet.create(allocator);
                        i = expand_macro_invocation(tokens, i, macro_def, hs, &expanded);
                        // Recursively process the expanded tokens for defined() etc.
                        expand_for_if(expanded[], output);
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
                        // End of arguments
                        if(current_arg.count > 0 || args.count > 0){
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

            // Set expansion location on all substituted tokens so nested macros
            // have the right invocation point
            foreach(ref tok; substituted[]){
                tok = with_expansion_loc(tok, invocation);
            }

            // Rescan for more macros
            size_t j = 0;
            while(j < substituted.count){
                PPToken tok = substituted[j];
                if(tok.type == PPTokenType.PP_IDENTIFIER){
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
            // Object-like macro
            Barray!PPToken substituted = make_barray!PPToken(allocator);
            foreach(tok; macro_def.replacement){
                // Set expansion location on replacement tokens so nested macros
                // have the right invocation point
                substituted ~= with_expansion_loc(tok, invocation);
            }

            // Rescan for more macros
            size_t j = 0;
            while(j < substituted.count){
                PPToken tok = substituted[j];
                if(tok.type == PPTokenType.PP_IDENTIFIER){
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
                        if(tok.type == PPTokenType.PP_IDENTIFIER){
                            int param_idx = macro_def.find_param(tok.lexeme);
                            if(param_idx >= 0 && param_idx < args.length && args[param_idx].length > 0){
                                // Get last non-whitespace token
                                PPToken[] arg = args[param_idx];
                                for(size_t ai = arg.length; ai > 0; ai--){
                                    if(arg[ai - 1].type != PPTokenType.PP_WHITESPACE){
                                        left = arg[ai - 1];
                                        break;
                                    }
                                }
                            }
                        }

                        // Get right token (possibly parameter)
                        PPToken right = repl[k];
                        if(right.type == PPTokenType.PP_IDENTIFIER){
                            int param_idx = macro_def.find_param(right.lexeme);
                            if(param_idx >= 0 && param_idx < args.length && args[param_idx].length > 0){
                                // Get first non-whitespace token
                                PPToken[] arg = args[param_idx];
                                foreach(argtok; arg){
                                    if(argtok.type != PPTokenType.PP_WHITESPACE){
                                        right = argtok;
                                        break;
                                    }
                                }
                            }
                        }

                        // Paste tokens
                        PPToken pasted = paste_tokens(left, right);
                        *output ~= pasted;
                        i = k + 1;
                        continue;
                    }
                }
            }

            // Regular parameter substitution
            if(tok.type == PPTokenType.PP_IDENTIFIER){
                int param_idx = macro_def.find_param(tok.lexeme);
                if(param_idx >= 0){
                    if(param_idx < args.length){
                        // Expand argument before substitution
                        foreach(arg_tok; args[param_idx]){
                            if(arg_tok.type != PPTokenType.PP_WHITESPACE){
                                *output ~= arg_tok;
                            }
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
            if(tok.type == PPTokenType.PP_WHITESPACE){
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
