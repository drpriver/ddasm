/*
 * C Front-End Parser for ddasm
 * Copyright 2025, David Priver
 */
module cfront.c_parser;

import core.stdc.stdio : fprintf, stderr;
import dlib.aliases;
import dlib.allocator : Allocator;
import dlib.barray : Barray, make_barray;
import dlib.stringbuilder : StringBuilder;
import dlib.table : Table;

import cfront.c_pp_to_c : CToken, CTokenType;
import cfront.c_ast;

struct CParser {
    Allocator allocator;
    CToken[] tokens;
    int current = 0;
    bool ERROR_OCCURRED = false;
    str current_library;  // Set by #pragma library("...")
    int switch_depth = 0;  // Track nesting level in switch statements for case/default
    Table!(str, CType*) struct_types;  // Defined struct types
    Table!(str, CType*) union_types;   // Defined union types
    Table!(str, CType*) enum_types;    // Defined enum types
    Table!(str, long) enum_constants;  // Enum constant values (name -> value)
    Table!(str, CType*) typedef_types; // Typedef aliases (name -> type)
    Table!(str, CType*) global_var_types; // Global variable types (for sizeof in const exprs)

    void error(CToken token, str message){
        ERROR_OCCURRED = true;
        // Show expansion location first (where macro was used), then definition location
        if(token.expansion_file.length > 0){
            fprintf(stderr, "%.*s:%d:%d: Parse Error at '%.*s': %.*s\n",
                        cast(int)token.expansion_file.length, token.expansion_file.ptr,
                        token.expansion_line, token.expansion_column,
                        cast(int)token.lexeme.length, token.lexeme.ptr,
                        cast(int)message.length, message.ptr);
            fprintf(stderr, "  note: expanded from macro defined at %.*s:%d:%d\n",
                        cast(int)token.file.length, token.file.ptr,
                        token.line, token.column);
        } else {
            fprintf(stderr, "%.*s:%d:%d: Parse Error at '%.*s': %.*s\n",
                        cast(int)token.file.length, token.file.ptr,
                        token.line, token.column,
                        cast(int)token.lexeme.length, token.lexeme.ptr,
                        cast(int)message.length, message.ptr);
        }
    }

    void error(str message){
        error(peek(), message);
    }

    // =========================================================================
    // Top-Level Parsing
    // =========================================================================

    int parse(CTranslationUnit* unit){
        auto functions = make_barray!CFunction(allocator);
        auto globals = make_barray!CGlobalVar(allocator);
        auto structs = make_barray!CStructDef(allocator);
        auto unions = make_barray!CUnionDef(allocator);
        auto enums = make_barray!CEnumDef(allocator);

        // Track function names to indices for merging forward declarations with definitions
        Table!(str, size_t) func_indices;
        func_indices.data.allocator = allocator;
        scope(exit) func_indices.cleanup();

        // Helper to add function, merging forward declarations with definitions
        void add_function(CFunction func){
            str fname = func.name.lexeme;
            if(auto idx_ptr = fname in func_indices){
                // Already have an entry for this function
                size_t idx = *idx_ptr;
                if(func.is_definition && !functions[idx].is_definition){
                    // Current is definition, existing is declaration - replace
                    functions[idx] = func;
                }
                // Otherwise (existing is definition or both declarations): skip
            } else {
                // New function
                func_indices[fname] = functions.count;
                functions ~= func;
            }
        }

        // Initialize type tables
        struct_types.data.allocator = allocator;
        union_types.data.allocator = allocator;
        enum_types.data.allocator = allocator;
        enum_constants.data.allocator = allocator;
        typedef_types.data.allocator = allocator;
        global_var_types.data.allocator = allocator;

        while(!at_end){
            // Handle #pragma (emitted as # pragma library ( "..." ) tokens)
            if(check(CTokenType.HASH) && peek_at(1).type == CTokenType.IDENTIFIER && peek_at(1).lexeme == "pragma"){
                handle_pragma();
                continue;
            }

            // Skip empty statements (from macros that expand to nothing)
            if(match(CTokenType.SEMICOLON)){
                continue;
            }

            // Track storage class specifiers (can appear in any order)
            // extern: for functions, this is the default; for objects, means defined elsewhere (dlimport)
            bool saw_extern = false;
            bool saw_inline = false;
            while(true){
                if(match(CTokenType.EXTERN)){ saw_extern = true; }
                else if(match(CTokenType.INLINE)){ saw_inline = true; }
                else if(match(CTokenType.NORETURN)){ /* skip */ }
                else if(check(CTokenType.IDENTIFIER) && peek().lexeme == "__forceinline"){
                    advance();
                    saw_inline = true;
                }
                else break;
            }

            if(check(CTokenType.STRUCT)){
                // Check if this is a struct definition or forward declaration
                // Look ahead: struct Name { ... means definition
                //             struct Name ; means forward declaration
                //             struct Name var... means variable of struct type
                if(peek_at(1).type == CTokenType.IDENTIFIER &&
                    peek_at(2).type == CTokenType.LEFT_BRACE){
                    CStructDef sdef;
                    int err = parse_struct_def(&sdef);
                    if(err) return err;
                    structs ~= sdef;
                } else if(peek_at(1).type == CTokenType.IDENTIFIER &&
                           peek_at(2).type == CTokenType.SEMICOLON){
                    // Forward declaration: struct Name;
                    advance();  // consume 'struct'
                    CToken struct_name = advance();  // consume name
                    advance();  // consume ';'

                    // Create incomplete struct type if not already defined
                    if((struct_name.lexeme in struct_types) is null){
                        auto data = allocator.alloc(CType.sizeof);
                        CType* incomplete = cast(CType*)data.ptr;
                        incomplete.kind = CTypeKind.STRUCT;
                        incomplete.struct_name = struct_name.lexeme;
                        incomplete.struct_size = 0;  // Unknown size
                        struct_types[struct_name.lexeme] = incomplete;
                    }
                } else {
                    // It's a variable/function with struct type
                    CType* type_ = parse_type();
                    if(type_ is null) return 1;

                    CToken name = consume(CTokenType.IDENTIFIER, "Expected identifier");
                    if(ERROR_OCCURRED) return 1;

                    if(check(CTokenType.LEFT_PAREN)){
                        CFunction func;
                        int err = parse_function_rest(type_, name, &func, saw_inline);
                        if(err) return err;
                        add_function(func);
                    } else {
                        // Global variable - use unified init-declarator-list parsing
                        int err = parse_init_declarator_list(&globals, type_, name, saw_extern, current_library);
                        if(err) return err;
                        consume(CTokenType.SEMICOLON, "Expected ';' after variable declaration");
                        if(ERROR_OCCURRED) return 1;
                    }
                }
            } else if(check(CTokenType.UNION)){
                // Check if this is a union definition or forward declaration
                // Look ahead: union Name { ... means definition
                //             union Name ; means forward declaration
                //             union Name var... means variable of union type
                if(peek_at(1).type == CTokenType.IDENTIFIER &&
                    peek_at(2).type == CTokenType.LEFT_BRACE){
                    CUnionDef udef;
                    int err = parse_union_def(&udef);
                    if(err) return err;
                    unions ~= udef;
                } else if(peek_at(1).type == CTokenType.IDENTIFIER &&
                           peek_at(2).type == CTokenType.SEMICOLON){
                    // Forward declaration: union Name;
                    advance();  // consume 'union'
                    CToken union_name = advance();  // consume name
                    advance();  // consume ';'

                    // Create incomplete union type if not already defined
                    if((union_name.lexeme in union_types) is null){
                        auto data = allocator.alloc(CType.sizeof);
                        CType* incomplete = cast(CType*)data.ptr;
                        incomplete.kind = CTypeKind.UNION;
                        incomplete.struct_name = union_name.lexeme;
                        incomplete.struct_size = 0;  // Unknown size
                        union_types[union_name.lexeme] = incomplete;
                    }
                } else {
                    // It's a variable/function with union type
                    CType* type_ = parse_type();
                    if(type_ is null) return 1;

                    CToken name = consume(CTokenType.IDENTIFIER, "Expected identifier");
                    if(ERROR_OCCURRED) return 1;

                    if(check(CTokenType.LEFT_PAREN)){
                        CFunction func;
                        int err = parse_function_rest(type_, name, &func, saw_inline);
                        if(err) return err;
                        add_function(func);
                    } else {
                        // Global variable - use unified init-declarator-list parsing
                        int err = parse_init_declarator_list(&globals, type_, name, saw_extern, current_library);
                        if(err) return err;
                        consume(CTokenType.SEMICOLON, "Expected ';' after variable declaration");
                        if(ERROR_OCCURRED) return 1;
                    }
                }
            } else if(check(CTokenType.ENUM)){
                // Check if this is an enum definition
                // Look ahead: enum Name { ... means named definition
                //             enum { ... means anonymous definition
                //             enum Name var... means variable of enum type
                bool is_anon_enum = peek_at(1).type == CTokenType.LEFT_BRACE;
                bool is_named_enum = peek_at(1).type == CTokenType.IDENTIFIER &&
                                     peek_at(2).type == CTokenType.LEFT_BRACE;
                if(is_anon_enum || is_named_enum){
                    CEnumDef edef;
                    int err = parse_enum_def(&edef);
                    if(err) return err;
                    enums ~= edef;
                } else {
                    // It's a variable/function with enum type
                    CType* type_ = parse_type();
                    if(type_ is null) return 1;

                    CToken name = consume(CTokenType.IDENTIFIER, "Expected identifier");
                    if(ERROR_OCCURRED) return 1;

                    if(check(CTokenType.LEFT_PAREN)){
                        CFunction func;
                        int err = parse_function_rest(type_, name, &func, saw_inline);
                        if(err) return err;
                        add_function(func);
                    } else {
                        // Global variable - use unified init-declarator-list parsing
                        int err = parse_init_declarator_list(&globals, type_, name, saw_extern, current_library);
                        if(err) return err;
                        consume(CTokenType.SEMICOLON, "Expected ';' after variable declaration");
                        if(ERROR_OCCURRED) return 1;
                    }
                }
            } else if(check(CTokenType.TYPEDEF)){
                // Parse typedef declaration
                int err = parse_typedef(&enums);
                if(err) return err;
            } else if(match(CTokenType.STATIC_ASSERT)){
                // _Static_assert(expr, "message");
                int err = parse_static_assert();
                if(err) return err;
            } else if(match(CTokenType.STATIC)){
                // Static function or variable
                // Check for inline/noreturn after static (can appear in any order)
                bool saw_static_inline = false;
                while(true){
                    if(match(CTokenType.INLINE)){ saw_static_inline = true; }
                    else if(match(CTokenType.NORETURN)){ /* skip */ }
                    else if(check(CTokenType.IDENTIFIER) && peek().lexeme == "__forceinline"){
                        advance();
                        saw_static_inline = true;
                    }
                    else break;
                }

                // Parse the type and name
                CType* type_ = parse_type();
                if(type_ is null){
                    // Skip to semicolon or brace on error
                    int brace_depth = 0;
                    while(!at_end){
                        if(check(CTokenType.LEFT_BRACE)){ brace_depth++; advance(); }
                        else if(check(CTokenType.RIGHT_BRACE)){ brace_depth--; advance(); if(brace_depth == 0) break; }
                        else if(check(CTokenType.SEMICOLON) && brace_depth == 0){ advance(); break; }
                        else advance();
                    }
                    continue;
                }

                CToken name = consume(CTokenType.IDENTIFIER, "Expected identifier");
                if(ERROR_OCCURRED) return 1;

                if(check(CTokenType.LEFT_PAREN)){
                    // Static function - parse it but mark as static
                    CFunction func;
                    int err = parse_function_rest(type_, name, &func, saw_static_inline);
                    if(err) return err;
                    func.is_static = true;
                    add_function(func);
                } else {
                    // Static variable - skip (internal linkage, not relevant for us)
                    int brace_depth = 0;
                    while(!at_end){
                        if(check(CTokenType.LEFT_BRACE)){ brace_depth++; advance(); }
                        else if(check(CTokenType.RIGHT_BRACE)){ brace_depth--; advance(); if(brace_depth == 0) break; }
                        else if(check(CTokenType.SEMICOLON) && brace_depth == 0){ advance(); break; }
                        else advance();
                    }
                }
            } else if(check(CTokenType.IDENTIFIER) && peek_at(1).type == CTokenType.LEFT_PAREN &&
                       (peek().lexeme in typedef_types) is null){
                // Unknown identifier (not a typedef) followed by ( - likely a function-like macro invocation
                // Skip it: identifier(...);
                advance();  // consume identifier
                skip_balanced_parens();
                match(CTokenType.SEMICOLON);  // consume optional semicolon
            } else {
                // Parse type and declarator, then decide if it's a function or global
                CType* base_type = parse_type();
                if(base_type is null) return 1;

                // Check for complex declarator starting with '(' (function pointer, pointer-to-array)
                if(check(CTokenType.LEFT_PAREN)){
                    // Parse the full declarator using the new infrastructure
                    CDeclarator* decl = parse_declarator(false);
                    if(decl is null) return 1;

                    // Build the final type
                    CType* final_type = apply_declarator_to_type(base_type, decl);

                    // Get the name from the declarator (may be in nested declarator)
                    // Also find the declarator that contains the name to check if it's a function
                    CToken name = decl.name;
                    CDeclarator* name_decl = decl;  // The declarator containing the name
                    if(name.lexeme.length == 0 && decl.nested !is null){
                        // Name is in a nested declarator - find it
                        CDeclarator* inner = decl.nested;
                        while(inner !is null && inner.name.lexeme.length == 0){
                            inner = inner.nested;
                        }
                        if(inner !is null){
                            name = inner.name;
                            name_decl = inner;
                        }
                    }

                    // If the declarator containing the name is a function declarator,
                    // then this is a function declaration, not a variable.
                    // e.g., void(*signal(int, void(*)(int)))(int) - signal is in a function-declarator
                    if(name_decl.is_function){
                        // Function declaration/definition
                        CFunction func;
                        func.name = name;
                        // Build return type from declarator parts outside the function containing the name
                        func.return_type = build_function_return_type(base_type, decl, name_decl);
                        // Extract params from the name_decl (the function declarator containing the name)
                        auto params = make_barray!CParam(allocator);
                        for(size_t i = 0; i < name_decl.param_types.length; i++){
                            CParam p;
                            p.type = name_decl.param_types[i];
                            // No name available from declarator parsing
                            params ~= p;
                        }
                        func.params = params[];
                        func.is_varargs = name_decl.is_varargs;
                        func.is_definition = false;
                        func.is_inline = saw_inline;
                        func.library = current_library;

                        // Check for function body
                        if(match(CTokenType.LEFT_BRACE)){
                            func.is_definition = true;
                            // Skip function body
                            int brace_depth = 1;
                            while(!at_end && brace_depth > 0){
                                if(check(CTokenType.LEFT_BRACE)) brace_depth++;
                                else if(check(CTokenType.RIGHT_BRACE)) brace_depth--;
                                advance();
                            }
                        } else {
                            consume(CTokenType.SEMICOLON, "Expected ';' after function declaration");
                            if(ERROR_OCCURRED) return 1;
                        }
                        add_function(func);
                    } else {
                        // It's a global variable (function pointer, pointer-to-array, etc.)
                        CGlobalVar gvar;
                        gvar.name = name;
                        gvar.var_type = final_type;
                        gvar.initializer = null;
                        gvar.is_extern = saw_extern;
                        gvar.library = current_library;

                        // Check for initializer
                        if(match(CTokenType.EQUAL)){
                            gvar.initializer = parse_initializer();
                            if(gvar.initializer is null) return 1;
                            // If there's an initializer, this is a definition, not extern
                            gvar.is_extern = false;
                        }

                        consume(CTokenType.SEMICOLON, "Expected ';' after variable declaration");
                        if(ERROR_OCCURRED) return 1;

                        globals ~= gvar;
                    }
                } else {
                    CToken name = consume(CTokenType.IDENTIFIER, "Expected identifier");
                    if(ERROR_OCCURRED) return 1;

                    if(check(CTokenType.LEFT_PAREN)){
                        // It's a function
                        CFunction func;
                        int err = parse_function_rest(base_type, name, &func, saw_inline);
                        if(err) return err;
                        add_function(func);
                    } else {
                        // Global variable - use unified init-declarator-list parsing
                        int err = parse_init_declarator_list(&globals, base_type, name, saw_extern, current_library);
                        if(err) return err;
                        consume(CTokenType.SEMICOLON, "Expected ';' after variable declaration");
                        if(ERROR_OCCURRED) return 1;
                    }
                }
            }
        }

        unit.functions = functions[];
        unit.globals = globals[];
        unit.structs = structs[];
        unit.unions = unions[];
        unit.enums = enums[];
        unit.current_library = current_library;
        return 0;
    }

    void handle_pragma(){
        advance();  // consume #
        advance();  // consume pragma

        // Check for library("...")
        if(check(CTokenType.IDENTIFIER) && peek().lexeme == "library"){
            advance();  // consume library
            if(match(CTokenType.LEFT_PAREN)){
                if(check(CTokenType.STRING)){
                    str lib = peek().lexeme;
                    // Remove quotes from string literal
                    if(lib.length >= 2 && lib[0] == '"' && lib[$ - 1] == '"'){
                        current_library = lib[1 .. $ - 1];
                    }
                    advance();  // consume string
                }
                match(CTokenType.RIGHT_PAREN);
            }
        }

        // Skip to end of pragma (until newline or EOF)
        while(!at_end && !check(CTokenType.EOF)){
            // Just advance past any remaining tokens on this line
            // Since we don't have newline tokens in CToken stream, just break
            break;
        }
    }

    // (6.7.3.2) struct-or-union-specifier:
    //     struct-or-union identifier_opt { struct-declaration-list }
    //     struct-or-union identifier
    int parse_struct_def(CStructDef* sdef){
        advance();  // consume 'struct'
        CToken name = consume(CTokenType.IDENTIFIER, "Expected struct name");
        if(ERROR_OCCURRED) return 1;

        consume(CTokenType.LEFT_BRACE, "Expected '{' after struct name");
        if(ERROR_OCCURRED) return 1;

        // Parse fields using unified member parsing
        auto fields = make_barray!StructField(allocator);
        size_t total_size = 0;
        int err = parse_member_declaration_list(&fields, &total_size, false);  // is_union=false
        if(err) return err;

        consume(CTokenType.RIGHT_BRACE, "Expected '}' after struct fields");
        if(ERROR_OCCURRED) return 1;

        consume(CTokenType.SEMICOLON, "Expected ';' after struct definition");
        if(ERROR_OCCURRED) return 1;

        // Check if there's an existing forward-declared type to update
        CType* struct_type;
        if(auto existing = name.lexeme in struct_types){
            // Update existing type in-place so typedefs continue to work
            struct_type = *existing;
            struct_type.fields = fields[];
            struct_type.struct_size = total_size;
        } else {
            // Create new struct type
            struct_type = make_struct_type(allocator, name.lexeme, fields[], total_size);
            struct_types[name.lexeme] = struct_type;
        }

        sdef.name = name;
        sdef.struct_type = struct_type;
        return 0;
    }

    // (6.7.3.2) struct-or-union-specifier:
    //     struct-or-union identifier_opt { struct-declaration-list }
    //     struct-or-union identifier
    int parse_union_def(CUnionDef* udef){
        advance();  // consume 'union'
        CToken name = consume(CTokenType.IDENTIFIER, "Expected union name");
        if(ERROR_OCCURRED) return 1;

        consume(CTokenType.LEFT_BRACE, "Expected '{' after union name");
        if(ERROR_OCCURRED) return 1;

        // Parse fields using unified member parsing
        auto fields = make_barray!StructField(allocator);
        size_t max_size = 0;
        int err = parse_member_declaration_list(&fields, &max_size, true);  // is_union=true
        if(err) return err;

        consume(CTokenType.RIGHT_BRACE, "Expected '}' after union fields");
        if(ERROR_OCCURRED) return 1;

        consume(CTokenType.SEMICOLON, "Expected ';' after union definition");
        if(ERROR_OCCURRED) return 1;

        // Check if there's an existing forward-declared type to update
        CType* union_type;
        if(auto existing = name.lexeme in union_types){
            // Update existing type in-place so typedefs continue to work
            union_type = *existing;
            union_type.fields = fields[];
            union_type.struct_size = max_size;
        } else {
            // Create new union type
            union_type = make_union_type(allocator, name.lexeme, fields[], max_size);
            union_types[name.lexeme] = union_type;
        }

        udef.name = name;
        udef.union_type = union_type;
        return 0;
    }

    // Result type for constant expression parsing
    struct ConstExprResult {
        long value;
        bool err;
    }

    // Parse a constant expression for enum values
    // Supports: integer literals, enum constant references, unary minus, +/-, comparisons, ternary
    ConstExprResult parse_enum_const_expr(){
        return parse_enum_const_ternary();
    }

    // Ternary operator - lowest precedence
    ConstExprResult parse_enum_const_ternary(){
        auto result = parse_enum_const_or();
        if(result.err) return result;

        if(check(CTokenType.QUESTION)){
            advance();  // consume ?
            auto if_true = parse_enum_const_expr();  // Recursive for nested ternary
            if(if_true.err) return if_true;

            consume(CTokenType.COLON, "Expected ':' in ternary expression");
            if(ERROR_OCCURRED){
                result.err = true;
                return result;
            }

            auto if_false = parse_enum_const_ternary();
            if(if_false.err) return if_false;

            result.value = result.value != 0 ? if_true.value : if_false.value;
        }
        return result;
    }

    // Bitwise OR - lowest precedence of bitwise ops
    ConstExprResult parse_enum_const_or(){
        auto result = parse_enum_const_xor();
        if(result.err) return result;

        while(check(CTokenType.PIPE)){
            advance();
            auto right = parse_enum_const_xor();
            if(right.err) return right;
            result.value = result.value | right.value;
        }
        return result;
    }

    // Bitwise XOR
    ConstExprResult parse_enum_const_xor(){
        auto result = parse_enum_const_and();
        if(result.err) return result;

        while(check(CTokenType.CARET)){
            advance();
            auto right = parse_enum_const_and();
            if(right.err) return right;
            result.value = result.value ^ right.value;
        }
        return result;
    }

    // Bitwise AND
    ConstExprResult parse_enum_const_and(){
        auto result = parse_enum_const_equality();
        if(result.err) return result;

        while(check(CTokenType.AMP)){
            advance();
            auto right = parse_enum_const_equality();
            if(right.err) return right;
            result.value = result.value & right.value;
        }
        return result;
    }

    ConstExprResult parse_enum_const_equality(){
        auto result = parse_enum_const_relational();
        if(result.err) return result;

        while(check(CTokenType.EQUAL_EQUAL) || check(CTokenType.BANG_EQUAL)){
            bool is_eq = check(CTokenType.EQUAL_EQUAL);
            advance();
            auto right = parse_enum_const_relational();
            if(right.err) return right;
            if(is_eq){
                result.value = result.value == right.value ? 1 : 0;
            } else {
                result.value = result.value != right.value ? 1 : 0;
            }
        }
        return result;
    }

    ConstExprResult parse_enum_const_relational(){
        auto result = parse_enum_const_shift();
        if(result.err) return result;

        while(check(CTokenType.LESS) || check(CTokenType.GREATER) ||
               check(CTokenType.LESS_EQUAL) || check(CTokenType.GREATER_EQUAL)){
            CTokenType op = peek().type;
            advance();
            auto right = parse_enum_const_shift();
            if(right.err) return right;
            if(op == CTokenType.LESS){
                result.value = result.value < right.value ? 1 : 0;
            } else if(op == CTokenType.GREATER){
                result.value = result.value > right.value ? 1 : 0;
            } else if(op == CTokenType.LESS_EQUAL){
                result.value = result.value <= right.value ? 1 : 0;
            } else {
                result.value = result.value >= right.value ? 1 : 0;
            }
        }
        return result;
    }

    // Shift operators
    ConstExprResult parse_enum_const_shift(){
        auto result = parse_enum_const_additive();
        if(result.err) return result;

        while(check(CTokenType.LESS_LESS) || check(CTokenType.GREATER_GREATER)){
            bool is_left = check(CTokenType.LESS_LESS);
            advance();
            auto right = parse_enum_const_additive();
            if(right.err) return right;
            if(is_left){
                result.value = result.value << right.value;
            } else {
                result.value = result.value >> right.value;
            }
        }
        return result;
    }

    ConstExprResult parse_enum_const_additive(){
        auto result = parse_enum_const_multiplicative();
        if(result.err) return result;

        while(check(CTokenType.PLUS) || check(CTokenType.MINUS)){
            bool is_plus = check(CTokenType.PLUS);
            advance();
            auto right = parse_enum_const_multiplicative();
            if(right.err) return right;
            if(is_plus){
                result.value = result.value + right.value;
            } else {
                result.value = result.value - right.value;
            }
        }
        return result;
    }

    ConstExprResult parse_enum_const_multiplicative(){
        auto result = parse_enum_const_unary();
        if(result.err) return result;

        while(check(CTokenType.STAR) || check(CTokenType.SLASH) || check(CTokenType.PERCENT)){
            CTokenType op = peek().type;
            advance();
            auto right = parse_enum_const_unary();
            if(right.err) return right;
            if(op == CTokenType.STAR){
                result.value = result.value * right.value;
            } else if(op == CTokenType.SLASH){
                if(right.value != 0){
                    result.value = result.value / right.value;
                } else {
                    result.value = 0;  // Avoid division by zero
                }
            } else {
                if(right.value != 0){
                    result.value = result.value % right.value;
                } else {
                    result.value = 0;
                }
            }
        }
        return result;
    }

    ConstExprResult parse_enum_const_unary(){
        if(match(CTokenType.MINUS)){
            auto result = parse_enum_const_primary();
            if(result.err) return result;
            result.value = -result.value;
            return result;
        }
        if(match(CTokenType.PLUS)){
            return parse_enum_const_primary();
        }
        return parse_enum_const_primary();
    }

    ConstExprResult parse_enum_const_primary(){
        ConstExprResult result;
        result.err = false;

        if(match(CTokenType.NUMBER)){
            CToken tok = previous();
            long value = 0;
            foreach(c; tok.lexeme){
                value = value * 10 + (c - '0');
            }
            result.value = value;
            return result;
        }

        if(match(CTokenType.HEX)){
            CToken tok = previous();
            long value = 0;
            str lexeme = tok.lexeme;
            // Skip "0x" or "0X" prefix
            foreach(c; lexeme[2 .. $]){
                if(c >= '0' && c <= '9'){
                    value = value * 16 + (c - '0');
                } else if(c >= 'a' && c <= 'f'){
                    value = value * 16 + (c - 'a' + 10);
                } else if(c >= 'A' && c <= 'F'){
                    value = value * 16 + (c - 'A' + 10);
                }
            }
            result.value = value;
            return result;
        }

        if(match(CTokenType.CHAR_LITERAL)){
            CToken tok = previous();
            str lexeme = tok.lexeme;
            // Lexeme is 'c' or '\x' or '\nnn' (with quotes)
            if(lexeme.length >= 3 && lexeme[1] == '\\'){
                // Escape sequence
                char escape_char = lexeme[2];
                switch(escape_char){
                    case 'n': result.value = '\n'; break;
                    case 'r': result.value = '\r'; break;
                    case 't': result.value = '\t'; break;
                    case 'b': result.value = '\b'; break;
                    case '\\': result.value = '\\'; break;
                    case '\'': result.value = '\''; break;
                    case '"': result.value = '"'; break;
                    case '0':
                        // Could be \0 or \0nn (octal)
                        if(lexeme.length == 4){
                            result.value = 0;
                        } else {
                            // Octal escape
                            long val = 0;
                            foreach(c; lexeme[2 .. $ - 1]){
                                if(c >= '0' && c <= '7'){
                                    val = val * 8 + (c - '0');
                                }
                            }
                            result.value = val;
                        }
                        break;
                    default:
                        // May be octal \nnn
                        if(escape_char >= '0' && escape_char <= '7'){
                            long val = 0;
                            foreach(c; lexeme[2 .. $ - 1]){
                                if(c >= '0' && c <= '7'){
                                    val = val * 8 + (c - '0');
                                }
                            }
                            result.value = val;
                        } else {
                            result.value = escape_char;
                        }
                }
            } else if(lexeme.length >= 3){
                // Simple character 'c'
                result.value = lexeme[1];
            }
            return result;
        }

        if(check(CTokenType.IDENTIFIER) && peek().lexeme == "__builtin_offsetof"){
            advance();  // consume __builtin_offsetof
            consume(CTokenType.LEFT_PAREN, "Expected '(' after __builtin_offsetof");
            if(ERROR_OCCURRED){ result.err = true; return result; }
            // Parse type (can be struct X, union Y, or typedef name)
            CType* type_ = parse_type();
            if(type_ is null){ result.err = true; return result; }
            consume(CTokenType.COMMA, "Expected ',' after type in __builtin_offsetof");
            if(ERROR_OCCURRED){ result.err = true; return result; }
            // Parse member name (can be nested: member1.member2)
            CToken member = consume(CTokenType.IDENTIFIER, "Expected member name");
            if(ERROR_OCCURRED){ result.err = true; return result; }
            // Look up the field offset in the type
            size_t offset = 0;
            CType* current_type = type_;
            // Handle nested member access (e.g., __value.__wch)
            while(true){
                if(current_type.kind != CTypeKind.STRUCT && current_type.kind != CTypeKind.UNION){
                    error(member, "offsetof requires struct or union type");
                    result.err = true; return result;
                }
                bool found = false;
                foreach(ref f; current_type.fields){
                    if(f.name == member.lexeme){
                        offset += f.offset;
                        current_type = f.type;
                        found = true;
                        break;
                    }
                }
                if(!found){
                    error(member, "Unknown field in offsetof");
                    result.err = true; return result;
                }
                if(match(CTokenType.DOT)){
                    member = consume(CTokenType.IDENTIFIER, "Expected member name after '.'");
                    if(ERROR_OCCURRED){ result.err = true; return result; }
                } else {
                    break;
                }
            }
            consume(CTokenType.RIGHT_PAREN, "Expected ')' after __builtin_offsetof");
            if(ERROR_OCCURRED){ result.err = true; return result; }
            result.value = cast(long)offset;
            return result;
        }

        if(match(CTokenType.IDENTIFIER)){
            CToken tok = previous();
            // Look up in already-defined enum constants
            if(long* val = tok.lexeme in enum_constants){
                result.value = *val;
                return result;
            }
            // Check for function-like macro call (unknown macro with parens)
            if(check(CTokenType.LEFT_PAREN)){
                // Skip the function-like macro call, treat as 0
                skip_balanced_parens();
                result.value = 0;
                return result;
            }
            error(tok, "Unknown enum constant in expression");
            result.err = true;
            return result;
        }

        if(match(CTokenType.LEFT_PAREN)){
            // Check if this is a cast expression: (type)value
            if(is_type_specifier(peek())){
                // It's a cast - parse and ignore the type, then parse the value
                CType* cast_type = parse_type();
                if(cast_type is null){
                    result.err = true;
                    return result;
                }
                consume(CTokenType.RIGHT_PAREN, "Expected ')' after cast type");
                if(ERROR_OCCURRED){
                    result.err = true;
                    return result;
                }
                // Parse the value being cast
                return parse_enum_const_unary();
            }
            // Regular parenthesized expression
            result = parse_enum_const_expr();
            if(result.err) return result;
            consume(CTokenType.RIGHT_PAREN, "Expected ')' after expression");
            if(ERROR_OCCURRED){
                result.err = true;
            }
            return result;
        }

        // sizeof(type) or sizeof(expr) in constant expression
        if(match(CTokenType.SIZEOF)){
            consume(CTokenType.LEFT_PAREN, "Expected '(' after sizeof");
            if(ERROR_OCCURRED){
                result.err = true;
                return result;
            }

            // Check if it's sizeof(type) or sizeof(expr)
            if(is_type_specifier(peek())){
                // sizeof(type) - use parse_type_name for array types
                CType* type = parse_type_name();
                if(type is null){
                    result.err = true;
                    return result;
                }

                consume(CTokenType.RIGHT_PAREN, "Expected ')' after type");
                if(ERROR_OCCURRED){
                    result.err = true;
                    return result;
                }

                result.value = cast(long) type.size_of();
                return result;
            } else {
                // sizeof(expr) - parse expression and infer its type
                CType* expr_type = parse_sizeof_expr_type();
                if(expr_type is null){
                    result.err = true;
                    return result;
                }

                consume(CTokenType.RIGHT_PAREN, "Expected ')' after sizeof expression");
                if(ERROR_OCCURRED){
                    result.err = true;
                    return result;
                }

                result.value = cast(long) expr_type.size_of();
                return result;
            }
        }

        if(match(CTokenType.ALIGNOF)){
            consume(CTokenType.LEFT_PAREN, "Expected '(' after _Alignof");
            if(ERROR_OCCURRED){
                result.err = true;
                return result;
            }

            // Check if it's _Alignof(type) or _Alignof(expr) (GNU extension)
            if(is_type_specifier(peek())){
                // _Alignof(type)
                CType* type = parse_type();
                if(type is null){
                    result.err = true;
                    return result;
                }

                consume(CTokenType.RIGHT_PAREN, "Expected ')' after type");
                if(ERROR_OCCURRED){
                    result.err = true;
                    return result;
                }

                result.value = cast(long) type.align_of();
                return result;
            } else {
                // _Alignof(expr) - GNU extension, parse expression and infer its type
                CType* expr_type = parse_sizeof_expr_type();
                if(expr_type is null){
                    result.err = true;
                    return result;
                }

                consume(CTokenType.RIGHT_PAREN, "Expected ')' after _Alignof expression");
                if(ERROR_OCCURRED){
                    result.err = true;
                    return result;
                }

                result.value = cast(long) expr_type.align_of();
                return result;
            }
        }

        error("Expected constant value in enum expression");
        result.err = true;
        return result;
    }

    // Parse expression inside sizeof() and return its type
    // Handles patterns like: ((Type*)0)->member, arr[0], etc.
    CType* parse_sizeof_expr_type(){
        return parse_sizeof_cast_or_primary();
    }

    // Parse a cast expression or primary expression for sizeof
    CType* parse_sizeof_cast_or_primary(){
        CType* current_type = null;

        // Handle leading parentheses
        if(match(CTokenType.LEFT_PAREN)){
            // Check if this is a cast: (type)
            if(is_type_specifier(peek())){
                CType* cast_type = parse_type();
                if(cast_type is null) return null;

                consume(CTokenType.RIGHT_PAREN, "Expected ')' after cast type");
                if(ERROR_OCCURRED) return null;

                // After a cast, we have the value being cast - skip it
                // and the result type is the cast type
                skip_sizeof_value();
                current_type = cast_type;
            } else {
                // Parenthesized expression - recurse
                CType* inner = parse_sizeof_cast_or_primary();
                if(inner is null) return null;

                // Handle postfix operations inside the parens
                inner = parse_sizeof_postfix(inner);
                if(inner is null) return null;

                consume(CTokenType.RIGHT_PAREN, "Expected ')'");
                if(ERROR_OCCURRED) return null;

                current_type = inner;
            }
        } else {
            // Primary expression (identifier, number, etc.)
            if(check(CTokenType.IDENTIFIER)){
                CToken id = advance();
                // Look up variable type in global_var_types
                if(CType** vtype = id.lexeme in global_var_types){
                    current_type = *vtype;
                } else {
                    error(id, "Unknown variable in sizeof expression");
                    return null;
                }
            } else if(check(CTokenType.NUMBER) || check(CTokenType.HEX)){
                advance();
                current_type = &TYPE_INT;
            } else {
                error("Expected expression in sizeof");
                return null;
            }
        }

        // Handle postfix operations
        return parse_sizeof_postfix(current_type);
    }

    // Skip a value expression (for the value being cast)
    void skip_sizeof_value(){
        // Skip balanced parens or simple tokens until we hit a postfix operator or end
        if(check(CTokenType.LEFT_PAREN)){
            skip_balanced_parens();
        } else {
            // Skip simple token (number, identifier, etc.)
            if(check(CTokenType.IDENTIFIER) || check(CTokenType.NUMBER) || check(CTokenType.HEX)){
                advance();
            }
        }
    }

    // Parse postfix operations for sizeof expression type
    CType* parse_sizeof_postfix(CType* current_type){
        if(current_type is null) return null;

        // Handle postfix operations: ->member, .member, [index]
        while(true){
            if(match(CTokenType.ARROW)){
                // Dereference pointer and access member
                if(current_type.kind != CTypeKind.POINTER){
                    error("Arrow operator requires pointer type");
                    return null;
                }
                CType* pointed = current_type.pointed_to;
                if(pointed is null || (pointed.kind != CTypeKind.STRUCT && pointed.kind != CTypeKind.UNION)){
                    error("Arrow operator requires pointer to struct/union");
                    return null;
                }

                CToken member = consume(CTokenType.IDENTIFIER, "Expected member name after ->");
                if(ERROR_OCCURRED) return null;

                // Look up member in struct/union
                CType* member_type = null;
                foreach(ref field; pointed.fields){
                    if(field.name == member.lexeme){
                        member_type = field.type;
                        break;
                    }
                }
                if(member_type is null){
                    error(member, "Unknown struct/union member");
                    return null;
                }
                current_type = member_type;
            } else if(match(CTokenType.DOT)){
                // Direct member access
                if(current_type.kind != CTypeKind.STRUCT && current_type.kind != CTypeKind.UNION){
                    error("Dot operator requires struct/union type");
                    return null;
                }

                CToken member = consume(CTokenType.IDENTIFIER, "Expected member name after .");
                if(ERROR_OCCURRED) return null;

                // Look up member in struct/union
                CType* member_type = null;
                foreach(ref field; current_type.fields){
                    if(field.name == member.lexeme){
                        member_type = field.type;
                        break;
                    }
                }
                if(member_type is null){
                    error(member, "Unknown struct/union member");
                    return null;
                }
                current_type = member_type;
            } else if(match(CTokenType.LEFT_BRACKET)){
                // Array subscript
                if(current_type.kind == CTypeKind.POINTER){
                    current_type = current_type.pointed_to;
                } else if(current_type.kind == CTypeKind.ARRAY){
                    current_type = current_type.pointed_to;
                } else {
                    error("Subscript requires array or pointer type");
                    return null;
                }

                // Skip the index expression
                int depth = 1;
                while(depth > 0 && !at_end){
                    if(check(CTokenType.LEFT_BRACKET)) depth++;
                    else if(check(CTokenType.RIGHT_BRACKET)) depth--;
                    if(depth > 0) advance();
                }
                consume(CTokenType.RIGHT_BRACKET, "Expected ']'");
                if(ERROR_OCCURRED) return null;
            } else {
                break;
            }
        }

        return current_type;
    }

    // (6.7.3.3) enum-specifier:
    //     enum identifier_opt { enumerator-list }
    //     enum identifier_opt { enumerator-list , }
    //     enum identifier
    int parse_enum_def(CEnumDef* edef){
        advance();  // consume 'enum'

        // Name is optional (anonymous enum)
        CToken name;
        bool has_name = false;
        if(check(CTokenType.IDENTIFIER)){
            name = advance();
            has_name = true;
        }

        consume(CTokenType.LEFT_BRACE, "Expected '{' after enum");
        if(ERROR_OCCURRED) return 1;

        // Parse enum constants
        auto constants = make_barray!EnumConstant(allocator);
        long next_value = 0;

        while(!check(CTokenType.RIGHT_BRACE) && !at_end){
            // Parse constant name
            CToken const_name = consume(CTokenType.IDENTIFIER, "Expected enum constant name");
            if(ERROR_OCCURRED) return 1;

            // Check for explicit value assignment
            long value = next_value;
            if(match(CTokenType.EQUAL)){
                // Parse constant expression (supports literals, enum constants, +/-)
                auto result = parse_enum_const_expr();
                if(result.err) return 1;
                value = result.value;
            }

            // Add constant
            EnumConstant ec;
            ec.name = const_name.lexeme;
            ec.value = value;
            constants ~= ec;

            // Register in global constants table
            enum_constants[const_name.lexeme] = value;

            // Next implicit value is one more
            next_value = value + 1;

            // Optional comma between constants
            if(!check(CTokenType.RIGHT_BRACE)){
                if(!match(CTokenType.COMMA)){
                    // Allow trailing comma or no comma before }
                    if(!check(CTokenType.RIGHT_BRACE)){
                        error("Expected ',' or '}' after enum constant");
                        return 1;
                    }
                }
            }
        }

        consume(CTokenType.RIGHT_BRACE, "Expected '}' after enum constants");
        if(ERROR_OCCURRED) return 1;

        consume(CTokenType.SEMICOLON, "Expected ';' after enum definition");
        if(ERROR_OCCURRED) return 1;

        // Create the enum type
        CType* enum_type = make_enum_type(allocator, has_name ? name.lexeme : "");

        // Register the enum type if named
        if(has_name){
            enum_types[name.lexeme] = enum_type;
        }

        edef.name = has_name ? name : CToken.init;
        edef.enum_type = enum_type;
        edef.constants = constants[];
        return 0;
    }

    // Parse _Static_assert(constant_expr, "message");
    int parse_static_assert(){
        consume(CTokenType.LEFT_PAREN, "Expected '(' after _Static_assert");
        if(ERROR_OCCURRED) return 1;

        // Parse constant expression using the enum constant expression parser
        auto result = parse_enum_const_expr();
        if(result.err) return 1;

        consume(CTokenType.COMMA, "Expected ',' after expression");
        if(ERROR_OCCURRED) return 1;

        CToken message = consume(CTokenType.STRING, "Expected string message");
        if(ERROR_OCCURRED) return 1;

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after message");
        if(ERROR_OCCURRED) return 1;

        consume(CTokenType.SEMICOLON, "Expected ';' after _Static_assert");
        if(ERROR_OCCURRED) return 1;

        // Check the assertion
        if(result.value == 0){
            // Strip quotes from message for error output
            str msg = message.lexeme;
            if(msg.length >= 2 && msg[0] == '"' && msg[$ - 1] == '"'){
                msg = msg[1 .. $ - 1];
            }
            error(message, msg);
            return 1;
        }

        return 0;
    }

    // (6.7.9) typedef-name:
    //     typedef declaration-specifiers init-declarator-list_opt ;
    // Supports: typedef <type> <name>;
    //           typedef struct { ... } Name;
    //           typedef struct Name { ... } Alias;
    //           typedef enum { ... } Name;
    int parse_typedef(Barray!(CEnumDef)* enums_out){
        advance();  // consume 'typedef'

        // Check for struct/union/enum definition within typedef
        if(check(CTokenType.STRUCT)){
            advance();  // consume 'struct'

            // Check if there's a name and/or brace
            CToken struct_name;
            bool has_name = false;
            bool has_body = false;

            if(check(CTokenType.IDENTIFIER)){
                struct_name = advance();
                has_name = true;
            }
            if(check(CTokenType.LEFT_BRACE)){
                has_body = true;
            }

            CType* struct_type;

            if(has_body){
                // Parse struct body using unified member parsing
                consume(CTokenType.LEFT_BRACE, "Expected '{'");
                if(ERROR_OCCURRED) return 1;

                auto fields = make_barray!StructField(allocator);
                size_t total_size = 0;
                int err = parse_member_declaration_list(&fields, &total_size, false);  // is_union=false
                if(err) return err;

                consume(CTokenType.RIGHT_BRACE, "Expected '}'");
                if(ERROR_OCCURRED) return 1;

                // Check if there's an existing forward-declared type to update
                if(has_name){
                    if(auto existing = struct_name.lexeme in struct_types){
                        // Update existing type in-place so typedefs continue to work
                        struct_type = *existing;
                        struct_type.fields = fields[];
                        struct_type.struct_size = total_size;
                    } else {
                        struct_type = make_struct_type(allocator, struct_name.lexeme, fields[], total_size);
                        struct_types[struct_name.lexeme] = struct_type;
                    }
                } else {
                    struct_type = make_struct_type(allocator, "", fields[], total_size);
                }
            } else {
                // Just referencing existing struct
                if(!has_name){
                    error("Expected struct name or body");
                    return 1;
                }
                if(CType** found = struct_name.lexeme in struct_types){
                    struct_type = *found;
                } else {
                    // Create incomplete struct type for forward reference
                    auto data = allocator.alloc(CType.sizeof);
                    CType* incomplete = cast(CType*)data.ptr;
                    incomplete.kind = CTypeKind.STRUCT;
                    incomplete.struct_name = struct_name.lexeme;
                    incomplete.struct_size = 0;  // Unknown size
                    struct_types[struct_name.lexeme] = incomplete;
                    struct_type = incomplete;
                }
            }

            // Handle pointer types (e.g., typedef struct X *ptr_t)
            while(match(CTokenType.STAR)){
                struct_type = make_pointer_type(allocator, struct_type);
                // Skip pointer qualifiers
                while(match(CTokenType.CONST) || match(CTokenType.VOLATILE) || match(CTokenType.RESTRICT)){}
            }

            // Check for function pointer typedef: typedef struct X *(*name)(...)
            if(check(CTokenType.LEFT_PAREN)){
                advance();  // consume '('

                // Skip any calling convention identifiers until we hit '*'
                while(check(CTokenType.IDENTIFIER)){
                    advance();
                }

                if(!match(CTokenType.STAR)){
                    error("Expected '*' in function pointer typedef");
                    return 1;
                }

                CToken typedef_name = consume(CTokenType.IDENTIFIER, "Expected typedef name");
                if(ERROR_OCCURRED) return 1;

                consume(CTokenType.RIGHT_PAREN, "Expected ')' after function pointer name");
                if(ERROR_OCCURRED) return 1;

                // Now parse the parameter list using unified parameter parsing
                consume(CTokenType.LEFT_PAREN, "Expected '(' for function parameters");
                if(ERROR_OCCURRED) return 1;

                auto param_result = parse_parameter_type_list();
                if(param_result.err) return 1;

                // Extract just the types from params
                auto param_types = make_barray!(CType*)(allocator);
                foreach(ref p; param_result.params[]){
                    param_types ~= p.type;
                }

                consume(CTokenType.RIGHT_PAREN, "Expected ')' after parameters");
                if(ERROR_OCCURRED) return 1;

                consume(CTokenType.SEMICOLON, "Expected ';' after typedef");
                if(ERROR_OCCURRED) return 1;

                CType* func_type = make_function_type(allocator, struct_type, param_types[], param_result.is_varargs);
                CType* func_ptr_type = make_pointer_type(allocator, func_type);

                typedef_types[typedef_name.lexeme] = func_ptr_type;
                return 0;
            }

            // Skip attribute macros (like SDL_AUDIOCVT_PACKED, __attribute__((packed))) before typedef name
            while(check(CTokenType.IDENTIFIER)){
                auto cur_lexeme = peek().lexeme;
                auto next = peek_at(1).type;

                // Only skip __attribute__(...) or known attribute macros
                if(cur_lexeme == "__attribute__" && next == CTokenType.LEFT_PAREN){
                    advance();
                    skip_balanced_parens();
                } else if(next == CTokenType.IDENTIFIER){
                    // Check if next token is __attribute__ - if so, current is the typedef name
                    auto next_lexeme = peek_at(1).lexeme;
                    if(next_lexeme == "__attribute__"){
                        break;  // Current is typedef name, stop skipping
                    }
                    // Skip simple attribute identifier (like SDL_AUDIOCVT_PACKED)
                    advance();
                } else if(next == CTokenType.LEFT_PAREN){
                    // Skip function-like macro attribute
                    advance();
                    skip_balanced_parens();
                } else {
                    break;  // Next token is the typedef name followed by ;
                }
            }

            // Now get the typedef name
            CToken typedef_name = consume(CTokenType.IDENTIFIER, "Expected typedef name");
            if(ERROR_OCCURRED) return 1;

            // Check for array dimensions (e.g., typedef struct X name[1])
            if(check(CTokenType.LEFT_BRACKET)){
                advance();  // consume '['
                // Parse array size expression (simplified: expect a number)
                if(check(CTokenType.NUMBER)){
                    CToken size_tok = advance();
                    size_t array_size = 0;
                    // Parse the number
                    foreach(c; size_tok.lexeme){
                        if(c >= '0' && c <= '9'){
                            array_size = array_size * 10 + (c - '0');
                        } else {
                            break;
                        }
                    }
                    consume(CTokenType.RIGHT_BRACKET, "Expected ']'");
                    if(ERROR_OCCURRED) return 1;

                    // Create array type
                    struct_type = make_array_type(allocator, struct_type, array_size);
                } else {
                    // Skip to ]
                    while(!check(CTokenType.RIGHT_BRACKET) && !at_end){
                        advance();
                    }
                    consume(CTokenType.RIGHT_BRACKET, "Expected ']'");
                    if(ERROR_OCCURRED) return 1;
                    struct_type = make_array_type(allocator, struct_type, 1);
                }
            }

            // Skip __attribute__((xxx)) after typedef name
            if(check(CTokenType.IDENTIFIER) && peek().lexeme == "__attribute__"){
                advance();
                skip_balanced_parens();
            }

            consume(CTokenType.SEMICOLON, "Expected ';' after typedef");
            if(ERROR_OCCURRED) return 1;

            typedef_types[typedef_name.lexeme] = struct_type;
            return 0;

        } else if(check(CTokenType.UNION)){
            advance();  // consume 'union'

            CToken union_name;
            bool has_name = false;
            bool has_body = false;

            if(check(CTokenType.IDENTIFIER)){
                union_name = advance();
                has_name = true;
            }
            if(check(CTokenType.LEFT_BRACE)){
                has_body = true;
            }

            CType* union_type;

            if(has_body){
                // Parse union body using unified member parsing
                consume(CTokenType.LEFT_BRACE, "Expected '{'");
                if(ERROR_OCCURRED) return 1;

                auto fields = make_barray!StructField(allocator);
                size_t max_size = 0;
                int err = parse_member_declaration_list(&fields, &max_size, true);  // is_union=true
                if(err) return err;

                consume(CTokenType.RIGHT_BRACE, "Expected '}'");
                if(ERROR_OCCURRED) return 1;

                // Check if there's an existing forward-declared type to update
                if(has_name){
                    if(auto existing = union_name.lexeme in union_types){
                        // Update existing type in-place so typedefs continue to work
                        union_type = *existing;
                        union_type.fields = fields[];
                        union_type.struct_size = max_size;
                    } else {
                        union_type = make_union_type(allocator, union_name.lexeme, fields[], max_size);
                        union_types[union_name.lexeme] = union_type;
                    }
                } else {
                    union_type = make_union_type(allocator, "", fields[], max_size);
                }
            } else {
                if(!has_name){
                    error("Expected union name or body");
                    return 1;
                }
                if(CType** found = union_name.lexeme in union_types){
                    union_type = *found;
                } else {
                    // Create incomplete union type for forward reference
                    auto data = allocator.alloc(CType.sizeof);
                    CType* incomplete = cast(CType*)data.ptr;
                    incomplete.kind = CTypeKind.UNION;
                    incomplete.struct_name = union_name.lexeme;
                    incomplete.struct_size = 0;
                    union_types[union_name.lexeme] = incomplete;
                    union_type = incomplete;
                }
            }

            // Handle pointer types (e.g., typedef union X *ptr_t)
            while(match(CTokenType.STAR)){
                union_type = make_pointer_type(allocator, union_type);
                // Skip pointer qualifiers
                while(match(CTokenType.CONST) || match(CTokenType.VOLATILE) || match(CTokenType.RESTRICT)){}
            }

            CToken typedef_name = consume(CTokenType.IDENTIFIER, "Expected typedef name");
            if(ERROR_OCCURRED) return 1;

            // Skip __attribute__((xxx)) after typedef name
            if(check(CTokenType.IDENTIFIER) && peek().lexeme == "__attribute__"){
                advance();
                skip_balanced_parens();
            }

            consume(CTokenType.SEMICOLON, "Expected ';' after typedef");
            if(ERROR_OCCURRED) return 1;

            typedef_types[typedef_name.lexeme] = union_type;
            return 0;

        } else if(check(CTokenType.ENUM)){
            advance();  // consume 'enum'

            CToken enum_name;
            bool has_name = false;
            bool has_body = false;

            if(check(CTokenType.IDENTIFIER)){
                enum_name = advance();
                has_name = true;
            }
            if(check(CTokenType.LEFT_BRACE)){
                has_body = true;
            }

            CType* enum_type;

            if(has_body){
                consume(CTokenType.LEFT_BRACE, "Expected '{'");
                if(ERROR_OCCURRED) return 1;

                // Parse enum constants
                auto constants = make_barray!EnumConstant(allocator);
                long next_value = 0;

                while(!check(CTokenType.RIGHT_BRACE) && !at_end){
                    CToken const_name = consume(CTokenType.IDENTIFIER, "Expected enum constant name");
                    if(ERROR_OCCURRED) return 1;

                    long value = next_value;
                    if(match(CTokenType.EQUAL)){
                        auto result = parse_enum_const_expr();
                        if(result.err) return 1;
                        value = result.value;
                    }

                    EnumConstant ec;
                    ec.name = const_name.lexeme;
                    ec.value = value;
                    constants ~= ec;
                    enum_constants[const_name.lexeme] = value;
                    next_value = value + 1;

                    if(!check(CTokenType.RIGHT_BRACE)){
                        if(!match(CTokenType.COMMA)){
                            if(!check(CTokenType.RIGHT_BRACE)){
                                error("Expected ',' or '}' after enum constant");
                                return 1;
                            }
                        }
                    }
                }

                consume(CTokenType.RIGHT_BRACE, "Expected '}'");
                if(ERROR_OCCURRED) return 1;

                enum_type = make_enum_type(allocator, has_name ? enum_name.lexeme : "");

                if(has_name){
                    enum_types[enum_name.lexeme] = enum_type;
                }

                // Add enum definition to translation unit so code generator sees constants
                CEnumDef edef;
                edef.name = has_name ? enum_name : CToken.init;
                edef.enum_type = enum_type;
                edef.constants = constants[];
                *enums_out ~= edef;
            } else {
                if(!has_name){
                    error("Expected enum name or body");
                    return 1;
                }
                if(CType** found = enum_name.lexeme in enum_types){
                    enum_type = *found;
                } else {
                    error("Unknown enum type");
                    return 1;
                }
            }

            CToken typedef_name = consume(CTokenType.IDENTIFIER, "Expected typedef name");
            if(ERROR_OCCURRED) return 1;

            consume(CTokenType.SEMICOLON, "Expected ';' after typedef");
            if(ERROR_OCCURRED) return 1;

            typedef_types[typedef_name.lexeme] = enum_type;
            return 0;

        } else {
            // Could be simple typedef or function pointer typedef
            // Simple: typedef <type> <name>;
            // Function pointer: typedef <ret_type> (* <name>)(<params>);
            // Function pointer with calling conv: typedef <ret_type> (SDLCALL * <name>)(<params>);

            CType* base_type = parse_type();
            if(base_type is null) return 1;

            // Check for function pointer syntax: starts with '('
            if(check(CTokenType.LEFT_PAREN)){
                advance();  // consume '('

                // Skip any calling convention identifiers until we hit '*'
                while(check(CTokenType.IDENTIFIER)){
                    advance();  // skip calling convention like SDLCALL
                }

                if(!match(CTokenType.STAR)){
                    error("Expected '*' in function pointer typedef");
                    return 1;
                }

                CToken typedef_name = consume(CTokenType.IDENTIFIER, "Expected typedef name");
                if(ERROR_OCCURRED) return 1;

                consume(CTokenType.RIGHT_PAREN, "Expected ')' after function pointer name");
                if(ERROR_OCCURRED) return 1;

                // Now parse the parameter list using unified parameter parsing
                consume(CTokenType.LEFT_PAREN, "Expected '(' for function parameters");
                if(ERROR_OCCURRED) return 1;

                auto param_result = parse_parameter_type_list();
                if(param_result.err) return 1;

                // Extract just the types from params
                auto param_types = make_barray!(CType*)(allocator);
                foreach(ref p; param_result.params[]){
                    param_types ~= p.type;
                }

                consume(CTokenType.RIGHT_PAREN, "Expected ')' after parameters");
                if(ERROR_OCCURRED) return 1;

                consume(CTokenType.SEMICOLON, "Expected ';' after typedef");
                if(ERROR_OCCURRED) return 1;

                // Create function type then wrap in pointer
                CType* func_type = make_function_type(allocator, base_type, param_types[], param_result.is_varargs);
                CType* func_ptr_type = make_pointer_type(allocator, func_type);

                typedef_types[typedef_name.lexeme] = func_ptr_type;
                return 0;
            }

            CToken typedef_name = consume(CTokenType.IDENTIFIER, "Expected typedef name");
            if(ERROR_OCCURRED) return 1;

            // Handle array typedefs: typedef int array_t[10];
            if(match(CTokenType.LEFT_BRACKET)){
                auto size_result = parse_enum_const_expr();
                if(size_result.err) return 1;
                if(size_result.value <= 0){
                    error("Array size must be positive");
                    return 1;
                }
                consume(CTokenType.RIGHT_BRACKET, "Expected ']'");
                if(ERROR_OCCURRED) return 1;
                base_type = make_array_type(allocator, base_type, cast(size_t) size_result.value);
            }

            // Skip __attribute__((xxx)) after typedef name
            if(check(CTokenType.IDENTIFIER) && peek().lexeme == "__attribute__"){
                advance();
                skip_balanced_parens();
            }

            consume(CTokenType.SEMICOLON, "Expected ';' after typedef");
            if(ERROR_OCCURRED) return 1;

            typedef_types[typedef_name.lexeme] = base_type;
            return 0;
        }
    }

    int parse_function(CFunction* func){
        // Parse return type
        CType* ret_type = parse_type();
        if(ret_type is null) return 1;

        // Parse function name
        CToken name = consume(CTokenType.IDENTIFIER, "Expected function name");
        if(ERROR_OCCURRED) return 1;

        return parse_function_rest(ret_type, name, func);
    }

    // Parse function after type and name have been consumed
    int parse_function_rest(CType* ret_type, CToken name, CFunction* func, bool saw_inline = false){
        // Parse parameters using unified parameter parsing
        consume(CTokenType.LEFT_PAREN, "Expected '(' after function name");
        if(ERROR_OCCURRED) return 1;

        auto param_result = parse_parameter_type_list();
        if(param_result.err) return 1;

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after parameters");
        if(ERROR_OCCURRED) return 1;

        // Handle asm label for symbol renaming: __asm("symbol_name")
        if(match(CTokenType.ASM)){
            consume(CTokenType.LEFT_PAREN, "Expected '(' after asm");
            if(ERROR_OCCURRED) return 1;
            // Skip string literals (may be multiple concatenated: __asm("_" "name"))
            while(check(CTokenType.STRING)){
                advance();
            }
            consume(CTokenType.RIGHT_PAREN, "Expected ')' after asm label");
            if(ERROR_OCCURRED) return 1;
        }

        // Skip __attribute__((...)) specifiers
        while(check(CTokenType.IDENTIFIER) && peek().lexeme == "__attribute__"){
            advance();  // consume __attribute__
            // __attribute__ uses double parens: __attribute__((...))
            if(check(CTokenType.LEFT_PAREN)){
                skip_balanced_parens();
            }
        }

        func.name = name;
        func.return_type = ret_type;
        func.params = param_result.params[];
        func.is_varargs = param_result.is_varargs;
        func.is_inline = saw_inline;
        func.library = current_library;

        // Check if declaration or definition
        if(match(CTokenType.SEMICOLON)){
            func.is_definition = false;
            return 0;
        }

        // Parse function body
        func.is_definition = true;
        consume(CTokenType.LEFT_BRACE, "Expected '{' for function body");
        if(ERROR_OCCURRED) return 1;

        auto body = make_barray!(CStmt*)(allocator);
        while(!check(CTokenType.RIGHT_BRACE) && !at_end){
            CStmt* stmt = parse_statement();
            if(stmt is null) return 1;
            body ~= stmt;
        }

        consume(CTokenType.RIGHT_BRACE, "Expected '}' after function body");
        if(ERROR_OCCURRED) return 1;

        func.body = body[];
        return 0;
    }

    // Parse global variable after type and name have been consumed
    int parse_global_var_rest(CType* var_type, CToken name, CGlobalVar* gvar, bool is_extern, str library){
        gvar.name = name;
        gvar.var_type = var_type;
        gvar.initializer = null;
        gvar.is_extern = is_extern;
        gvar.library = library;

        // Handle array declarations: type name[size1][size2]...
        // In C, int arr[2][3] means array[2] of array[3] of int
        // We need to collect dimensions and apply in reverse order
        size_t[8] dims;  // Support up to 8 dimensions
        size_t num_dims = 0;
        while(check(CTokenType.LEFT_BRACKET)){
            advance();  // consume '['
            // Parse array size if present
            size_t dim = 0;
            if(!check(CTokenType.RIGHT_BRACKET)){
                auto size_result = parse_enum_const_expr();
                if(!size_result.err && size_result.value > 0){
                    dim = cast(size_t)size_result.value;
                }
            }
            consume(CTokenType.RIGHT_BRACKET, "Expected ']' after array size");
            if(ERROR_OCCURRED) return 1;
            if(num_dims < 8){
                dims[num_dims++] = dim;
            }
        }
        // Apply dimensions in reverse order (rightmost is innermost)
        for(size_t i = num_dims; i > 0; i--){
            gvar.var_type = make_array_type(allocator, gvar.var_type, dims[i - 1]);
        }

        // Skip __asm("symbol") renaming
        if(match(CTokenType.ASM)){
            consume(CTokenType.LEFT_PAREN, "Expected '(' after __asm");
            if(ERROR_OCCURRED) return 1;
            while(check(CTokenType.STRING)) advance();
            consume(CTokenType.RIGHT_PAREN, "Expected ')' after __asm label");
            if(ERROR_OCCURRED) return 1;
        }

        // Skip __attribute__((...))
        while(check(CTokenType.IDENTIFIER) && peek().lexeme == "__attribute__"){
            advance();
            if(check(CTokenType.LEFT_PAREN)){
                skip_balanced_parens();
            }
        }

        // Check for initializer
        if(match(CTokenType.EQUAL)){
            gvar.initializer = parse_initializer();
            if(gvar.initializer is null) return 1;
            // If there's an initializer, this is a definition, not an extern declaration
            gvar.is_extern = false;

            // Infer array size from initializer
            if(gvar.var_type.is_array() && gvar.var_type.array_size == 0){
                // String literal initializer for char[]
                if(gvar.var_type.pointed_to && gvar.var_type.pointed_to.kind == CTypeKind.CHAR){
                    if(CLiteral* lit = gvar.initializer.as_literal()){
                        if(lit.value.type == CTokenType.STRING){
                            // Count actual characters including escapes plus null terminator
                            str s = lit.value.lexeme;
                            if(s.length >= 2) s = s[1 .. $ - 1];  // Remove quotes
                            size_t char_count = 0;
                            for(size_t i = 0; i < s.length; ){
                                if(s[i] == '\\' && i + 1 < s.length){
                                    i += 2;  // Skip escape sequence
                                } else {
                                    i++;
                                }
                                char_count++;
                            }
                            char_count++;  // Add null terminator
                            gvar.var_type.array_size = char_count;
                        }
                    }
                }
                // Brace-enclosed initializer list
                if(CInitList* ilist = gvar.initializer.as_init_list()){
                    gvar.var_type.array_size = ilist.elements.length;
                }
            }
        }

        // Register variable type for sizeof lookups in constant expressions
        global_var_types[name.lexeme] = gvar.var_type;

        // Don't consume semicolon here - caller handles comma-separated declarations
        return 0;
    }

    // =========================================================================
    // Type Parsing
    // =========================================================================

    // (6.7.7.1) declarator: pointer_opt direct-declarator
    // (6.7.7.1) pointer: * type-qualifier-list_opt pointer_opt
    // This function parses the type portion (base type + pointer modifiers)
    CType* parse_type(){
        CType* base = parse_base_type();
        if(base is null) return null;

        // Handle postfix qualifiers (e.g., "int const *" instead of "const int *")
        while(match(CTokenType.CONST) || match(CTokenType.VOLATILE) || match(CTokenType.RESTRICT)){
            // Just skip - we don't track these
        }

        // Handle pointer types
        while(check(CTokenType.STAR)){
            advance();
            base = make_pointer_type(allocator, base);
            // Skip pointer qualifiers (const, volatile, restrict)
            while(match(CTokenType.CONST) || match(CTokenType.VOLATILE) || match(CTokenType.RESTRICT)){
                // Just skip - we don't track pointer constness
            }
        }

        return base;
    }

    // Parse a type-name for casts, sizeof, compound literals
    // This includes full abstract declarators (pointers, arrays, function pointers)
    CType* parse_type_name(){
        CType* base = parse_base_type();
        if(base is null) return null;

        // Parse abstract declarator (pointers, arrays, function pointers)
        // For abstract declarators, don't consume identifiers
        CDeclarator* decl = parse_abstract_declarator();
        if(decl !is null){
            base = apply_declarator_to_type(base, decl);
        }

        return base;
    }

    // Parse an abstract declarator (no identifier allowed)
    CDeclarator* parse_abstract_declarator(){
        auto data = allocator.alloc(CDeclarator.sizeof);
        CDeclarator* decl = cast(CDeclarator*)data.ptr;
        *decl = CDeclarator.init;

        // Handle postfix qualifiers (e.g., "char const *" instead of "const char *")
        while(match(CTokenType.CONST) || match(CTokenType.VOLATILE) ||
               match(CTokenType.RESTRICT) || match(CTokenType.ATOMIC)){}

        // Parse pointer prefix
        while(check(CTokenType.STAR)){
            advance();
            decl.pointer_depth++;
            while(match(CTokenType.CONST) || match(CTokenType.VOLATILE) ||
                   match(CTokenType.RESTRICT) || match(CTokenType.ATOMIC)){}
        }

        // Handle parenthesized abstract declarator: (*)[10] or (*)(int)
        if(check(CTokenType.LEFT_PAREN)){
            CToken next = peek_at(1);
            // Only parse as nested declarator if it starts with * or (
            if(next.type == CTokenType.STAR || next.type == CTokenType.LEFT_PAREN){
                advance();  // consume '('
                decl.nested = parse_abstract_declarator();
                if(decl.nested is null) return null;
                consume(CTokenType.RIGHT_PAREN, "Expected ')' after abstract declarator");
                if(ERROR_OCCURRED) return null;
            }
        }

        // Handle array suffix: [size]
        while(check(CTokenType.LEFT_BRACKET)){
            advance();  // consume '['
            decl.is_array = true;
            if(!check(CTokenType.RIGHT_BRACKET)){
                auto size_result = parse_enum_const_expr();
                if(!size_result.err && size_result.value > 0){
                    decl.array_dim = cast(size_t)size_result.value;
                }
            }
            consume(CTokenType.RIGHT_BRACKET, "Expected ']'");
            if(ERROR_OCCURRED) return null;
        }

        return decl;
    }

    // Get base type by unwrapping pointers (for comma-separated declarations)
    static CType* unwrap_pointers(CType* t){
        while(t !is null && t.kind == CTypeKind.POINTER){
            t = t.pointed_to;
        }
        return t;
    }

    // Parse pointer modifiers and build pointer type
    CType* parse_pointer_modifiers(CType* base){
        // Handle postfix qualifiers (e.g., "int const *" instead of "const int *")
        while(match(CTokenType.CONST) || match(CTokenType.VOLATILE) || match(CTokenType.RESTRICT)){}

        while(check(CTokenType.STAR)){
            advance();
            base = make_pointer_type(allocator, base);
            // Skip pointer qualifiers
            while(match(CTokenType.CONST) || match(CTokenType.VOLATILE) || match(CTokenType.RESTRICT)){}
        }
        return base;
    }

    // =========================================================================
    // Declarator Parsing (ISO/IEC 9899:202y Section 6.7.7)
    // =========================================================================

    // (6.7.7.1) declarator:
    //     pointer_opt direct-declarator
    CDeclarator* parse_declarator(bool allow_abstract = false){
        auto data = allocator.alloc(CDeclarator.sizeof);
        CDeclarator* decl = cast(CDeclarator*)data.ptr;
        *decl = CDeclarator.init;

        // (6.7.7.1) pointer:
        //     * attribute-specifier-sequence_opt type-qualifier-list_opt
        //     * attribute-specifier-sequence_opt type-qualifier-list_opt pointer
        while(check(CTokenType.STAR)){
            advance();
            decl.pointer_depth++;
            // (6.7.4.1) type-qualifier-list:
            //     type-qualifier | type-qualifier-list type-qualifier
            // (6.7.4.1) type-qualifier: const | restrict | volatile | _Atomic
            while(match(CTokenType.CONST) || match(CTokenType.VOLATILE) ||
                   match(CTokenType.RESTRICT) || match(CTokenType.ATOMIC)){
                // Skip qualifiers - we don't track per-pointer constness
            }
        }

        // Parse direct-declarator
        return parse_direct_declarator(decl, allow_abstract);
    }

    // (6.7.7.1) direct-declarator:
    //     identifier attribute-specifier-sequence_opt
    //     ( declarator )
    //     array-declarator attribute-specifier-sequence_opt
    //     function-declarator attribute-specifier-sequence_opt
    //
    // (6.7.7.1) array-declarator:
    //     direct-declarator [ type-qualifier-list_opt assignment-expression_opt ]
    //
    // (6.7.7.1) function-declarator:
    //     direct-declarator ( parameter-type-list_opt )
    CDeclarator* parse_direct_declarator(CDeclarator* decl, bool allow_abstract){
        // Handle parenthesized declarator: ( declarator )
        // This is for cases like: int (*ptr), void (*func)(int)
        if(check(CTokenType.LEFT_PAREN)){
            // Look ahead to distinguish ( declarator ) from ( parameters )
            // A declarator starts with * or identifier or (
            CToken next = peek_at(1);
            if(next.type == CTokenType.STAR ||
                (next.type == CTokenType.IDENTIFIER && !is_type_start(next)) ||
                next.type == CTokenType.LEFT_PAREN){
                advance();  // consume '('
                decl.nested = parse_declarator(allow_abstract);
                if(decl.nested is null) return null;
                consume(CTokenType.RIGHT_PAREN, "Expected ')' after declarator");
                if(ERROR_OCCURRED) return null;
            }
        }

        // Handle identifier (only if we didn't parse a nested declarator)
        if(decl.nested is null){
            if(check(CTokenType.IDENTIFIER)){
                decl.name = advance();
            } else if(!allow_abstract){
                error("Expected identifier in declarator");
                return null;
            }
            // For abstract declarators (allow_abstract=true), name is optional
        }

        // Skip __attribute__((...)) after identifier
        while(check(CTokenType.IDENTIFIER) && peek().lexeme == "__attribute__"){
            advance();
            if(check(CTokenType.LEFT_PAREN)){
                skip_balanced_parens();
            }
        }

        // Handle array and function suffixes (left-to-right)
        // These suffixes apply to the direct-declarator (which may include a nested declarator)
        while(true){
            if(check(CTokenType.LEFT_BRACKET)){
                // (6.7.7.1) array-declarator:
                //     direct-declarator [ type-qualifier-list_opt assignment-expression_opt ]
                advance();  // consume '['
                decl.is_array = true;

                // Parse array size if present
                size_t dim = 0;
                if(!check(CTokenType.RIGHT_BRACKET)){
                    auto size_result = parse_enum_const_expr();
                    if(!size_result.err && size_result.value > 0){
                        dim = cast(size_t)size_result.value;
                    }
                }
                decl.array_dim = dim;

                consume(CTokenType.RIGHT_BRACKET, "Expected ']' after array size");
                if(ERROR_OCCURRED) return null;

            } else if(check(CTokenType.LEFT_PAREN)){
                // (6.7.7.1) function-declarator:
                //     direct-declarator ( parameter-type-list_opt )
                // This applies whether or not we have a nested declarator
                // For `int (*ptr)(int)`, after parsing `(*ptr)`, this parses `(int)`
                decl.is_function = true;

                // Parse the parameter list
                advance();  // consume '('
                auto param_result = parse_parameter_type_list();
                if(param_result.err) return null;
                decl.is_varargs = param_result.is_varargs;
                // Store param types
                auto param_types = make_barray!(CType*)(allocator);
                foreach(ref p; param_result.params[]){
                    param_types ~= p.type;
                }
                decl.param_types = param_types[];

                consume(CTokenType.RIGHT_PAREN, "Expected ')' after parameters");
                if(ERROR_OCCURRED) return null;

            } else {
                break;
            }
        }

        return decl;
    }

    // Helper: Check if a token starts a type
    bool is_type_start(CToken tok){
        if(tok.type == CTokenType.IDENTIFIER){
            // Check if it's a typedef name
            return (tok.lexeme in typedef_types) !is null;
        }
        // Check for type keywords
        switch(tok.type){
            case CTokenType.VOID:
            case CTokenType.CHAR:
            case CTokenType.SHORT:
            case CTokenType.INT:
            case CTokenType.LONG:
            case CTokenType.FLOAT:
            case CTokenType.DOUBLE:
            case CTokenType.SIGNED:
            case CTokenType.UNSIGNED:
            case CTokenType.STRUCT:
            case CTokenType.UNION:
            case CTokenType.ENUM:
            case CTokenType.CONST:
            case CTokenType.VOLATILE:
            case CTokenType.RESTRICT:
            case CTokenType.ATOMIC:
                return true;
            default:
                return false;
        }
    }

    // Helper: Build the return type for a complex function declaration.
    // For declarations like `void(*signal(int))(int)`, the return type is `void(*)(int)`.
    // This applies declarator parts from the outermost down to (but not including) the
    // function-declarator containing the name.
    //
    // Parameters:
    //   base: The base type specifier (e.g., void)
    //   decl: The full declarator
    //   name_decl: The declarator that contains the identifier (has is_function=true for the params)
    CType* build_function_return_type(CType* base, CDeclarator* decl, CDeclarator* name_decl){
        if(decl is null || base is null) return base;
        if(decl is name_decl){
            // We've reached the function declarator containing the name
            // Apply only the pointers, not the function part (that's the function's params, not return type)
            CType* result = base;
            for(int i = 0; i < decl.pointer_depth; i++){
                result = make_pointer_type(allocator, result);
            }
            // Don't apply is_function here - that's the function we're declaring
            return result;
        }

        // Apply this level's modifiers
        CType* result = base;

        // Apply pointers
        for(int i = 0; i < decl.pointer_depth; i++){
            result = make_pointer_type(allocator, result);
        }

        // Apply array suffix
        if(decl.is_array){
            if(decl.array_dim > 0){
                result = make_array_type(allocator, result, decl.array_dim);
            } else {
                result = make_pointer_type(allocator, result);
            }
        }

        // Apply function suffix
        if(decl.is_function){
            result = make_function_type(allocator, result, decl.param_types, decl.is_varargs);
        }

        // Recurse into nested declarator
        if(decl.nested !is null){
            result = build_function_return_type(result, decl.nested, name_decl);
        }

        return result;
    }

    // Helper: Apply declarator modifiers to a base type to build the final CType*
    // This converts the declarator's pointer_depth, array_dim, param_types, etc. into CType nodes
    //
    // Example: int (*ptr)[10]
    //   - Outer declarator: is_array=true, array_dim=10, pointer_depth=0
    //   - Nested declarator: pointer_depth=1, name=ptr
    //   Build: int -> int[10] -> pointer to int[10]
    //
    // Example: int (*funcs[10])(void)
    //   - Outer declarator: is_function=true, params=void, pointer_depth=0
    //   - Nested declarator: pointer_depth=1, is_array=true, array_dim=10, name=funcs
    //   Build: int -> int(void) -> int(*)(void) -> int(*[10])(void)
    CType* apply_declarator_to_type(CType* base, CDeclarator* decl){
        if(decl is null || base is null) return base;

        CType* result = base;

        // Step 1: Apply this level's pointers first
        // These pointers are "closest" to the base type in the type hierarchy
        for(int i = 0; i < decl.pointer_depth; i++){
            result = make_pointer_type(allocator, result);
        }

        // Step 2: Apply suffixes (array, function)
        // These have higher precedence than the nested declarator's modifiers
        if(decl.is_array){
            if(decl.array_dim > 0){
                result = make_array_type(allocator, result, decl.array_dim);
            } else {
                // Unsized array - treat as pointer (e.g., int x[])
                result = make_pointer_type(allocator, result);
            }
        }

        if(decl.is_function){
            result = make_function_type(allocator, result, decl.param_types, decl.is_varargs);
        }

        // Step 3: Recurse into nested declarator
        // The nested declarator's modifiers wrap around everything we've built so far
        if(decl.nested !is null){
            result = apply_declarator_to_type(result, decl.nested);
        }

        return result;
    }

    // ========================================================================
    // Phase 2: Unified Member Declaration Parsing
    // ========================================================================

    // (6.7.3.2) member-declaration-list:
    //     member-declaration
    //     member-declaration-list member-declaration
    // Bitfield packing state
    struct BitfieldState {
        size_t storage_start;  // Byte offset where current storage unit starts
        size_t storage_size;   // Size of current storage unit (e.g., 4 for int)
        size_t bits_used;      // Bits used in current storage unit
        bool active;           // Whether we're in a bitfield sequence
    }

    //
    // Parses all member declarations in a struct/union body.
    // Returns the total size (accumulated offset for structs, max size for unions).
    // For structs: is_union=false, offset accumulates
    // For unions: is_union=true, all offsets are 0, returns max field size
    // Total size is padded to be a multiple of struct alignment (max field alignment).
    int parse_member_declaration_list(Barray!StructField* fields, size_t* total_size, bool is_union){
        size_t offset = 0;
        size_t max_size = 0;
        BitfieldState bf_state;

        while(!check(CTokenType.RIGHT_BRACE) && !at_end){
            size_t field_size = 0;
            int err = parse_member_declaration(fields, is_union ? 0 : offset, &field_size, is_union, &bf_state);
            if(err) return err;

            if(is_union){
                if(field_size > max_size) max_size = field_size;
            } else {
                offset += field_size;
            }
        }

        // Finalize any active bitfield storage unit
        if(!is_union && bf_state.active){
            // The last bitfield group may need its storage unit accounted for
            // (already handled in parse_member_declaration when transitioning)
        }

        size_t raw_size = is_union ? max_size : offset;

        // Compute struct alignment (max alignment of all fields)
        size_t max_align = 1;
        foreach(ref f; (*fields)[]){
            size_t a = f.type.align_of();
            if(a > max_align) max_align = a;
        }

        // Pad total size to be a multiple of struct alignment
        *total_size = (raw_size + max_align - 1) & ~(max_align - 1);
        return 0;
    }

    // (6.7.3.2) member-declaration:
    //     attribute-specifier-sequence_opt specifier-qualifier-list member-declarator-list_opt ;
    //     static_assert-declaration
    //
    // Parses a single member declaration line (which may declare multiple fields via comma).
    // base_offset: starting offset for fields in this declaration
    // total_field_size: output - total size of all fields declared
    // bf_state: bitfield packing state (tracks current storage unit)
    int parse_member_declaration(Barray!StructField* fields, size_t base_offset, size_t* total_field_size, bool is_union, BitfieldState* bf_state){
        *total_field_size = 0;

        // Check for inline struct/union: struct { ... } field_name; or struct { ... };
        if((check(CTokenType.UNION) || check(CTokenType.STRUCT)) &&
            peek_at(1).type == CTokenType.LEFT_BRACE){
            // Non-bitfield member: finalize any active bitfield group
            if(!is_union && bf_state.active){
                bf_state.active = false;
            }

            bool is_inline_union = check(CTokenType.UNION);
            advance();  // consume union/struct
            advance();  // consume {

            // Parse the inline struct/union body
            auto inline_fields = make_barray!StructField(allocator);
            size_t inline_size = 0;
            int err = parse_member_declaration_list(&inline_fields, &inline_size, is_inline_union);
            if(err) return err;

            consume(CTokenType.RIGHT_BRACE, "Expected '}' after inline struct/union");
            if(ERROR_OCCURRED) return 1;

            // Check for field name after the struct body: struct { ... } field_name;
            if(check(CTokenType.IDENTIFIER)){
                CToken field_name = advance();
                // Create anonymous struct/union type for this field
                CType* inline_type;
                if(is_inline_union){
                    inline_type = make_union_type(allocator, "", inline_fields[], inline_size);
                } else {
                    inline_type = make_struct_type(allocator, "", inline_fields[], inline_size);
                }
                // Handle array dimension: struct { ... } field_name[N];
                while(match(CTokenType.LEFT_BRACKET)){
                    auto size_result = parse_enum_const_expr();
                    if(size_result.err) return 1;
                    if(size_result.value <= 0){
                        error("Array size must be positive");
                        return 1;
                    }
                    consume(CTokenType.RIGHT_BRACKET, "Expected ']' after array size");
                    if(ERROR_OCCURRED) return 1;
                    inline_type = make_array_type(allocator, inline_type, cast(size_t) size_result.value);
                }
                StructField field;
                field.name = field_name.lexeme;
                field.type = inline_type;
                field.offset = base_offset;
                *fields ~= field;
                *total_field_size = inline_type.size_of();
            } else {
                // True anonymous struct/union - add fields directly to parent
                foreach(ref f; inline_fields[]){
                    StructField field;
                    field.name = f.name;
                    field.type = f.type;
                    field.offset = base_offset + f.offset;
                    *fields ~= field;
                }
                *total_field_size = inline_size;
            }
            consume(CTokenType.SEMICOLON, "Expected ';' after inline struct/union");
            if(ERROR_OCCURRED) return 1;
            return 0;
        }

        // Check for anonymous enum field: enum { ... } field_name;
        if(check(CTokenType.ENUM) && peek_at(1).type == CTokenType.LEFT_BRACE){
            // Non-bitfield member: finalize any active bitfield group
            if(!is_union && bf_state.active){
                bf_state.active = false;
            }

            advance();  // consume 'enum'
            advance();  // consume '{'
            // Parse and register enum values
            long enum_val = 0;
            while(!check(CTokenType.RIGHT_BRACE) && !at_end){
                CToken ename = consume(CTokenType.IDENTIFIER, "Expected enum constant name");
                if(ERROR_OCCURRED) return 1;
                if(match(CTokenType.EQUAL)){
                    auto result = parse_enum_const_expr();
                    if(result.err) return 1;
                    enum_val = result.value;
                }
                enum_constants[ename.lexeme] = enum_val;
                enum_val++;
                if(!match(CTokenType.COMMA)) break;
            }
            consume(CTokenType.RIGHT_BRACE, "Expected '}'");
            if(ERROR_OCCURRED) return 1;
            // Now parse the field name
            CToken field_name = consume(CTokenType.IDENTIFIER, "Expected field name");
            if(ERROR_OCCURRED) return 1;
            StructField field;
            field.name = field_name.lexeme;
            field.type = &TYPE_INT;  // enum is int
            field.offset = base_offset;
            *fields ~= field;
            *total_field_size = TYPE_INT.size_of();
            consume(CTokenType.SEMICOLON, "Expected ';'");
            if(ERROR_OCCURRED) return 1;
            return 0;
        }

        // (6.7.3.2) specifier-qualifier-list - parse the base type
        CType* base_type = parse_type();
        if(base_type is null) return 1;

        // Check for function pointer field: type (CONV * name) (params)
        if(check(CTokenType.LEFT_PAREN)){
            // Non-bitfield member: finalize any active bitfield group
            if(!is_union && bf_state.active){
                bf_state.active = false;
            }
            return parse_function_pointer_member(fields, base_offset, total_field_size, base_type);
        }

        // (6.7.3.2) member-declarator-list:
        //     member-declarator
        //     member-declarator-list , member-declarator
        //
        // Get the base type without pointers - each declarator can have its own pointer level
        // e.g., `int *a, *b, c;` - a and b are int*, c is int
        CType* pure_base_type = unwrap_pointers(base_type);
        size_t current_offset = base_offset;
        bool first_declarator = true;
        do {
            // (6.7.3.2) member-declarator:
            //     declarator
            //     declarator_opt : constant-expression

            // Check for anonymous bit field (e.g., unsigned int :24;)
            if(check(CTokenType.COLON)){
                advance();  // consume :
                auto width_result = parse_enum_const_expr();
                if(width_result.err) return 1;
                long width = width_result.value;
                size_t type_size = base_type.size_of();
                size_t type_bits = type_size * 8;

                if(width == 0){
                    // Zero-width: force alignment to next storage unit
                    if(bf_state.active){
                        // Finish current storage unit
                        current_offset = bf_state.storage_start + bf_state.storage_size;
                        *total_field_size = current_offset - base_offset;
                        bf_state.active = false;
                    }
                } else if(!is_union){
                    // Anonymous bitfield still consumes bits
                    if(!bf_state.active || bf_state.storage_size != type_size ||
                       bf_state.bits_used + width > type_bits){
                        // Start new storage unit
                        if(bf_state.active){
                            current_offset = bf_state.storage_start + bf_state.storage_size;
                        }
                        // Align to type alignment
                        size_t align_req = base_type.align_of();
                        size_t misalign = current_offset % align_req;
                        if(misalign != 0) current_offset += align_req - misalign;

                        bf_state.storage_start = current_offset;
                        bf_state.storage_size = type_size;
                        bf_state.bits_used = cast(size_t)width;
                        bf_state.active = true;
                    } else {
                        bf_state.bits_used += width;
                    }
                    *total_field_size = bf_state.storage_start + bf_state.storage_size - base_offset;
                }
                first_declarator = false;
                continue;
            }

            // For the first declarator, use the type as-is (it already has pointers consumed)
            // For subsequent declarators, start from pure base type and parse pointer modifiers
            CType* decl_type;
            if(first_declarator){
                decl_type = base_type;
                first_declarator = false;
            } else {
                decl_type = parse_pointer_modifiers(pure_base_type);
            }

            // Regular field - parse name
            CToken field_name = consume(CTokenType.IDENTIFIER, "Expected field name");
            if(ERROR_OCCURRED) return 1;

            // Handle arrays (including multi-dimensional and flexible arrays)
            while(match(CTokenType.LEFT_BRACKET)){
                // Check for flexible array member: type name[]
                if(check(CTokenType.RIGHT_BRACKET)){
                    advance();  // consume ']'
                    // Flexible array member - treat as zero-size array
                    decl_type = make_array_type(allocator, decl_type, 0);
                    continue;
                }
                auto size_result = parse_enum_const_expr();
                if(size_result.err) return 1;
                if(size_result.value <= 0){
                    error("Array size must be positive");
                    return 1;
                }
                consume(CTokenType.RIGHT_BRACKET, "Expected ']' after array size");
                if(ERROR_OCCURRED) return 1;
                decl_type = make_array_type(allocator, decl_type, cast(size_t) size_result.value);
            }

            // Handle bit fields (e.g., unsigned int field:2;)
            if(match(CTokenType.COLON)){
                auto width_result = parse_enum_const_expr();
                if(width_result.err) return 1;
                long width = width_result.value;
                size_t type_size = decl_type.size_of();
                size_t type_bits = type_size * 8;

                if(!is_union){
                    // Check if bitfield fits in current storage unit
                    if(!bf_state.active || bf_state.storage_size != type_size ||
                       bf_state.bits_used + width > type_bits){
                        // Start new storage unit
                        if(bf_state.active){
                            current_offset = bf_state.storage_start + bf_state.storage_size;
                        }
                        // Align to type alignment
                        size_t align_req = decl_type.align_of();
                        size_t misalign = current_offset % align_req;
                        if(misalign != 0) current_offset += align_req - misalign;

                        bf_state.storage_start = current_offset;
                        bf_state.storage_size = type_size;
                        bf_state.bits_used = cast(size_t)width;
                        bf_state.active = true;
                    } else {
                        bf_state.bits_used += width;
                    }

                    // Add field pointing to storage unit start
                    StructField field;
                    field.name = field_name.lexeme;
                    field.type = decl_type;
                    field.offset = bf_state.storage_start;
                    *fields ~= field;

                    *total_field_size = bf_state.storage_start + bf_state.storage_size - base_offset;
                } else {
                    // Union bitfields: each at offset 0
                    StructField field;
                    field.name = field_name.lexeme;
                    field.type = decl_type;
                    field.offset = 0;
                    *fields ~= field;
                    if(type_size > *total_field_size){
                        *total_field_size = type_size;
                    }
                }
                continue;  // Don't do normal field processing
            }

            // Non-bitfield member: finalize any active bitfield group
            if(!is_union && bf_state.active){
                current_offset = bf_state.storage_start + bf_state.storage_size;
                bf_state.active = false;
            }

            // Add the field
            StructField field;
            field.name = field_name.lexeme;
            field.type = decl_type;

            // Align the offset to the field's alignment requirement
            size_t field_align = decl_type.align_of();
            if(!is_union && field_align > 0){
                size_t misalign = current_offset % field_align;
                if(misalign != 0){
                    current_offset += field_align - misalign;  // Add padding
                }
            }

            field.offset = is_union ? 0 : current_offset;
            *fields ~= field;

            size_t field_size = decl_type.size_of();
            if(is_union){
                // For unions, track max field size
                if(field_size > *total_field_size){
                    *total_field_size = field_size;
                }
            } else {
                current_offset += field_size;
                *total_field_size = current_offset - base_offset;  // Return delta including padding
            }

        } while(match(CTokenType.COMMA));

        consume(CTokenType.SEMICOLON, "Expected ';' after field declaration");
        if(ERROR_OCCURRED) return 1;
        return 0;
    }

    // Helper for parsing function pointer members: type (CONV * name) (params)
    int parse_function_pointer_member(Barray!StructField* fields, size_t offset, size_t* field_size, CType* return_type){
        advance();  // consume '('
        // Skip calling convention identifiers until we hit '*'
        while(check(CTokenType.IDENTIFIER)){
            advance();
        }
        if(!match(CTokenType.STAR)){
            error("Expected '*' in function pointer field");
            return 1;
        }
        // Skip pointer qualifiers after * (e.g., (*const funcptr))
        while(match(CTokenType.CONST) || match(CTokenType.VOLATILE) || match(CTokenType.RESTRICT)){}

        CToken field_name = consume(CTokenType.IDENTIFIER, "Expected field name");
        if(ERROR_OCCURRED) return 1;

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after field name");
        if(ERROR_OCCURRED) return 1;

        // Skip the parameter list
        if(check(CTokenType.LEFT_PAREN)){
            skip_balanced_parens();
        }

        // Treat as void* for simplicity
        CType* fptr_type = make_pointer_type(allocator, &TYPE_VOID);

        consume(CTokenType.SEMICOLON, "Expected ';' after function pointer field");
        if(ERROR_OCCURRED) return 1;

        // Add function pointer field
        StructField field;
        field.name = field_name.lexeme;
        field.type = fptr_type;
        field.offset = offset;
        *fields ~= field;
        *field_size = fptr_type.size_of();

        return 0;
    }

    // ========================================================================
    // Phase 3: Unified Parameter Parsing
    // ========================================================================

    // Result type for parameter list parsing
    struct ParameterListResult {
        Barray!CParam params;
        bool is_varargs;
        bool err;
    }

    // (6.7.7.1) parameter-type-list:
    //     parameter-list
    //     parameter-list , ...
    //     ...
    //
    // Parses a complete parameter list (everything inside the parentheses).
    // Caller should have already consumed the opening '('.
    ParameterListResult parse_parameter_type_list(){
        ParameterListResult result;
        result.params = make_barray!CParam(allocator);
        result.is_varargs = false;
        result.err = false;

        if(!check(CTokenType.RIGHT_PAREN)){
            do {
                // (6.7.7.1) parameter-type-list can start with ...
                if(check(CTokenType.ELLIPSIS)){
                    advance();
                    result.is_varargs = true;
                    break;
                }

                // (6.7.7.1) parameter-declaration
                int perr = parse_parameter_declaration(&result.params);
                if(perr){
                    result.err = true;
                    return result;
                }
            } while(match(CTokenType.COMMA));
        }

        return result;
    }

    // (6.7.7.1) parameter-declaration:
    //     attribute-specifier-sequence_opt declaration-specifiers declarator
    //     attribute-specifier-sequence_opt declaration-specifiers abstract-declarator_opt
    //
    // Parses a single parameter and adds it to the params array.
    // Returns 0 on success, 1 on error.
    int parse_parameter_declaration(Barray!CParam* params){
        // Check for(void) - means no parameters
        if(check(CTokenType.VOID)){
            CToken void_tok = advance();
            if(check(CTokenType.RIGHT_PAREN) || check(CTokenType.COMMA)){
                // Just "void" alone - means no parameters if at end, or error if followed by comma
                if(check(CTokenType.COMMA)){
                    error("'void' parameter must be the only parameter");
                    return 1;
                }
                // "void" at end means no parameters - don't add anything
                return 0;
            }
            // void* or void type - handle as regular parameter starting with void
            CParam param;
            param.type = &TYPE_VOID;

            // Handle postfix qualifiers (void const *)
            while(match(CTokenType.CONST) || match(CTokenType.VOLATILE) || match(CTokenType.RESTRICT)){}

            // Handle pointers
            while(match(CTokenType.STAR)){
                param.type = make_pointer_type(allocator, param.type);
                while(match(CTokenType.CONST) || match(CTokenType.VOLATILE) || match(CTokenType.RESTRICT)){}
            }

            // Check for function pointer parameter: void (*name)(params)
            if(check(CTokenType.LEFT_PAREN)){
                advance();  // consume '('
                if(match(CTokenType.STAR)){
                    if(check(CTokenType.IDENTIFIER)){
                        param.name = advance();
                    }
                    consume(CTokenType.RIGHT_PAREN, "Expected ')' after function pointer");
                    if(ERROR_OCCURRED) return 1;
                    if(check(CTokenType.LEFT_PAREN)){
                        skip_balanced_parens();
                    }
                    param.type = make_pointer_type(allocator, &TYPE_VOID);
                    *params ~= param;
                    return 0;
                } else {
                    error("Unexpected '(' in void parameter");
                    return 1;
                }
            }

            // Parameter name is optional
            if(check(CTokenType.IDENTIFIER)){
                param.name = advance();
            }
            *params ~= param;
            return 0;
        }

        // Regular parameter
        CParam param;
        param.type = parse_type();
        if(param.type is null) return 1;

        // Check for function pointer parameter: type (*name)(params) or type (*)(params)
        if(check(CTokenType.LEFT_PAREN)){
            advance();  // consume '('
            if(match(CTokenType.STAR)){
                // Function pointer parameter - name is optional
                if(check(CTokenType.IDENTIFIER)){
                    param.name = advance();
                }
                consume(CTokenType.RIGHT_PAREN, "Expected ')' after function pointer");
                if(ERROR_OCCURRED) return 1;
                // Skip the parameter list
                if(check(CTokenType.LEFT_PAREN)){
                    skip_balanced_parens();
                }
                // Treat as void* for simplicity
                param.type = make_pointer_type(allocator, &TYPE_VOID);
                *params ~= param;
                return 0;
            } else {
                error("Unexpected '(' in parameter");
                return 1;
            }
        }

        // Skip const/volatile qualifiers between type and name
        while(match(CTokenType.CONST) || match(CTokenType.VOLATILE) || match(CTokenType.RESTRICT)){}

        // Parameter name is optional in declarations
        if(check(CTokenType.IDENTIFIER)){
            param.name = advance();
        }

        // Handle array parameters: type name[size] -> converted to pointer
        if(check(CTokenType.LEFT_BRACKET)){
            while(check(CTokenType.LEFT_BRACKET)){
                advance();  // consume '['
                while(!check(CTokenType.RIGHT_BRACKET) && !at_end){
                    advance();
                }
                if(check(CTokenType.RIGHT_BRACKET)) advance();
            }
            param.type = make_pointer_type(allocator, param.type);
        }

        *params ~= param;
        return 0;
    }

    // ========================================================================
    // Phase 4: Unified Global/Local Declaration Parsing
    // ========================================================================

    // (6.7.1) init-declarator-list:
    //     init-declarator
    //     init-declarator-list , init-declarator
    //
    // Parses a comma-separated list of global variable declarations.
    // The first variable (first_type, first_name) has already been parsed.
    // Adds all variables to the globals array.
    // Does NOT consume the final semicolon - caller must do that.
    // is_extern: true if 'extern' keyword was present (declaration only, object defined elsewhere)
    // library: from #pragma library, specifies which library contains the extern object
    int parse_init_declarator_list(Barray!CGlobalVar* globals, CType* first_type, CToken first_name,
                                   bool is_extern = false, str library = null){
        // Get base type by unwrapping pointers (for subsequent declarators)
        CType* base_type = unwrap_pointers(first_type);

        // (6.7.1) init-declarator:
        //     declarator
        //     declarator = initializer
        // Parse the first variable
        CGlobalVar gvar;
        int err = parse_global_var_rest(first_type, first_name, &gvar, is_extern, library);
        if(err) return err;
        *globals ~= gvar;

        // Parse remaining comma-separated declarators
        while(match(CTokenType.COMMA)){
            // Each declarator can have its own pointer modifiers
            CType* decl_type = parse_pointer_modifiers(base_type);
            CToken next_name = consume(CTokenType.IDENTIFIER, "Expected variable name");
            if(ERROR_OCCURRED) return 1;

            CGlobalVar gvar2;
            err = parse_global_var_rest(decl_type, next_name, &gvar2, is_extern, library);
            if(err) return err;
            *globals ~= gvar2;
        }

        return 0;
    }

    // (6.7.3.1) type-specifier:
    //     void | char | short | int | long | float | double | signed | unsigned
    //     _BitInt ( constant-expression )     (NOT IMPLEMENTED)
    //     bool | _Complex                     (NOT IMPLEMENTED)
    //     _Decimal32 | _Decimal64 | _Decimal128  (NOT IMPLEMENTED)
    //     atomic-type-specifier | struct-or-union-specifier | enum-specifier
    //     typedef-name | typeof-specifier
    //
    // (6.7.4.1) type-qualifier: const | restrict | volatile | _Atomic
    CType* parse_base_type(){
        bool is_unsigned = false;
        bool is_const = false;

        // Handle qualifiers and modifiers
        while(true){
            if(match(CTokenType.CONST)){
                is_const = true;
            } else if(match(CTokenType.VOLATILE)){
                // Skip volatile - we don't track it
            } else if(match(CTokenType.RESTRICT)){
                // Skip restrict - we don't track it
            } else if(match(CTokenType.ATOMIC)){
                // Skip _Atomic - handle both _Atomic T and _Atomic(T)
                if(match(CTokenType.LEFT_PAREN)){
                    // _Atomic(T) - parse full type T (may include pointers) and consume closing paren
                    CType* atomic_type = parse_type();
                    consume(CTokenType.RIGHT_PAREN, "Expected ')' after _Atomic type");
                    if(ERROR_OCCURRED) return null;
                    return atomic_type;  // Return the inner type
                }
                // Otherwise just _Atomic as qualifier, continue parsing
            } else if(match(CTokenType.UNSIGNED)){
                is_unsigned = true;
            } else if(match(CTokenType.SIGNED)){
                is_unsigned = false;
            } else {
                break;
            }
        }

        CType* result;

        if(match(CTokenType.VOID)){
            result = &TYPE_VOID;
        } else if(match(CTokenType.CHAR)){
            result = is_unsigned ? &TYPE_UCHAR : &TYPE_CHAR;
        } else if(match(CTokenType.SHORT)){
            // Handle trailing modifiers (e.g., 'short unsigned int')
            while(match(CTokenType.UNSIGNED)) is_unsigned = true;
            while(match(CTokenType.SIGNED)) is_unsigned = false;
            match(CTokenType.INT);  // optional trailing int
            // Allocate new type for short
            auto data = allocator.alloc(CType.sizeof);
            result = cast(CType*)data.ptr;
            result.kind = CTypeKind.SHORT;
            result.is_unsigned = is_unsigned;
        } else if(match(CTokenType.INT)){
            // Handle trailing modifiers (e.g., 'int unsigned')
            while(match(CTokenType.UNSIGNED)) is_unsigned = true;
            while(match(CTokenType.SIGNED)) is_unsigned = false;
            result = is_unsigned ? &TYPE_UINT : &TYPE_INT;
        } else if(match(CTokenType.LONG)){
            // Check for 'long long' or 'long double'
            // Also handle trailing unsigned/signed and int (e.g., 'long unsigned int')
            if(match(CTokenType.LONG)){
                // long long - same as long on 64-bit
                // Consume trailing unsigned/signed/int
                while(match(CTokenType.UNSIGNED)) is_unsigned = true;
                while(match(CTokenType.SIGNED)) is_unsigned = false;
                match(CTokenType.INT);  // optional trailing int
                result = is_unsigned ? &TYPE_ULONG : &TYPE_LONG;
            } else if(match(CTokenType.DOUBLE)){
                // long double - 80-bit extended precision (16 bytes on x86_64)
                result = &TYPE_LONG_DOUBLE;
                match(CTokenType.COMPLEX);  // consume trailing _Complex if present
            } else {
                // Consume trailing unsigned/signed/int (e.g., 'long unsigned int')
                while(match(CTokenType.UNSIGNED)) is_unsigned = true;
                while(match(CTokenType.SIGNED)) is_unsigned = false;
                match(CTokenType.INT);  // optional trailing int
                result = is_unsigned ? &TYPE_ULONG : &TYPE_LONG;
            }
        } else if(match(CTokenType.FLOAT)){
            result = &TYPE_FLOAT;
            match(CTokenType.COMPLEX);  // consume trailing _Complex if present (stub)
        } else if(match(CTokenType.DOUBLE)){
            result = &TYPE_DOUBLE;
            match(CTokenType.COMPLEX);  // consume trailing _Complex if present (stub)
        } else if(match(CTokenType.FLOAT16)){
            // _Float16 - treat as float for now
            result = &TYPE_FLOAT;
        } else if(match(CTokenType.FLOAT32)){
            // _Float32 - treat as float
            result = &TYPE_FLOAT;
        } else if(match(CTokenType.FLOAT64)){
            // _Float64 - treat as double
            result = &TYPE_DOUBLE;
        } else if(match(CTokenType.FLOAT128)){
            // _Float128 - stub as double for now
            result = &TYPE_DOUBLE;
        } else if(match(CTokenType.FLOAT32X)){
            // _Float32x - stub as double for now
            result = &TYPE_DOUBLE;
        } else if(match(CTokenType.FLOAT64X)){
            // _Float64x - stub as double for now
            result = &TYPE_DOUBLE;
        } else if(match(CTokenType.BOOL)){
            // _Bool - treat as unsigned char for now
            result = &TYPE_UCHAR;
        } else if(match(CTokenType.INT128)){
            result = is_unsigned ? &TYPE_UINT128 : &TYPE_INT128;
        } else if(match(CTokenType.UINT128)){
            result = &TYPE_UINT128;
        } else if(match(CTokenType.COMPLEX)){
            // _Complex - stub as double for parsing, codegen will error if used
            // Skip the following type specifier (double, float, etc.)
            if(match(CTokenType.DOUBLE) || match(CTokenType.FLOAT) || match(CTokenType.LONG)){
                // Consumed
            }
            result = &TYPE_DOUBLE;  // Stub
        } else if(match(CTokenType.DECIMAL32)){
            result = &TYPE_FLOAT;  // Stub
        } else if(match(CTokenType.DECIMAL64)){
            result = &TYPE_DOUBLE;  // Stub
        } else if(match(CTokenType.DECIMAL128)){
            result = &TYPE_DOUBLE;  // Stub
        } else if(match(CTokenType.STRUCT)){
            // struct Name or struct { ... }
            bool has_name = check(CTokenType.IDENTIFIER);
            CToken struct_name;
            if(has_name){
                struct_name = advance();
            }

            // Check for inline definition
            if(check(CTokenType.LEFT_BRACE)){
                advance();  // consume '{'
                auto fields = make_barray!StructField(allocator);
                size_t total_size = 0;

                while(!check(CTokenType.RIGHT_BRACE) && !at_end){
                    CType* field_type = parse_type();
                    if(field_type is null) return null;

                    // Handle comma-separated declarators (e.g., int x:1, y:2;)
                    CType* base_type = field_type;
                    do {
                        // Check for anonymous bit field
                        if(check(CTokenType.COLON)){
                            advance();  // consume :
                            auto width_result = parse_enum_const_expr();
                            if(width_result.err) return null;
                            // Skip anonymous bit fields - they're just padding
                        } else {
                            CToken field_name = consume(CTokenType.IDENTIFIER, "Expected field name");
                            if(ERROR_OCCURRED) return null;

                            CType* decl_type = base_type;

                            while(match(CTokenType.LEFT_BRACKET)){
                                // Check for flexible array member
                                if(check(CTokenType.RIGHT_BRACKET)){
                                    advance();
                                    decl_type = make_array_type(allocator, decl_type, 0);
                                    continue;
                                }
                                auto size_result = parse_enum_const_expr();
                                if(size_result.err) return null;
                                if(size_result.value <= 0){
                                    error("Array size must be positive");
                                    return null;
                                }
                                consume(CTokenType.RIGHT_BRACKET, "Expected ']'");
                                if(ERROR_OCCURRED) return null;
                                decl_type = make_array_type(allocator, decl_type, cast(size_t) size_result.value);
                            }

                            // Handle bit fields
                            if(match(CTokenType.COLON)){
                                auto width_result = parse_enum_const_expr();
                                if(width_result.err) return null;
                            }

                            StructField field;
                            field.name = field_name.lexeme;
                            field.type = decl_type;
                            field.offset = total_size;
                            fields ~= field;
                            total_size += decl_type.size_of();
                        }
                    } while(match(CTokenType.COMMA));

                    consume(CTokenType.SEMICOLON, "Expected ';' after field");
                    if(ERROR_OCCURRED) return null;
                }

                consume(CTokenType.RIGHT_BRACE, "Expected '}'");
                if(ERROR_OCCURRED) return null;

                // Check if there's an existing forward-declared type to update
                if(has_name){
                    if(auto existing = struct_name.lexeme in struct_types){
                        // Update existing type in-place so typedefs continue to work
                        result = *existing;
                        result.fields = fields[];
                        result.struct_size = total_size;
                    } else {
                        result = make_struct_type(allocator, struct_name.lexeme, fields[], total_size);
                        struct_types[struct_name.lexeme] = result;
                    }
                } else {
                    result = make_struct_type(allocator, "", fields[], total_size);
                }
            } else {
                // Just a reference - need name
                if(!has_name){
                    error("Expected struct name or body");
                    return null;
                }
                // Look up struct in defined structs
                if(CType** found = struct_name.lexeme in struct_types){
                    result = *found;
                } else {
                    // Create incomplete struct type for forward reference
                    auto data = allocator.alloc(CType.sizeof);
                    CType* incomplete = cast(CType*)data.ptr;
                    incomplete.kind = CTypeKind.STRUCT;
                    incomplete.struct_name = struct_name.lexeme;
                    incomplete.struct_size = 0;  // Unknown size
                    struct_types[struct_name.lexeme] = incomplete;
                    result = incomplete;
                }
            }
        } else if(match(CTokenType.UNION)){
            // union Name or union { ... }
            bool has_name = check(CTokenType.IDENTIFIER);
            CToken union_name;
            if(has_name){
                union_name = advance();
            }

            // Check for inline definition
            if(check(CTokenType.LEFT_BRACE)){
                advance();  // consume '{'
                auto fields = make_barray!StructField(allocator);
                size_t max_size = 0;

                while(!check(CTokenType.RIGHT_BRACE) && !at_end){
                    CType* field_type = parse_type();
                    if(field_type is null) return null;

                    // Handle comma-separated declarators (e.g., int x:1, y:2;)
                    CType* base_type = field_type;
                    do {
                        // Check for anonymous bit field
                        if(check(CTokenType.COLON)){
                            advance();  // consume :
                            auto width_result = parse_enum_const_expr();
                            if(width_result.err) return null;
                            // Skip anonymous bit fields - they're just padding
                        } else {
                            CToken field_name = consume(CTokenType.IDENTIFIER, "Expected field name");
                            if(ERROR_OCCURRED) return null;

                            CType* decl_type = base_type;

                            while(match(CTokenType.LEFT_BRACKET)){
                                // Check for flexible array member
                                if(check(CTokenType.RIGHT_BRACKET)){
                                    advance();
                                    decl_type = make_array_type(allocator, decl_type, 0);
                                    continue;
                                }
                                auto size_result = parse_enum_const_expr();
                                if(size_result.err) return null;
                                if(size_result.value <= 0){
                                    error("Array size must be positive");
                                    return null;
                                }
                                consume(CTokenType.RIGHT_BRACKET, "Expected ']'");
                                if(ERROR_OCCURRED) return null;
                                decl_type = make_array_type(allocator, decl_type, cast(size_t) size_result.value);
                            }

                            // Handle bit fields
                            if(match(CTokenType.COLON)){
                                auto width_result = parse_enum_const_expr();
                                if(width_result.err) return null;
                            }

                            StructField field;
                            field.name = field_name.lexeme;
                            field.type = decl_type;
                            field.offset = 0;  // All union fields at offset 0
                            fields ~= field;
                            size_t field_size = decl_type.size_of();
                            if(field_size > max_size) max_size = field_size;
                        }
                    } while(match(CTokenType.COMMA));

                    consume(CTokenType.SEMICOLON, "Expected ';' after field");
                    if(ERROR_OCCURRED) return null;
                }

                consume(CTokenType.RIGHT_BRACE, "Expected '}'");
                if(ERROR_OCCURRED) return null;

                // Check if there's an existing forward-declared type to update
                if(has_name){
                    if(auto existing = union_name.lexeme in union_types){
                        // Update existing type in-place so typedefs continue to work
                        result = *existing;
                        result.fields = fields[];
                        result.struct_size = max_size;
                    } else {
                        result = make_union_type(allocator, union_name.lexeme, fields[], max_size);
                        union_types[union_name.lexeme] = result;
                    }
                } else {
                    result = make_union_type(allocator, "", fields[], max_size);
                }
            } else {
                // Just a reference - need name
                if(!has_name){
                    error("Expected union name or body");
                    return null;
                }
                // Look up union in defined unions
                if(CType** found = union_name.lexeme in union_types){
                    result = *found;
                } else {
                    // Create incomplete union type for forward reference
                    auto data = allocator.alloc(CType.sizeof);
                    CType* incomplete = cast(CType*)data.ptr;
                    incomplete.kind = CTypeKind.UNION;
                    incomplete.struct_name = union_name.lexeme;
                    incomplete.struct_size = 0;  // Unknown size
                    union_types[union_name.lexeme] = incomplete;
                    result = incomplete;
                }
            }
        } else if(match(CTokenType.ENUM)){
            // enum Name or enum Name { ... } or enum { ... }
            CToken name;
            bool has_name = false;
            if(check(CTokenType.IDENTIFIER)){
                name = advance();
                has_name = true;
            }

            // Check for inline enum definition
            if(check(CTokenType.LEFT_BRACE)){
                advance();  // consume '{'

                // Parse enum constants
                long next_value = 0;
                while(!check(CTokenType.RIGHT_BRACE) && !at_end){
                    CToken const_name = consume(CTokenType.IDENTIFIER, "Expected enum constant name");
                    if(ERROR_OCCURRED) return null;

                    long value = next_value;
                    if(match(CTokenType.EQUAL)){
                        auto expr_result = parse_enum_const_expr();
                        if(expr_result.err) return null;
                        value = expr_result.value;
                    }

                    enum_constants[const_name.lexeme] = value;
                    next_value = value + 1;

                    if(!check(CTokenType.RIGHT_BRACE)){
                        if(!match(CTokenType.COMMA)){
                            if(!check(CTokenType.RIGHT_BRACE)){
                                error("Expected ',' or '}' after enum constant");
                                return null;
                            }
                        }
                    }
                }

                consume(CTokenType.RIGHT_BRACE, "Expected '}' after enum constants");
                if(ERROR_OCCURRED) return null;

                // Create and register the enum type
                result = make_enum_type(allocator, has_name ? name.lexeme : "");
                if(has_name){
                    enum_types[name.lexeme] = result;
                }
            } else {
                // Just a reference to existing enum
                if(!has_name){
                    error("Expected enum name or '{'");
                    return null;
                }
                if(CType** found = name.lexeme in enum_types){
                    result = *found;
                } else {
                    error("Unknown enum type");
                    return null;
                }
            }
        } else if(is_unsigned){
            // 'unsigned' by itself means 'unsigned int'
            result = &TYPE_UINT;
        } else if(check(CTokenType.IDENTIFIER)){
            // Check for typedef name
            CToken name = peek();
            if(CType** found = name.lexeme in typedef_types){
                advance();  // consume the typedef name
                result = *found;
            } else {
                error("Expected type specifier");
                return null;
            }
        } else {
            error("Expected type specifier");
            return null;
        }

        // Handle 'long int', 'long long', etc.
        if(result.kind == CTypeKind.LONG || result.kind == CTypeKind.SHORT){
            match(CTokenType.INT);  // Optional 'int' after long/short
        }

        if(is_const && result !is &TYPE_VOID){
            // Need to allocate a new type with const flag
            auto data = allocator.alloc(CType.sizeof);
            auto new_type = cast(CType*)data.ptr;
            *new_type = *result;
            new_type.is_const = true;
            result = new_type;
        }

        return result;
    }

    // =========================================================================
    // Statement Parsing
    // =========================================================================

    // (6.8.1) statement:
    //     labeled-statement | compound-statement | expression-statement
    //     selection-statement | iteration-statement | jump-statement
    CStmt* parse_statement(){
        if(match(CTokenType.SEMICOLON)){
            return CEmptyStmt.get();
        }
        if(match(CTokenType.RETURN)) return parse_return();
        if(match(CTokenType.IF)) return parse_if();
        if(match(CTokenType.WHILE)) return parse_while();
        if(match(CTokenType.DO)) return parse_do_while();
        if(match(CTokenType.FOR)) return parse_for();
        if(match(CTokenType.LEFT_BRACE)) return parse_block();
        if(match(CTokenType.BREAK)) return parse_break();
        if(match(CTokenType.CONTINUE)) return parse_continue();
        if(match(CTokenType.SWITCH)) return parse_switch();
        if(match(CTokenType.GOTO)) return parse_goto();
        if(match(CTokenType.DASM)) return parse_dasm();
        if(match(CTokenType.ASM)) return parse_asm();

        // Handle case/default labels inside switch (including nested statements like Duff's device)
        if(switch_depth > 0){
            if(match(CTokenType.CASE)) return parse_case_label();
            if(match(CTokenType.DEFAULT)) return parse_default_label();
        }

        // Check for label: identifier followed by colon
        if(check(CTokenType.IDENTIFIER) && peek_at(1).type == CTokenType.COLON){
            return parse_label();
        }

        // Check for variable declaration (starts with type)
        if(is_type_specifier(peek())){
            return parse_var_decl();
        }

        // Expression statement
        return parse_expr_stmt();
    }

    bool is_type_specifier(CToken tok){
        with (CTokenType){
            if(tok.type == VOID || tok.type == CHAR || tok.type == SHORT ||
                tok.type == INT || tok.type == LONG || tok.type == FLOAT ||
                tok.type == DOUBLE || tok.type == UNSIGNED ||
                tok.type == SIGNED || tok.type == CONST || tok.type == VOLATILE ||
                tok.type == RESTRICT || tok.type == ATOMIC || tok.type == STRUCT ||
                tok.type == UNION || tok.type == ENUM || tok.type == BOOL ||
                tok.type == COMPLEX || tok.type == DECIMAL32 || tok.type == DECIMAL64 ||
                tok.type == DECIMAL128){
                return true;
            }
            // Check for typedef names
            if(tok.type == IDENTIFIER){
                return (tok.lexeme in typedef_types) !is null;
            }
            return false;
        }
    }

    // (6.8.7.1) return statement:
    //     return expression_opt ;
    CStmt* parse_return(){
        CToken keyword = previous();
        CExpr* value = null;

        if(!check(CTokenType.SEMICOLON)){
            value = parse_expression();
            if(value is null) return null;
        }

        consume(CTokenType.SEMICOLON, "Expected ';' after return");
        if(ERROR_OCCURRED) return null;

        return CReturnStmt.make(allocator, value, keyword);
    }

    // (6.8.5.1) if statement:
    //     if( expression ) statement
    //     if( expression ) statement else statement
    CStmt* parse_if(){
        CToken keyword = previous();

        consume(CTokenType.LEFT_PAREN, "Expected '(' after 'if'");
        if(ERROR_OCCURRED) return null;

        CExpr* condition = parse_expression();
        if(condition is null) return null;

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after if condition");
        if(ERROR_OCCURRED) return null;

        CStmt* then_branch = parse_statement();
        if(then_branch is null) return null;

        CStmt* else_branch = null;
        if(match(CTokenType.ELSE)){
            else_branch = parse_statement();
            if(else_branch is null) return null;
        }

        return CIfStmt.make(allocator, condition, then_branch, else_branch, keyword);
    }

    // (6.8.6.1) while statement:
    //     while( expression ) statement
    CStmt* parse_while(){
        CToken keyword = previous();

        consume(CTokenType.LEFT_PAREN, "Expected '(' after 'while'");
        if(ERROR_OCCURRED) return null;

        CExpr* condition = parse_expression();
        if(condition is null) return null;

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after while condition");
        if(ERROR_OCCURRED) return null;

        CStmt* body = parse_statement();
        if(body is null) return null;

        return CWhileStmt.make(allocator, condition, body, keyword);
    }

    // (6.8.6.1) do-while statement:
    //     do statement while( expression ) ;
    CStmt* parse_do_while(){
        CToken keyword = previous();

        CStmt* body = parse_statement();
        if(body is null) return null;

        consume(CTokenType.WHILE, "Expected 'while' after do body");
        if(ERROR_OCCURRED) return null;

        consume(CTokenType.LEFT_PAREN, "Expected '(' after 'while'");
        if(ERROR_OCCURRED) return null;

        CExpr* condition = parse_expression();
        if(condition is null) return null;

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after do-while condition");
        if(ERROR_OCCURRED) return null;

        consume(CTokenType.SEMICOLON, "Expected ';' after do-while");
        if(ERROR_OCCURRED) return null;

        return CDoWhileStmt.make(allocator, body, condition, keyword);
    }

    // (6.8.6.1) for statement:
    //     for( expression_opt ; expression_opt ; expression_opt ) statement
    //     for( declaration expression_opt ; expression_opt ) statement
    CStmt* parse_for(){
        CToken keyword = previous();

        consume(CTokenType.LEFT_PAREN, "Expected '(' after 'for'");
        if(ERROR_OCCURRED) return null;

        // Initializer
        CStmt* init = null;
        if(!check(CTokenType.SEMICOLON)){
            if(is_type_specifier(peek())){
                init = parse_var_decl();
            } else {
                init = parse_expr_stmt();
            }
            if(init is null) return null;
        } else {
            advance();  // consume ';'
        }

        // Condition
        CExpr* condition = null;
        if(!check(CTokenType.SEMICOLON)){
            condition = parse_expression();
            if(condition is null) return null;
        }
        consume(CTokenType.SEMICOLON, "Expected ';' after for condition");
        if(ERROR_OCCURRED) return null;

        // Increment
        CExpr* increment = null;
        if(!check(CTokenType.RIGHT_PAREN)){
            increment = parse_expression();
            if(increment is null) return null;
        }

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after for clauses");
        if(ERROR_OCCURRED) return null;

        CStmt* body = parse_statement();
        if(body is null) return null;

        return CForStmt.make(allocator, init, condition, increment, body, keyword);
    }

    // (6.8.3) compound-statement:
    //     { block-item-list_opt }
    CStmt* parse_block(){
        CToken brace = previous();
        auto statements = make_barray!(CStmt*)(allocator);

        while(!check(CTokenType.RIGHT_BRACE) && !at_end){
            CStmt* stmt = parse_statement();
            if(stmt is null) return null;
            statements ~= stmt;
        }

        consume(CTokenType.RIGHT_BRACE, "Expected '}' after block");
        if(ERROR_OCCURRED) return null;

        return CBlock.make(allocator, statements[], brace);
    }

    // (6.8.7.1) break statement
    CStmt* parse_break(){
        CToken keyword = previous();
        consume(CTokenType.SEMICOLON, "Expected ';' after 'break'");
        if(ERROR_OCCURRED) return null;
        return CBreakStmt.make(allocator, keyword);
    }

    // (6.8.7.1) continue statement
    CStmt* parse_continue(){
        CToken keyword = previous();
        consume(CTokenType.SEMICOLON, "Expected ';' after 'continue'");
        if(ERROR_OCCURRED) return null;
        return CContinueStmt.make(allocator, keyword);
    }

    // (6.8.5.1) switch statement:
    //     switch( expression ) statement
    // The body can contain case/default labels at any nesting level (Duff's device)
    CStmt* parse_switch(){
        CToken keyword = previous();

        consume(CTokenType.LEFT_PAREN, "Expected '(' after 'switch'");
        if(ERROR_OCCURRED) return null;

        CExpr* condition = parse_expression();
        if(condition is null) return null;

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after switch condition");
        if(ERROR_OCCURRED) return null;

        // Track that we're inside a switch so case/default are allowed
        switch_depth++;
        scope(exit) switch_depth--;

        // Parse the body - usually a block, but can be any statement
        CStmt* body_ = parse_statement();
        if(body_ is null) return null;

        return CSwitchStmt.make(allocator, condition, body_, keyword);
    }

    // (6.8.1) case labeled-statement:
    //     case constant-expression : statement
    CStmt* parse_case_label(){
        CToken keyword = previous();

        CExpr* case_val = parse_expression();
        if(case_val is null) return null;

        consume(CTokenType.COLON, "Expected ':' after case value");
        if(ERROR_OCCURRED) return null;

        // Parse the labeled statement
        CStmt* stmt = parse_statement();
        if(stmt is null) return null;

        return CCaseLabelStmt.make(allocator, case_val, stmt, false, keyword);
    }

    // (6.8.1) default labeled-statement:
    //     default : statement
    CStmt* parse_default_label(){
        CToken keyword = previous();

        consume(CTokenType.COLON, "Expected ':' after 'default'");
        if(ERROR_OCCURRED) return null;

        // Parse the labeled statement
        CStmt* stmt = parse_statement();
        if(stmt is null) return null;

        return CCaseLabelStmt.make(allocator, null, stmt, true, keyword);
    }

    // (6.8.7.1) goto statement:
    //     goto identifier ;
    CStmt* parse_goto(){
        CToken keyword = previous();

        CToken label = consume(CTokenType.IDENTIFIER, "Expected label after 'goto'");
        if(ERROR_OCCURRED) return null;

        consume(CTokenType.SEMICOLON, "Expected ';' after goto label");
        if(ERROR_OCCURRED) return null;

        return CGotoStmt.make(allocator, label, keyword);
    }

    // dasm { ... } - inline assembly block
    CStmt* parse_dasm(){
        CToken keyword = previous();

        consume(CTokenType.LEFT_BRACE, "Expected '{' after 'dasm'");
        if(ERROR_OCCURRED) return null;

        // Collect tokens until matching '}'
        StringBuilder sb;
        sb.allocator = allocator;

        int last_line = peek().line;
        int depth = 1;
        CTokenType prev_type = CTokenType.EOF;

        while(depth > 0 && !at_end){
            CToken tok = peek();

            if(tok.type == CTokenType.LEFT_BRACE){
                depth++;
                sb.write('{');
                prev_type = tok.type;
                advance();
            } else if(tok.type == CTokenType.RIGHT_BRACE){
                depth--;
                if(depth > 0){
                    sb.write('}');
                    prev_type = tok.type;
                    advance();
                }
                // Don't advance past final '}' yet
            } else {
                // Check if we've moved to a new line
                if(tok.line > last_line){
                    sb.write('\n');
                    last_line = tok.line;
                } else if(sb.cursor > 0){
                    // Add space between tokens, but not around dots
                    if(tok.type != CTokenType.DOT && prev_type != CTokenType.DOT){
                        sb.write(' ');
                    }
                }
                sb.write(tok.lexeme);
                prev_type = tok.type;
                advance();
            }
        }

        consume(CTokenType.RIGHT_BRACE, "Expected '}' to close dasm block");
        if(ERROR_OCCURRED) return null;

        return CDasmStmt.make(allocator, sb.borrow(), keyword);
    }

    // GCC inline assembly: __asm__ [volatile] ( "template" [: outputs [: inputs [: clobbers]]] );
    CStmt* parse_asm(){
        CToken keyword = previous();

        // Skip optional 'volatile' or '__volatile__'
        if(check(CTokenType.VOLATILE) ||
           (check(CTokenType.IDENTIFIER) && (peek().lexeme == "__volatile__" || peek().lexeme == "volatile"))){
            advance();
        }

        consume(CTokenType.LEFT_PAREN, "Expected '(' after asm");
        if(ERROR_OCCURRED) return null;

        // Skip everything inside the parentheses (handles nested parens)
        int depth = 1;
        while(depth > 0 && !at_end){
            if(check(CTokenType.LEFT_PAREN)){
                depth++;
            } else if(check(CTokenType.RIGHT_PAREN)){
                depth--;
                if(depth == 0) break;
            }
            advance();
        }

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after asm");
        if(ERROR_OCCURRED) return null;

        consume(CTokenType.SEMICOLON, "Expected ';' after asm statement");
        if(ERROR_OCCURRED) return null;

        return CAsmStmt.make(allocator, keyword);
    }

    // (6.8.2) labeled-statement:
    //     identifier : statement
    CStmt* parse_label(){
        CToken label = advance();  // consume identifier
        advance();  // consume ':'

        CStmt* stmt = parse_statement();
        if(stmt is null) return null;

        return CLabelStmt.make(allocator, label, stmt, label);
    }

    // (6.7.1) declaration:
    //     declaration-specifiers init-declarator-list_opt ;
    // (6.7.1) init-declarator-list:
    //     init-declarator
    //     init-declarator-list , init-declarator
    CStmt* parse_var_decl(){
        CToken type_tok = peek();
        CType* base_type = parse_base_type();
        if(base_type is null) return null;

        // Handle type-only declarations (e.g., "enum Foo { A, B };")
        if(match(CTokenType.SEMICOLON)){
            return CEmptyStmt.get();
        }

        auto decls = make_barray!(CStmt*)(allocator);

        do {
            // Parse pointer modifiers for this declarator
            CType* var_type = parse_pointer_modifiers(base_type);

            CToken name = consume(CTokenType.IDENTIFIER, "Expected variable name");
            if(ERROR_OCCURRED) return null;

            // Check for array declaration: int arr[2][3] etc.
            // Collect dimensions and apply in reverse order (rightmost is innermost)
            size_t[8] dims;
            size_t num_dims = 0;
            while(match(CTokenType.LEFT_BRACKET)){
                auto size_result = parse_enum_const_expr();
                if(size_result.err) return null;
                if(size_result.value <= 0){
                    error("Array size must be positive");
                    return null;
                }

                consume(CTokenType.RIGHT_BRACKET, "Expected ']' after array size");
                if(ERROR_OCCURRED) return null;

                if(num_dims < 8){
                    dims[num_dims++] = cast(size_t) size_result.value;
                }
            }
            // Apply dimensions in reverse order
            for(size_t i = num_dims; i > 0; i--){
                var_type = make_array_type(allocator, var_type, dims[i - 1]);
            }

            CExpr* initializer = null;
            if(match(CTokenType.EQUAL)){
                initializer = parse_initializer();
                if(initializer is null) return null;
            }

            decls ~= CVarDecl.make(allocator, var_type, name, initializer, type_tok);

        } while(match(CTokenType.COMMA));

        consume(CTokenType.SEMICOLON, "Expected ';' after variable declaration");
        if(ERROR_OCCURRED) return null;

        // If only one declaration, return it directly
        if(decls[].length == 1){
            return decls[0];
        }

        // Multiple declarations - wrap in a block
        return CBlock.make(allocator, decls[], type_tok);
    }

    // (6.7.11) initializer:
    //     assignment-expression
    //     { initializer-list }
    //     { initializer-list , }
    CExpr* parse_initializer(){
        if(check(CTokenType.LEFT_BRACE)){
            return parse_init_list();
        }
        return parse_assignment();
    }

    // (6.7.11) designation:
    //     designator-list =
    // (6.7.11) designator:
    //     [ constant-expression ]
    //     . identifier
    // Returns empty slice if no designation present
    CDesignator[] parse_designation(){
        auto designators = make_barray!CDesignator(allocator);

        while(true){
            if(check(CTokenType.DOT) && peek_at(1).type == CTokenType.IDENTIFIER){
                // Field designator: .identifier
                advance();  // consume '.'
                CToken name = advance();  // consume identifier
                CDesignator d;
                d.kind = CDesignatorKind.FIELD;
                d.field_name = name.lexeme;
                d.token = name;
                designators ~= d;
            } else if(check(CTokenType.LEFT_BRACKET)){
                // Array designator: [constant-expression]
                CToken bracket = advance();  // consume '['
                auto result = parse_enum_const_expr();
                if(result.err) return null;
                consume(CTokenType.RIGHT_BRACKET, "Expected ']' after designator index");
                if(ERROR_OCCURRED) return null;

                CDesignator d;
                d.kind = CDesignatorKind.INDEX;
                d.index_value = result.value;
                d.token = bracket;
                designators ~= d;
            } else {
                break;
            }
        }

        // If we parsed designators, expect '='
        if(designators[].length > 0){
            consume(CTokenType.EQUAL, "Expected '=' after designator");
            if(ERROR_OCCURRED) return null;
        }

        return designators[];
    }

    // (6.7.11) braced-initializer:
    //     { }
    //     { initializer-list }
    //     { initializer-list , }
    // (6.7.11) initializer-list:
    //     designation_opt initializer
    //     initializer-list , designation_opt initializer
    CExpr* parse_init_list(){
        CToken brace = advance();  // consume '{'
        auto elements = make_barray!CInitElement(allocator);

        // Handle empty initializer {}
        if(!check(CTokenType.RIGHT_BRACE)){
            do {
                CInitElement elem;

                // Parse optional designation
                elem.designators = parse_designation();
                if(ERROR_OCCURRED) return null;

                // Parse initializer (recursive for nested braces)
                elem.value = parse_initializer();
                if(elem.value is null) return null;

                elements ~= elem;
            } while(match(CTokenType.COMMA) && !check(CTokenType.RIGHT_BRACE));
        }

        consume(CTokenType.RIGHT_BRACE, "Expected '}' after initializer list");
        if(ERROR_OCCURRED) return null;

        return CInitList.make(allocator, elements[], brace);
    }

    // (6.8.4) expression-statement:
    //     expression_opt ;
    CStmt* parse_expr_stmt(){
        CToken tok = peek();
        CExpr* expr = parse_expression();
        if(expr is null) return null;

        consume(CTokenType.SEMICOLON, "Expected ';' after expression");
        if(ERROR_OCCURRED) return null;

        return CExprStmt.make(allocator, expr, tok);
    }

    // =========================================================================
    // Expression Parsing (Pratt/precedence climbing)
    // =========================================================================

    // (6.5.18) expression:
    //     assignment-expression
    //     expression , assignment-expression
    CExpr* parse_expression(){
        CExpr* expr = parse_assignment();
        if(expr is null) return null;

        while(check(CTokenType.COMMA)){
            CToken op = advance();
            CExpr* right = parse_assignment();
            if(right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    // (6.5.17.1) assignment-expression:
    //     conditional-expression
    //     unary-expression assignment-operator assignment-expression
    // assignment-operator: = *= /= %= += -= <<= >>= &= ^= |=
    CExpr* parse_assignment(){
        CExpr* expr = parse_ternary();
        if(expr is null) return null;

        if(check(CTokenType.EQUAL) || check(CTokenType.PLUS_EQUAL) ||
            check(CTokenType.MINUS_EQUAL) || check(CTokenType.STAR_EQUAL) ||
            check(CTokenType.SLASH_EQUAL) || check(CTokenType.PERCENT_EQUAL) ||
            check(CTokenType.AMP_EQUAL) || check(CTokenType.PIPE_EQUAL) ||
            check(CTokenType.CARET_EQUAL) || check(CTokenType.LESS_LESS_EQUAL) ||
            check(CTokenType.GREATER_GREATER_EQUAL)){
            CToken op_tok = advance();
            CExpr* value = parse_assignment();  // Right associative
            if(value is null) return null;
            return CAssign.make(allocator, expr, op_tok.type, value, op_tok);
        }

        return expr;
    }

    // (6.5.16) conditional-expression:
    //     logical-OR-expression
    //     logical-OR-expression ? expression : conditional-expression
    CExpr* parse_ternary(){
        CExpr* condition = parse_logical_or();
        if(condition is null) return null;

        if(!check(CTokenType.QUESTION)){
            return condition;
        }

        CToken question_tok = advance();  // consume '?'

        // After '?', parse full expression (allows comma operator)
        CExpr* if_true = parse_expression();
        if(if_true is null) return null;

        if(!check(CTokenType.COLON)){
            error("Expected ':' in ternary expression");
            return null;
        }
        advance();  // consume ':'

        // After ':', parse conditional-expression (right-associative)
        CExpr* if_false = parse_ternary();
        if(if_false is null) return null;

        return CTernary.make(allocator, condition, if_true, if_false, question_tok);
    }

    // (6.5.15) logical-OR-expression:
    //     logical-AND-expression
    //     logical-OR-expression || logical-AND-expression
    CExpr* parse_logical_or(){
        CExpr* expr = parse_logical_and();
        if(expr is null) return null;

        while(check(CTokenType.PIPE_PIPE)){
            CToken op = advance();
            CExpr* right = parse_logical_and();
            if(right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    // (6.5.14) logical-AND-expression:
    //     inclusive-OR-expression
    //     logical-AND-expression && inclusive-OR-expression
    CExpr* parse_logical_and(){
        CExpr* expr = parse_bitwise_or();
        if(expr is null) return null;

        while(check(CTokenType.AMP_AMP)){
            CToken op = advance();
            CExpr* right = parse_bitwise_or();
            if(right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    // (6.5.13) inclusive-OR-expression:
    //     exclusive-OR-expression
    //     inclusive-OR-expression | exclusive-OR-expression
    CExpr* parse_bitwise_or(){
        CExpr* expr = parse_bitwise_xor();
        if(expr is null) return null;

        while(check(CTokenType.PIPE) && !check_next(CTokenType.PIPE)){
            CToken op = advance();
            CExpr* right = parse_bitwise_xor();
            if(right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    // (6.5.12) exclusive-OR-expression:
    //     AND-expression
    //     exclusive-OR-expression ^ AND-expression
    CExpr* parse_bitwise_xor(){
        CExpr* expr = parse_bitwise_and();
        if(expr is null) return null;

        while(check(CTokenType.CARET)){
            CToken op = advance();
            CExpr* right = parse_bitwise_and();
            if(right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    // (6.5.11) AND-expression:
    //     equality-expression
    //     AND-expression & equality-expression
    CExpr* parse_bitwise_and(){
        CExpr* expr = parse_equality();
        if(expr is null) return null;

        while(check(CTokenType.AMP) && !check_next(CTokenType.AMP)){
            CToken op = advance();
            CExpr* right = parse_equality();
            if(right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    // (6.5.10) equality-expression:
    //     relational-expression
    //     equality-expression == relational-expression
    //     equality-expression != relational-expression
    CExpr* parse_equality(){
        CExpr* expr = parse_comparison();
        if(expr is null) return null;

        while(check(CTokenType.EQUAL_EQUAL) || check(CTokenType.BANG_EQUAL)){
            CToken op = advance();
            CExpr* right = parse_comparison();
            if(right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    // (6.5.9) relational-expression:
    //     shift-expression
    //     relational-expression < shift-expression
    //     relational-expression > shift-expression
    //     relational-expression <= shift-expression
    //     relational-expression >= shift-expression
    CExpr* parse_comparison(){
        CExpr* expr = parse_shift();
        if(expr is null) return null;

        while(check(CTokenType.LESS) || check(CTokenType.LESS_EQUAL) ||
               check(CTokenType.GREATER) || check(CTokenType.GREATER_EQUAL)){
            CToken op = advance();
            CExpr* right = parse_shift();
            if(right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    // (6.5.8) shift-expression:
    //     additive-expression
    //     shift-expression << additive-expression
    //     shift-expression >> additive-expression
    CExpr* parse_shift(){
        CExpr* expr = parse_additive();
        if(expr is null) return null;

        while(check(CTokenType.LESS_LESS) || check(CTokenType.GREATER_GREATER)){
            CToken op = advance();
            CExpr* right = parse_additive();
            if(right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    // (6.5.7) additive-expression:
    //     multiplicative-expression
    //     additive-expression + multiplicative-expression
    //     additive-expression - multiplicative-expression
    CExpr* parse_additive(){
        CExpr* expr = parse_multiplicative();
        if(expr is null) return null;

        while(check(CTokenType.PLUS) || check(CTokenType.MINUS)){
            CToken op = advance();
            CExpr* right = parse_multiplicative();
            if(right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    // (6.5.6) multiplicative-expression:
    //     cast-expression
    //     multiplicative-expression * cast-expression
    //     multiplicative-expression / cast-expression
    //     multiplicative-expression % cast-expression
    CExpr* parse_multiplicative(){
        CExpr* expr = parse_unary();
        if(expr is null) return null;

        while(check(CTokenType.STAR) || check(CTokenType.SLASH) || check(CTokenType.PERCENT)){
            CToken op = advance();
            CExpr* right = parse_unary();
            if(right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    // (6.5.4.1) unary-expression:
    //     postfix-expression
    //     ++ unary-expression
    //     -- unary-expression
    //     unary-operator cast-expression
    //     sizeof unary-expression
    //     sizeof ( type-name )
    //     alignof ( type-name )
    //     _Countof unary-expression        (NOT IMPLEMENTED)
    //     _Countof ( type-name )           (NOT IMPLEMENTED)
    //
    // (6.5.4.1) unary-operator: one of
    //     & * + - ~ !
    CExpr* parse_unary(){
        // Prefix operators
        if(check(CTokenType.BANG) || check(CTokenType.MINUS) ||
            check(CTokenType.TILDE) || check(CTokenType.STAR) ||
            check(CTokenType.AMP) || check(CTokenType.PLUS_PLUS) ||
            check(CTokenType.MINUS_MINUS) || check(CTokenType.PLUS)){
            CToken op = advance();
            CExpr* operand = parse_unary();
            if(operand is null) return null;
            return CUnary.make(allocator, op.type, operand, true, op);
        }

        // sizeof operator: sizeof(type) or sizeof expr
        if(match(CTokenType.SIZEOF)){
            CToken op = previous();

            // Check for sizeof(type) - requires parens
            if(check(CTokenType.LEFT_PAREN)){
                // Peek ahead to see if it's a type
                if(peek_at(1).type == CTokenType.VOID ||
                    peek_at(1).type == CTokenType.CHAR ||
                    peek_at(1).type == CTokenType.SHORT ||
                    peek_at(1).type == CTokenType.INT ||
                    peek_at(1).type == CTokenType.LONG ||
                    peek_at(1).type == CTokenType.FLOAT ||
                    peek_at(1).type == CTokenType.DOUBLE ||
                    peek_at(1).type == CTokenType.UNSIGNED ||
                    peek_at(1).type == CTokenType.SIGNED ||
                    peek_at(1).type == CTokenType.STRUCT ||
                    peek_at(1).type == CTokenType.UNION ||
                    peek_at(1).type == CTokenType.ENUM ||
                    (peek_at(1).type == CTokenType.IDENTIFIER &&
                     (peek_at(1).lexeme in typedef_types) !is null)){
                    // sizeof(type) - use parse_type_name for array types like sizeof(int[10])
                    advance();  // consume '('
                    CType* type = parse_type_name();
                    if(type is null) return null;
                    consume(CTokenType.RIGHT_PAREN, "Expected ')' after type");
                    if(ERROR_OCCURRED) return null;
                    size_t size = type.size_of();
                    return CSizeof.make(allocator, type, size, op);
                }
            }

            // sizeof expr (unary operator on expression)
            CExpr* expr = parse_unary();
            if(expr is null) return null;
            return CSizeof.make_expr(allocator, expr, op);
        }

        // _Alignof operator: _Alignof(type) or _Alignof(expr) (GNU extension)
        if(match(CTokenType.ALIGNOF)){
            CToken op = previous();

            // _Alignof always requires parens
            consume(CTokenType.LEFT_PAREN, "Expected '(' after _Alignof");
            if(ERROR_OCCURRED) return null;

            // Check if it's _Alignof(type) or _Alignof(expr)
            if(peek().type == CTokenType.VOID ||
                peek().type == CTokenType.CHAR ||
                peek().type == CTokenType.SHORT ||
                peek().type == CTokenType.INT ||
                peek().type == CTokenType.LONG ||
                peek().type == CTokenType.FLOAT ||
                peek().type == CTokenType.DOUBLE ||
                peek().type == CTokenType.UNSIGNED ||
                peek().type == CTokenType.SIGNED ||
                peek().type == CTokenType.STRUCT ||
                peek().type == CTokenType.UNION ||
                peek().type == CTokenType.ENUM ||
                (peek().type == CTokenType.IDENTIFIER &&
                 (peek().lexeme in typedef_types) !is null)){
                // _Alignof(type) - use parse_type_name for array types
                CType* type = parse_type_name();
                if(type is null) return null;
                consume(CTokenType.RIGHT_PAREN, "Expected ')' after type");
                if(ERROR_OCCURRED) return null;
                size_t alignment = type.align_of();
                return CAlignof.make(allocator, type, alignment, op);
            } else {
                // _Alignof(expr) - GNU extension
                CExpr* expr = parse_expression();
                if(expr is null) return null;
                consume(CTokenType.RIGHT_PAREN, "Expected ')' after expression");
                if(ERROR_OCCURRED) return null;
                return CAlignof.make_expr(allocator, expr, op);
            }
        }

        // _Countof operator: _Countof(type) or _Countof expr
        if(match(CTokenType.COUNTOF)){
            CToken op = previous();

            // Check for _Countof(type) - requires parens
            if(check(CTokenType.LEFT_PAREN)){
                // Peek ahead to see if it's a type
                if(peek_at(1).type == CTokenType.VOID ||
                    peek_at(1).type == CTokenType.CHAR ||
                    peek_at(1).type == CTokenType.SHORT ||
                    peek_at(1).type == CTokenType.INT ||
                    peek_at(1).type == CTokenType.LONG ||
                    peek_at(1).type == CTokenType.FLOAT ||
                    peek_at(1).type == CTokenType.DOUBLE ||
                    peek_at(1).type == CTokenType.UNSIGNED ||
                    peek_at(1).type == CTokenType.SIGNED ||
                    peek_at(1).type == CTokenType.STRUCT ||
                    peek_at(1).type == CTokenType.UNION ||
                    peek_at(1).type == CTokenType.ENUM ||
                    (peek_at(1).type == CTokenType.IDENTIFIER &&
                     (peek_at(1).lexeme in typedef_types) !is null)){
                    // _Countof(type) - type must be an array type
                    advance();  // consume '('
                    CType* type = parse_type_name();
                    if(type is null) return null;
                    consume(CTokenType.RIGHT_PAREN, "Expected ')' after type");
                    if(ERROR_OCCURRED) return null;

                    if(!type.is_array()){
                        error("_Countof requires an array type");
                        return null;
                    }
                    return CCountof.make(allocator, type, type.array_size, op);
                }
            }

            // _Countof expr (unary operator on expression)
            CExpr* expr = parse_unary();
            if(expr is null) return null;
            // Count will be computed during codegen based on expression's type
            return CCountof.make_expr(allocator, expr, 0, op);
        }

        return parse_postfix();
    }

    // (6.5.3.1) postfix-expression:
    //     primary-expression
    //     postfix-expression [ expression ]
    //     postfix-expression ( argument-expression-list_opt )
    //     postfix-expression . identifier
    //     postfix-expression -> identifier
    //     postfix-expression ++
    //     postfix-expression --
    //     compound-literal
    //
    // (6.5.3.6) compound-literal:  (NOT IMPLEMENTED)
    //     ( storage-class-specifiers_opt type-name ) braced-initializer
    CExpr* parse_postfix(){
        CExpr* expr = parse_primary();
        if(expr is null) return null;

        while(true){
            if(match(CTokenType.LEFT_PAREN)){
                // Function call
                expr = finish_call(expr);
                if(expr is null) return null;
            } else if(match(CTokenType.LEFT_BRACKET)){
                // Array subscript
                CToken bracket = previous();
                CExpr* index = parse_expression();
                if(index is null) return null;
                consume(CTokenType.RIGHT_BRACKET, "Expected ']' after subscript");
                if(ERROR_OCCURRED) return null;
                expr = CSubscript.make(allocator, expr, index, bracket);
            } else if(match(CTokenType.DOT)){
                // Member access: expr.member
                CToken dot = previous();
                CToken member = consume(CTokenType.IDENTIFIER, "Expected member name after '.'");
                if(ERROR_OCCURRED) return null;
                expr = CMemberAccess.make(allocator, expr, member, false, dot);
            } else if(match(CTokenType.ARROW)){
                // Pointer member access: expr->member
                CToken arrow = previous();
                CToken member = consume(CTokenType.IDENTIFIER, "Expected member name after '->'");
                if(ERROR_OCCURRED) return null;
                expr = CMemberAccess.make(allocator, expr, member, true, arrow);
            } else if(match(CTokenType.PLUS_PLUS) || match(CTokenType.MINUS_MINUS)){
                // Postfix increment/decrement
                CToken op = previous();
                expr = CUnary.make(allocator, op.type, expr, false, op);
            } else {
                break;
            }
        }

        return expr;
    }

    // (6.5.3.1) argument-expression-list:
    //     assignment-expression
    //     argument-expression-list , assignment-expression
    CExpr* finish_call(CExpr* callee){
        CToken paren = previous();
        auto args = make_barray!(CExpr*)(allocator);

        if(!check(CTokenType.RIGHT_PAREN)){
            do {
                // Use parse_assignment, not parse_expression, to avoid comma operator
                CExpr* arg = parse_assignment();
                if(arg is null) return null;
                args ~= arg;
            } while(match(CTokenType.COMMA));
        }

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after arguments");
        if(ERROR_OCCURRED) return null;

        return CCall.make(allocator, callee, args[], paren);
    }

    // (6.5.2) primary-expression:
    //     identifier
    //     constant
    //     string-literal
    //     ( expression )
    //     generic-selection  (NOT IMPLEMENTED)
    CExpr* parse_primary(){
        CToken tok = peek();

        if(match(CTokenType.NUMBER) || match(CTokenType.HEX)){
            return CLiteral.make(allocator, previous(), &TYPE_INT);
        }

        if(match(CTokenType.STRING)){
            // String literal is a char*
            return CLiteral.make(allocator, previous(), make_pointer_type(allocator, &TYPE_CHAR));
        }

        if(match(CTokenType.CHAR_LITERAL)){
            return CLiteral.make(allocator, previous(), &TYPE_CHAR);
        }

        // C23 true/false keywords
        if(match(CTokenType.TRUE_KW)){
            return CLiteral.make_int(allocator, 1, previous());
        }
        if(match(CTokenType.FALSE_KW)){
            return CLiteral.make_int(allocator, 0, previous());
        }

        // C11 _Generic selection
        if(match(CTokenType.GENERIC)){
            CToken gen_tok = previous();
            consume(CTokenType.LEFT_PAREN, "Expected '(' after _Generic");
            if(ERROR_OCCURRED) return null;

            CExpr* ctrl = parse_assignment();
            if(ctrl is null) return null;
            consume(CTokenType.COMMA, "Expected ',' after controlling expression");
            if(ERROR_OCCURRED) return null;

            auto assocs = make_barray!CGenericAssoc(allocator);

            while(!check(CTokenType.RIGHT_PAREN) && !at_end){
                CGenericAssoc assoc;
                if(match(CTokenType.DEFAULT)){
                    assoc.type = null;  // default case
                } else {
                    assoc.type = parse_type();
                    if(assoc.type is null) return null;
                }
                consume(CTokenType.COLON, "Expected ':' in generic association");
                if(ERROR_OCCURRED) return null;
                assoc.result = parse_assignment();
                if(assoc.result is null) return null;
                assocs ~= assoc;

                if(!match(CTokenType.COMMA)) break;
            }
            consume(CTokenType.RIGHT_PAREN, "Expected ')' after _Generic");
            if(ERROR_OCCURRED) return null;
            return CGeneric.make(allocator, ctrl, assocs[], gen_tok);
        }

        if(check(CTokenType.IDENTIFIER)){
            // Check for __builtin_va_arg(ap, type)
            if(peek().lexeme == "__builtin_va_arg"){
                CToken va_tok = advance();
                consume(CTokenType.LEFT_PAREN, "Expected '(' after __builtin_va_arg");
                if(ERROR_OCCURRED) return null;
                CExpr* va_list_expr = parse_assignment();
                if(va_list_expr is null) return null;
                consume(CTokenType.COMMA, "Expected ',' after va_list in __builtin_va_arg");
                if(ERROR_OCCURRED) return null;
                CType* arg_type = parse_type();
                if(arg_type is null) return null;
                consume(CTokenType.RIGHT_PAREN, "Expected ')' after type in __builtin_va_arg");
                if(ERROR_OCCURRED) return null;
                return CVaArg.make(allocator, va_list_expr, arg_type, va_tok);
            }
            // __builtin_va_start and __builtin_va_end - just parse and ignore
            if(peek().lexeme == "__builtin_va_start" || peek().lexeme == "__builtin_va_end"){
                CToken va_tok = advance();
                consume(CTokenType.LEFT_PAREN, "Expected '('");
                if(ERROR_OCCURRED) return null;
                // Skip arguments
                int depth = 1;
                while(depth > 0 && !at_end){
                    if(match(CTokenType.LEFT_PAREN)) depth++;
                    else if(match(CTokenType.RIGHT_PAREN)) depth--;
                    else advance();
                }
                // Return a dummy literal 0
                return CLiteral.make_int(allocator, 0, va_tok);
            }
            // __builtin_offsetof(type, member) - compute offset of member in type
            if(peek().lexeme == "__builtin_offsetof"){
                CToken off_tok = advance();
                consume(CTokenType.LEFT_PAREN, "Expected '(' after __builtin_offsetof");
                if(ERROR_OCCURRED) return null;
                // Parse type (can be struct X, union Y, or typedef name)
                CType* type_ = parse_type();
                if(type_ is null) return null;
                consume(CTokenType.COMMA, "Expected ',' after type in __builtin_offsetof");
                if(ERROR_OCCURRED) return null;
                // Parse member name (can be nested: member1.member2)
                CToken member = consume(CTokenType.IDENTIFIER, "Expected member name");
                if(ERROR_OCCURRED) return null;
                // Look up the field offset in the type
                size_t offset = 0;
                CType* current_type = type_;
                bool found = false;
                // Handle nested member access (e.g., __value.__wch)
                while(true){
                    if(current_type.kind != CTypeKind.STRUCT && current_type.kind != CTypeKind.UNION){
                        error(member, "offsetof requires struct or union type");
                        return null;
                    }
                    foreach(ref f; current_type.fields){
                        if(f.name == member.lexeme){
                            offset += f.offset;
                            current_type = f.type;
                            found = true;
                            break;
                        }
                    }
                    if(!found){
                        error(member, "Unknown field in offsetof");
                        return null;
                    }
                    if(match(CTokenType.DOT)){
                        member = consume(CTokenType.IDENTIFIER, "Expected member name after '.'");
                        if(ERROR_OCCURRED) return null;
                        found = false;
                    } else {
                        break;
                    }
                }
                consume(CTokenType.RIGHT_PAREN, "Expected ')' after __builtin_offsetof");
                if(ERROR_OCCURRED) return null;
                return CLiteral.make_int(allocator, cast(long)offset, off_tok);
            }
            // __embed("path", offset, length) - emitted by preprocessor for #embed
            if(peek().lexeme == "__embed"){
                CToken embed_tok = advance();
                consume(CTokenType.LEFT_PAREN, "Expected '(' after __embed");
                if(ERROR_OCCURRED) return null;

                // Parse path string
                if(!check(CTokenType.STRING)){
                    error("Expected string literal for __embed path");
                    return null;
                }
                str path = advance().lexeme;
                // Strip quotes from path
                if(path.length >= 2 && path[0] == '"' && path[$-1] == '"')
                    path = path[1..$-1];

                consume(CTokenType.COMMA, "Expected ',' after __embed path");
                if(ERROR_OCCURRED) return null;

                // Parse offset
                if(!check(CTokenType.NUMBER)){
                    error("Expected number for __embed offset");
                    return null;
                }
                import core.stdc.stdlib : strtol;
                long offset = strtol(advance().lexeme.ptr, null, 10);

                consume(CTokenType.COMMA, "Expected ',' after __embed offset");
                if(ERROR_OCCURRED) return null;

                // Parse length
                if(!check(CTokenType.NUMBER)){
                    error("Expected number for __embed length");
                    return null;
                }
                long length = strtol(advance().lexeme.ptr, null, 10);

                consume(CTokenType.RIGHT_PAREN, "Expected ')' after __embed");
                if(ERROR_OCCURRED) return null;

                return CEmbed.make(allocator, path, offset, length, embed_tok);
            }
            return CIdentifier.make(allocator, advance());
        }

        if(match(CTokenType.LEFT_PAREN)){
            // Check if this is a cast expression: (type)value or compound literal: (type){...}
            if(is_type_specifier(peek())){
                // It's a cast or compound literal - use parse_type_name for full abstract declarators
                CType* cast_type = parse_type_name();
                if(cast_type is null) return null;
                consume(CTokenType.RIGHT_PAREN, "Expected ')' after cast type");
                if(ERROR_OCCURRED) return null;

                // Check for compound literal: (type){...}
                if(check(CTokenType.LEFT_BRACE)){
                    CExpr* init_expr = parse_initializer();
                    if(init_expr is null) return null;
                    return CCompoundLiteral.make(allocator, cast_type, init_expr, tok);
                }

                // Parse the value being cast
                CExpr* operand = parse_unary();
                if(operand is null) return null;
                return CCast.make(allocator, cast_type, operand, tok);
            }
            // Regular parenthesized expression
            CExpr* expr = parse_expression();
            if(expr is null) return null;
            consume(CTokenType.RIGHT_PAREN, "Expected ')' after expression");
            if(ERROR_OCCURRED) return null;
            return CGrouping.make(allocator, expr, tok);
        }

        error("Expected expression");
        return null;
    }

    // =========================================================================
    // Utility Methods
    // =========================================================================

    CToken consume(CTokenType type, str message){
        if(check(type)) return advance();
        error(peek(), message);
        return peek();
    }

    bool match(CTokenType type){
        if(check(type)){
            advance();
            return true;
        }
        return false;
    }

    bool check(CTokenType type){
        if(at_end) return false;
        return peek().type == type;
    }

    bool check_next(CTokenType type){
        if(current + 1 >= tokens.length) return false;
        return tokens[current + 1].type == type;
    }

    CToken advance(){
        if(!at_end) current++;
        return previous();
    }

    bool at_end(){
        return peek().type == CTokenType.EOF;
    }

    // Skip balanced parentheses including nested ones
    void skip_balanced_parens(){
        if(!check(CTokenType.LEFT_PAREN)) return;
        advance();  // consume '('
        int depth = 1;
        while(depth > 0 && !at_end){
            if(check(CTokenType.LEFT_PAREN)) depth++;
            else if(check(CTokenType.RIGHT_PAREN)) depth--;
            advance();
        }
    }

    void skip_balanced_braces(){
        if(!check(CTokenType.LEFT_BRACE)) return;
        advance();  // consume '{'
        int depth = 1;
        while(depth > 0 && !at_end){
            if(check(CTokenType.LEFT_BRACE)) depth++;
            else if(check(CTokenType.RIGHT_BRACE)) depth--;
            advance();
        }
    }

    CToken peek(){
        return tokens[current];
    }

    CToken peek_at(int offset){
        if(current + offset >= tokens.length)
            return tokens[$ - 1];  // Return EOF
        return tokens[current + offset];
    }

    CToken previous(){
        return tokens[current - 1];
    }
}
