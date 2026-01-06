/*
 * C Front-End Parser for ddasm
 * Copyright 2025, David Priver
 */
module cfront.c_parser;

import core.stdc.stdio : fprintf, stderr;
import dlib.aliases;
import dlib.allocator : Allocator;
import dlib.barray : Barray, make_barray;
import dlib.stringbuilder : StringBuilder, Q, mwritef;
import dlib.table : Table;
import dlib.str_util: startswith;

import cfront.c_pp_to_c : CToken, CTokenType;
import cfront.c_const_eval: try_eval_constant, ConstValue;
import cfront.c_ast;

// =============================================================================
// Scope Stack for proper lexical scoping (ISO C block scope)
// =============================================================================
struct Scope {
    Table!(str, CType*) variables;
    Table!(str, long) enum_constants;
    Table!(str, str) static_local_names;  // Original name -> mangled global name
    Table!(str, str) inner_function_names;  // Local name -> mangled global name
    Scope* parent;
}

// =============================================================================
// Declaration Specifiers (ISO/IEC 9899 Section 6.7.1)
// =============================================================================
// Accumulates all specifiers from parsing declaration-specifiers:
//   declaration-specifiers:
//       declaration-specifier attribute-specifier-sequence_opt
//       declaration-specifier declaration-specifiers
//   declaration-specifier:
//       storage-class-specifier | type-specifier-qualifier | function-specifier
struct DeclSpecifiers {
    // Storage class flags (grammar allows multiple, semantic constraint checks later)
    enum StorageClass : ubyte {
        NONE        = 0,
        TYPEDEF     = 1 << 0,
        EXTERN      = 1 << 1,
        STATIC      = 1 << 2,
        AUTO        = 1 << 3,
        REGISTER    = 1 << 4,
        THREAD_LOCAL = 1 << 5,
        CONSTEXPR   = 1 << 6,
    }
    StorageClass storage = StorageClass.NONE;

    // Function specifiers (can combine)
    bool is_inline;
    bool is_noreturn;

    // Type qualifiers (can combine, may appear before or after type-specifier)
    bool is_const;
    bool is_volatile;
    bool is_restrict;
    bool is_atomic;

    // Type specifier components (can appear in any order, combined at end)
    bool saw_signed;
    bool saw_unsigned;
    bool saw_char;
    bool saw_short;
    bool saw_int;
    bool saw_long;
    bool saw_long_long;  // second 'long' in 'long long'
    bool saw_float;
    bool saw_double;

    // The actual base type (struct X, typedef-name, etc.) - set directly for complex types
    CType* base_type;

    // Source location for error reporting
    CToken first_token;
}

struct CParser {
    Allocator allocator;
    CToken[] tokens;
    int current = 0;
    bool ERROR_OCCURRED = false;
    str current_library;  // Set by #pragma library("...")
    int switch_depth = 0;  // Track nesting level in switch statements for case/default
    bool debug_types = false;  // Print type info during parsing
    Table!(str, CType*) struct_types;  // Defined struct types
    Table!(str, CType*) union_types;   // Defined union types
    Table!(str, CType*) enum_types;    // Defined enum types
    Table!(str, long) enum_constants;  // Enum constant values (name -> value)
    Table!(str, CType*) typedef_types; // Typedef aliases (name -> type)
    Table!(str, CType*) global_var_types; // Global variable types (for sizeof in const exprs)

    // Type tracking for implicit casts
    Scope* current_scope;  // Current lexical scope (linked list to parent scopes)
    Table!(str, CType*) func_types;        // Function name -> function type (for setting expr types)
    Table!(str, size_t) func_info;         // Function name -> index in functions array
    // Track function names to indices for merging forward declarations with definitions
    Table!(str, size_t) func_indices;
    // Track global names to indices for merging extern decls with definitions
    Table!(str, size_t) global_indices;
    CType* current_return_type;            // Current function's return type (for return checking)
    str current_function_name;             // Current function name (for static local mangling)
    int static_local_counter;              // Counter for static locals in current function
    int lambda_counter;                     // Counter for anonymous function literals
    StringBuilder error_sb;
    Barray!CFunction functions;
    Barray!CGlobalVar globals;
    Barray!CStructDef structs;
    Barray!CUnionDef unions;
    Barray!CEnumDef enums;



    void errorf(A...)(CToken tok, A args){
        error_sb.FORMAT(tok.file, ':', tok.line, ':', tok.column, ": ParseError: ");
        foreach(a; args)
            error_sb.write(a);
        error_sb.write('\n');
        if(tok.expansion_file.length){
            error_sb.FORMAT("  note: expanded from macro used at ", tok.expansion_file, ':', tok.expansion_line, ':', tok.expansion_column, '\n');
        }
        str msg = error_sb.borrow();
        fprintf(stderr, "%.*s", cast(int)msg.length, msg.ptr);
        error_sb.reset();
    }
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
    // Scope Management (ISO C block scope)
    // =========================================================================

    // Push a new scope (on block entry)
    void push_scope(){
        void[] data = allocator.zalloc(Scope.sizeof);
        auto new_scope = cast(Scope*)data.ptr;
        new_scope.variables.data.allocator = allocator;
        new_scope.enum_constants.data.allocator = allocator;
        new_scope.static_local_names.data.allocator = allocator;
        new_scope.inner_function_names.data.allocator = allocator;
        new_scope.parent = current_scope;
        current_scope = new_scope;
    }

    // Pop scope (on block exit)
    void pop_scope(){
        if(current_scope !is null){
            current_scope = current_scope.parent;
        }
    }

    // Look up variable in scope chain
    CType* lookup_variable(str name){
        // Walk the scope chain from innermost to outermost
        for(auto scope_ = current_scope; scope_ !is null; scope_ = scope_.parent){
            if(auto t = name in scope_.variables)
                return *t;
        }
        // Fall back to globals
        if(auto t = name in global_var_types)
            return *t;
        return null;
    }

    // Look up variable in local scope only (not globals)
    CType* lookup_local_variable(str name){
        for(auto scope_ = current_scope; scope_ !is null; scope_ = scope_.parent){
            if(auto t = name in scope_.variables)
                return *t;
        }
        return null;
    }

    // Look up enum constant in scope chain
    long* lookup_enum_constant(str name){
        // Walk the scope chain from innermost to outermost
        for(auto scope_ = current_scope; scope_ !is null; scope_ = scope_.parent){
            if(auto val = name in scope_.enum_constants)
                return val;
        }
        // Fall back to global enum_constants
        if(auto val = name in enum_constants)
            return val;
        return null;
    }

    // Look up static local mangled name in scope chain
    str* lookup_static_local_name(str name){
        for(auto scope_ = current_scope; scope_ !is null; scope_ = scope_.parent){
            if(auto mangled = name in scope_.static_local_names)
                return mangled;
        }
        return null;
    }

    // Look up inner function mangled name in scope chain
    str* lookup_inner_function_name(str name){
        for(auto scope_ = current_scope; scope_ !is null; scope_ = scope_.parent){
            if(auto mangled = name in scope_.inner_function_names)
                return mangled;
        }
        return null;
    }

    // Try to look up a function by name (for UFCS)
    // Returns CIdentifier* for the function or null if not found
    CExpr* try_lookup_function(CToken name_tok){
        str name = name_tok.lexeme;

        // Check inner functions first
        if(auto mangled = lookup_inner_function_name(name)){
            if(auto t = *mangled in func_types){
                CToken mangled_tok = name_tok;
                mangled_tok.lexeme = *mangled;
                return CIdentifier.make_func(allocator, mangled_tok, *t);
            }
        }

        // Check regular/extern functions
        if(auto t = name in func_types){
            if(auto idx = name in func_info){
                CFunction* func = &functions[*idx];
                if(func.library.length > 0 && !func.is_definition)
                    return CIdentifier.make_extern_func(allocator, name_tok, *t, func.library);
            }
            return CIdentifier.make_func(allocator, name_tok, *t);
        }

        return null;
    }

    // Declare enum constant in current scope (or global if no scope)
    // Returns false if redeclaration error
    bool declare_enum_constant(str name, long value, CToken tok){
        if(current_scope !is null){
            // Check for redeclaration in same scope
            if(name in current_scope.enum_constants){
                errorf(tok, "redeclaration of enumerator '", name, "'");
                return false;
            }
            if(name in current_scope.variables){
                errorf(tok, "redeclaration of '", name, "' as enum constant");
                return false;
            }
            current_scope.enum_constants[name] = value;
        } else {
            // At file scope
            if(name in enum_constants){
                errorf(tok, "redeclaration of enumerator '", name, "'");
                return false;
            }
            if(name in global_var_types){
                errorf(tok, "redeclaration of '", name, "' as enum constant");
                return false;
            }
            enum_constants[name] = value;
        }
        return true;
    }

    // Check if variable is declared in current scope (for redeclaration errors)
    bool is_declared_in_current_scope(str name){
        if(current_scope is null) return false;
        return (name in current_scope.variables) !is null;
    }

    // Declare a variable in the current scope
    bool declare_variable(str name, CType* type, CToken tok){
        if(current_scope is null){
            // At global scope, use global_var_types
            // Check for collision with enum constant
            if(name in enum_constants){
                errorf(tok, "'", name, "' redeclared as different kind of symbol");
                return false;
            }
            global_var_types[name] = type;
            return true;
        }
        // Check for redeclaration in same scope
        if(name in current_scope.variables){
            errorf(tok, "redeclaration of '", name, "' in same scope");
            return false;
        }
        // Check for collision with enum constant in same scope
        if(name in current_scope.enum_constants){
            errorf(tok, "'", name, "' redeclared as different kind of symbol");
            return false;
        }
        current_scope.variables[name] = type;
        return true;
    }

    // Add or merge a global variable declaration/definition
    // C allows: extern int a; ... int a = 0; (definition overrides declaration)
    // Also: int a; ... int a = 1; (definition with init overrides tentative)
    void add_or_merge_global(CGlobalVar gvar){
        str name = gvar.name.lexeme;
        if(size_t* existing_idx = name in global_indices){
            // Merge: definition beats extern, initializer beats tentative
            CGlobalVar* prev = &globals[*existing_idx];
            if(gvar.is_extern && !prev.is_extern){
                // Keep the definition, ignore extern
            } else if(!gvar.is_extern && prev.is_extern){
                // Definition overrides extern declaration
                *prev = gvar;
            } else if(gvar.initializer !is null && prev.initializer is null){
                // Definition with initializer overrides tentative
                *prev = gvar;
            }
            // Otherwise keep existing (first definition wins)
        } else {
            global_indices[name] = globals.count;
            globals ~= gvar;
        }
        // Always update the type lookup
        global_var_types[name] = gvar.var_type;
    }

    // =========================================================================
    // Declaration Specifier Parsing (ISO C Section 6.7)
    // =========================================================================

    // (6.7.2) storage-class-specifier:
    //     typedef | extern | static | _Thread_local | auto | register
    bool parse_storage_class_specifier(DeclSpecifiers* specs){
        alias SC = DeclSpecifiers.StorageClass;
        if(match(CTokenType.TYPEDEF))   { specs.storage |= SC.TYPEDEF; return true; }
        if(match(CTokenType.EXTERN))    { specs.storage |= SC.EXTERN; return true; }
        if(match(CTokenType.STATIC))    { specs.storage |= SC.STATIC; return true; }
        if(match(CTokenType.AUTO))      { specs.storage |= SC.AUTO; return true; }
        if(match(CTokenType.REGISTER))  { specs.storage |= SC.REGISTER; return true; }
        if(match(CTokenType.CONSTEXPR)) { specs.storage |= SC.CONSTEXPR; return true; }
        if(match_id("_Thread_local"))   { specs.storage |= SC.THREAD_LOCAL; return true; }
        if(match_id("__auto_type"))     { specs.storage |= SC.AUTO; return true; }  // GCC extension
        return false;
    }

    // (6.7.5) function-specifier:
    //     inline | _Noreturn | noreturn
    bool parse_function_specifier(DeclSpecifiers* specs){
        if(match(CTokenType.INLINE))   { specs.is_inline = true; return true; }
        if(match(CTokenType.NORETURN)) { specs.is_noreturn = true; return true; }
        if(match_id("__forceinline"))  { specs.is_inline = true; return true; }
        return false;
    }

    // (6.7.4.1) type-qualifier:
    //     const | restrict | volatile | _Atomic
    // Note: _Atomic(T) is a type specifier, _Atomic alone is a qualifier
    bool parse_type_qualifier(DeclSpecifiers* specs){
        if(match(CTokenType.CONST))    { specs.is_const = true; return true; }
        if(match(CTokenType.VOLATILE)) { specs.is_volatile = true; return true; }
        if(match(CTokenType.RESTRICT)) { specs.is_restrict = true; return true; }
        // _Atomic without parens is a qualifier
        if(match(CTokenType.ATOMIC)){
            if(check(CTokenType.LEFT_PAREN)){
                // _Atomic(T) is a type specifier - parse the inner type
                advance();  // consume '('
                specs.base_type = parse_type();
                if(!match(CTokenType.RIGHT_PAREN)){
                    error("Expected ')' after _Atomic type");
                    return false;
                }
            } else {
                specs.is_atomic = true;
            }
            return true;
        }
        if(match_id("_Atomic"))        { specs.is_atomic = true; return true; }
        return false;
    }

    // Check if current token could start a type specifier
    bool is_type_specifier_token(){
        return is_type_specifier(peek());
    }

    // Check if we have any primitive type flags set
    bool has_primitive_type_flags(DeclSpecifiers* specs){
        return specs.saw_signed || specs.saw_unsigned || specs.saw_char ||
               specs.saw_short || specs.saw_int || specs.saw_long || specs.saw_long_long ||
               specs.saw_float || specs.saw_double;
    }

    // Finalize type specifiers: combine flags into base_type
    void finalize_type_specifiers(DeclSpecifiers* specs){
        if(specs.base_type !is null) return;  // Already set (struct, typedef, etc.)
        if(!has_primitive_type_flags(specs)) return;  // No primitive type

        bool is_unsigned = specs.saw_unsigned;

        // long double
        if(specs.saw_long && specs.saw_double){
            specs.base_type = &TYPE_LONG_DOUBLE;
            return;
        }
        // double
        if(specs.saw_double){
            specs.base_type = &TYPE_DOUBLE;
            return;
        }
        // float
        if(specs.saw_float){
            specs.base_type = &TYPE_FLOAT;
            return;
        }
        // char
        if(specs.saw_char){
            specs.base_type = is_unsigned ? &TYPE_UCHAR : &TYPE_CHAR;
            return;
        }
        // short (with optional int)
        if(specs.saw_short){
            void[] data = allocator.zalloc(CType.sizeof);
            CType* result = cast(CType*)data.ptr;
            result.kind = CTypeKind.SHORT;
            result.is_signed = !is_unsigned;
            specs.base_type = result;
            return;
        }
        // long long (with optional int)
        if(specs.saw_long_long){
            specs.base_type = is_unsigned ? &TYPE_ULLONG : &TYPE_LLONG;
            return;
        }
        // long (with optional int)
        if(specs.saw_long){
            specs.base_type = is_unsigned ? &TYPE_ULONG : &TYPE_LONG;
            return;
        }
        // int, signed, unsigned alone
        specs.base_type = is_unsigned ? &TYPE_UINT : &TYPE_INT;
    }

    // (6.7.3.1) type-specifier (simplified - sets flags, finalized later)
    // Returns true if a type specifier was consumed
    bool parse_type_specifier(DeclSpecifiers* specs){
        // Primitive type flags - can appear in any order
        if(match(CTokenType.SIGNED))   { specs.saw_signed = true; return true; }
        if(match(CTokenType.UNSIGNED)) { specs.saw_unsigned = true; return true; }
        if(match(CTokenType.CHAR))     { specs.saw_char = true; return true; }
        if(match(CTokenType.SHORT))    { specs.saw_short = true; return true; }
        if(match(CTokenType.INT))      { specs.saw_int = true; return true; }
        if(match(CTokenType.LONG)){
            if(specs.saw_long) specs.saw_long_long = true;
            else specs.saw_long = true;
            return true;
        }
        if(match(CTokenType.FLOAT))    { specs.saw_float = true; return true; }
        if(match(CTokenType.DOUBLE))   { specs.saw_double = true; return true; }
        if(match(CTokenType.BOOL))     { specs.saw_int = true; return true; }  // _Bool as int
        if(match(CTokenType.FLOAT16))  { specs.saw_float = true; return true; }  // _Float16 → float stub
        if(match(CTokenType.FLOAT32))  { specs.saw_float = true; return true; }  // _Float32 → float
        if(match(CTokenType.FLOAT64))  { specs.saw_double = true; return true; }  // _Float64 → double
        if(match(CTokenType.FLOAT128)) { specs.saw_long = true; specs.saw_double = true; return true; }  // _Float128 → long double stub

        // Only one complex base type allowed
        if(specs.base_type !is null) return false;

        // void - sets base_type directly
        if(match(CTokenType.VOID))     { specs.base_type = &TYPE_VOID; return true; }
        // 128-bit integer types
        if(match(CTokenType.INT128))   { specs.base_type = specs.saw_unsigned ? &TYPE_UINT128 : &TYPE_INT128; return true; }
        if(match(CTokenType.UINT128))  { specs.base_type = &TYPE_UINT128; return true; }

        // (6.7.3.2) struct-or-union-specifier:
        //     struct-or-union identifier_opt { member-declaration-list }
        //     struct-or-union identifier
        if(match(CTokenType.STRUCT)){
            Attributes attrs;
            parse_gnu_attributes(attrs);

            CToken name_tok;
            bool has_name = false;
            if(check(CTokenType.IDENTIFIER)){
                name_tok = advance();
                has_name = true;
            }

            parse_gnu_attributes(attrs);

            // Check for definition: struct Name { ... } or struct { ... }
            if(check(CTokenType.LEFT_BRACE)){
                advance();  // consume '{'
                // Parse member-declaration-list
                auto fields = make_barray!StructField(allocator);
                size_t total_size = 0;
                int err = parse_member_declaration_list(&fields, &total_size, false);  // is_union=false
                if(err){ ERROR_OCCURRED = true; return false; }

                consume(CTokenType.RIGHT_BRACE, "Expected '}' after struct fields");
                if(ERROR_OCCURRED) return false;

                parse_gnu_attributes(attrs);

                // Register struct type
                CType* struct_type;
                if(has_name){
                    if(auto existing = name_tok.lexeme in struct_types){
                        // Update existing type in-place so typedefs continue to work
                        struct_type = *existing;
                        struct_type.fields = fields[];
                        struct_type.struct_size = total_size;
                    } else {
                        struct_type = make_struct_type(allocator, name_tok.lexeme, fields[], total_size);
                        struct_types[name_tok.lexeme] = struct_type;
                    }
                } else {
                    struct_type = make_struct_type(allocator, "", fields[], total_size);
                }
                specs.base_type = struct_type;
                return true;
            } else if(has_name){
                // Just a reference: struct Name
                if(auto t = name_tok.lexeme in struct_types){
                    specs.base_type = *t;
                    return true;
                }
                // Create incomplete struct type for forward reference
                void[] data = allocator.zalloc(CType.sizeof);
                CType* incomplete = cast(CType*)data.ptr;
                incomplete.kind = CTypeKind.STRUCT;
                incomplete.struct_name = name_tok.lexeme;
                incomplete.struct_size = 0;  // Unknown size
                struct_types[name_tok.lexeme] = incomplete;
                specs.base_type = incomplete;
                return true;
            }
            return false;  // Incomplete struct specifier (no name, no body)
        }
        if(match(CTokenType.UNION)){
            CToken name_tok;
            bool has_name = false;
            if(check(CTokenType.IDENTIFIER)){
                name_tok = advance();
                has_name = true;
            }

            // Check for definition: union Name { ... } or union { ... }
            if(check(CTokenType.LEFT_BRACE)){
                advance();  // consume '{'
                // Parse member-declaration-list
                auto fields = make_barray!StructField(allocator);
                size_t max_size = 0;
                int err = parse_member_declaration_list(&fields, &max_size, true);  // is_union=true
                if(err){ ERROR_OCCURRED = true; return false; }

                consume(CTokenType.RIGHT_BRACE, "Expected '}' after union fields");
                if(ERROR_OCCURRED) return false;

                // Register union type
                CType* union_type;
                if(has_name){
                    if(auto existing = name_tok.lexeme in union_types){
                        union_type = *existing;
                        union_type.fields = fields[];
                        union_type.struct_size = max_size;
                    } else {
                        union_type = make_union_type(allocator, name_tok.lexeme, fields[], max_size);
                        union_types[name_tok.lexeme] = union_type;
                    }
                } else {
                    union_type = make_union_type(allocator, "", fields[], max_size);
                }
                specs.base_type = union_type;
                return true;
            } else if(has_name){
                // Just a reference: union Name
                if(auto t = name_tok.lexeme in union_types){
                    specs.base_type = *t;
                    return true;
                }
                // Create incomplete union type for forward reference
                void[] data = allocator.zalloc(CType.sizeof);
                CType* incomplete = cast(CType*)data.ptr;
                incomplete.kind = CTypeKind.UNION;
                incomplete.struct_name = name_tok.lexeme;
                incomplete.struct_size = 0;
                union_types[name_tok.lexeme] = incomplete;
                specs.base_type = incomplete;
                return true;
            }
            return false;  // Incomplete union specifier
        }
        if(match(CTokenType.ENUM)){
            CToken name_tok;
            bool has_name = false;
            if(check(CTokenType.IDENTIFIER)){
                name_tok = advance();
                has_name = true;
            }

            // Check for definition: enum Name { ... } or enum { ... }
            if(check(CTokenType.LEFT_BRACE)){
                advance();  // consume '{'
                // Parse enumerator-list
                long enum_value = 0;
                while(!check(CTokenType.RIGHT_BRACE) && !at_end){
                    Attributes enum_attrs;
                    parse_gnu_attributes(enum_attrs);

                    CToken const_name = consume(CTokenType.IDENTIFIER, "Expected enumerator name");
                    if(ERROR_OCCURRED) return false;

                    parse_gnu_attributes(enum_attrs);

                    if(match(CTokenType.EQUAL)){
                        auto val_result = parse_enum_const_expr();
                        if(val_result.err){ ERROR_OCCURRED = true; return false; }
                        enum_value = val_result.value;
                    }

                    // Register enum constant in current scope
                    if(current_scope !is null){
                        current_scope.enum_constants[const_name.lexeme] = enum_value;
                    } else {
                        enum_constants[const_name.lexeme] = enum_value;
                    }
                    enum_value++;

                    if(!match(CTokenType.COMMA)){
                        break;
                    }
                }

                consume(CTokenType.RIGHT_BRACE, "Expected '}' after enum values");
                if(ERROR_OCCURRED) return false;

                // Enums are just ints
                CType* enum_type = &TYPE_INT;
                if(has_name){
                    enum_types[name_tok.lexeme] = enum_type;
                }
                specs.base_type = enum_type;
                return true;
            } else if(has_name){
                // Just a reference: enum Name
                if(auto t = name_tok.lexeme in enum_types){
                    specs.base_type = *t;
                    return true;
                }
                // Enum not found - treat as int (common pattern)
                specs.base_type = &TYPE_INT;
                return true;
            }
            return false;  // Incomplete enum specifier
        }

        // typeof specifier
        if(match(CTokenType.TYPEOF)){
            if(!match(CTokenType.LEFT_PAREN)){
                error("Expected '(' after typeof");
                return false;
            }
            if(!is_type_specifier(peek())){
                CExpr* expr = parse_expression();
                if(expr is null) return false;
                specs.base_type = expr.type;
            } else {
                specs.base_type = parse_type_name();
                if(specs.base_type is null) return false;
            }
            if(!match(CTokenType.RIGHT_PAREN)){
                error("Expected ')' after typeof");
                return false;
            }
            return true;
        }

        // Typedef names - only if we don't already have a type
        // This handles "int myint" where myint is a typedef: int is the type, myint is the declarator
        if(check(CTokenType.IDENTIFIER) && specs.base_type is null && !has_primitive_type_flags(specs)){
            if(auto t = peek().lexeme in typedef_types){
                advance();
                specs.base_type = *t;
                return true;
            }
            // Special type keywords
            str lex = peek().lexeme;
            if(lex == "_Bool"){ advance(); specs.base_type = &TYPE_INT; return true; }
        }
        return false;
    }

    // Parse declaration-specifiers (unified entry point)
    // This is the main function to parse the specifier sequence at the start of declarations
    DeclSpecifiers parse_declaration_specifiers(){
        DeclSpecifiers specs;
        specs.first_token = peek();

        while(true){
            // Skip attributes (GCC __attribute__)
            skip_gcc_attributes();

            // Try each specifier type
            if(parse_storage_class_specifier(&specs)) continue;
            if(parse_function_specifier(&specs)) continue;
            if(parse_type_qualifier(&specs)) continue;
            if(parse_type_specifier(&specs)) continue;

            break;
        }

        return specs;
    }

    // Parse specifier-qualifier-list (for type-name, member declarations)
    // Like declaration-specifiers but no storage-class or function-specifier
    // (6.7.3.2) specifier-qualifier-list:
    //     type-specifier-qualifier attribute-specifier-sequence_opt
    //     type-specifier-qualifier specifier-qualifier-list
    CType* parse_specifier_qualifier_list(){
        DeclSpecifiers specs;
        specs.first_token = peek();

        while(true){
            skip_gcc_attributes();
            if(parse_type_qualifier(&specs)) continue;
            if(parse_type_specifier(&specs)) continue;
            break;
        }

        // Finalize primitive type flags into base_type
        finalize_type_specifiers(&specs);

        if(specs.base_type is null){
            error("Expected type specifier");
            return null;
        }
        return specs.base_type;
    }

    // Skip GCC __attribute__((...)) sequences
    void skip_gcc_attributes(){
        while(match_id("__attribute__")){
            if(check(CTokenType.LEFT_PAREN)){
                skip_balanced_parens();
            }
        }
    }

    // =========================================================================
    // External Declaration Parsing (following C grammar)
    // =========================================================================

    // (6.9.1) external-declaration:
    //     function-definition
    //     declaration
    //
    // (6.7.1) declaration:
    //     declaration-specifiers init-declarator-list_opt ;
    //
    // This is the unified entry point for parsing top-level declarations.
    // It handles: function definitions, function declarations, variable declarations,
    // struct/union/enum definitions (with or without variable declarations).
    int parse_external_declaration(){
        CToken first_tok = peek();

        // Parse declaration-specifiers (storage class, type qualifiers, type specifier)
        DeclSpecifiers specs = parse_declaration_specifiers();
        if(specs.base_type is null){
            error("Expected type specifier");
            return 1;
        }

        // Handle type-only declaration (e.g., "struct Foo { int x; };")
        if(check(CTokenType.SEMICOLON)){
            advance();
            return 0;
        }

        // Parse the declarator
        CDeclarator* decl = parse_declarator(false);  // allow_abstract=false
        if(decl is null){
            error("Expected declarator");
            return 1;
        }
        CToken name = get_declarator_name(decl);
        CType* decl_type = apply_declarator_to_type(specs.base_type, decl);

        // Determine if this is a function definition or declaration
        bool is_function_type = decl_type !is null && decl_type.kind == CTypeKind.FUNCTION;

        if(is_function_type){
            // Function - either definition or declaration
            CFunction func;
            func.name = name;
            func.return_type = decl_type.return_type;

            // Get parameters from the declarator's function type
            auto params = make_barray!CParam(allocator);
            if(decl_type.param_types.length > 0 || decl_type.is_varargs){
                // Build parameter list from type info
                // Note: param names may be in the declarator, not the type
                foreach(i, pt; decl_type.param_types){
                    CParam p;
                    p.type = pt;
                    // Names are stored in declarator, but for now use placeholder
                    params ~= p;
                }
            }
            func.params = params[];
            func.is_varargs = decl_type.is_varargs;
            func.is_inline = specs.is_inline;
            func.library = current_library;

            skip_gcc_attributes();

            // Check if declaration or definition
            if(match(CTokenType.SEMICOLON)){
                func.is_definition = false;
                add_function(func);
                return 0;
            }

            // Function definition - parse body
            func.is_definition = true;
            add_function(func);

            consume(CTokenType.LEFT_BRACE, "Expected '{' for function body");
            if(ERROR_OCCURRED) return 1;

            // Set up type context for function body
            current_return_type = func.return_type;
            current_function_name = func.name.lexeme;
            static_local_counter = 0;

            push_scope();
            scope(exit) {
                current_function_name = null;
                current_return_type = null;
                pop_scope();
            }

            // Add parameters to scope
            foreach(ref param; func.params){
                if(param.name.lexeme.length > 0){
                    current_scope.variables[param.name.lexeme] = param.type;
                }
            }

            auto body = make_barray!(CStmt*)(allocator);
            while(!check(CTokenType.RIGHT_BRACE) && !at_end){
                CStmt* stmt = parse_statement();
                if(stmt is null) return 1;
                body ~= stmt;
            }

            consume(CTokenType.RIGHT_BRACE, "Expected '}' after function body");
            if(ERROR_OCCURRED) return 1;

            func.body = body[];
            if(auto idx = func.name.lexeme in func_indices){
                functions[*idx].body = body[];
            }
            return 0;
        } else {
            // Variable declaration - use parse_init_declarator_list
            bool is_extern = (specs.storage & DeclSpecifiers.StorageClass.EXTERN) != 0;
            int err = parse_init_declarator_list(decl_type, name, is_extern, current_library);
            if(err) return err;
            consume(CTokenType.SEMICOLON, "Expected ';' after variable declaration");
            if(ERROR_OCCURRED) return 1;
            return 0;
        }
    }

    // =========================================================================
    // Type Inference and Implicit Casts
    // =========================================================================

    // Get the type of an expression
    CType* get_expr_type(CExpr* e){
        if(e is null) return null;
        e = e.ungroup();

        // If type is already set, return it
        if(e.type !is null) return e.type;

        final switch(e.kind) with (CExprKind){
            case LITERAL:
                auto lit = e.as_literal;
                if(lit.value.type == CTokenType.STRING)
                    return &TYPE_CHAR_PTR;
                if(lit.value.type == CTokenType.CHAR_LITERAL)
                    return &TYPE_CHAR;
                if(lit.value.type == CTokenType.FLOAT_LITERAL){
                    // Check suffix for float vs double
                    str lex = lit.value.lexeme;
                    if(lex.length > 0){
                        char last = lex[$-1];
                        if(last == 'f' || last == 'F')
                            return &TYPE_FLOAT;
                    }
                    return &TYPE_DOUBLE;
                }
                return &TYPE_INT;  // Integer literals are int
            case IDENTIFIER:
                auto id = e.as_identifier;
                // Use scope chain lookup (walks from current scope up to globals)
                if(auto t = lookup_variable(id.name.lexeme))
                    return t;
                // Could be an enum constant - return int
                if(lookup_enum_constant(id.name.lexeme) !is null)
                    return &TYPE_INT;
                return null;
            case BINARY:
                auto bin = e.as_binary;
                auto lt = get_expr_type(bin.left);
                auto rt = get_expr_type(bin.right);
                // Comparison operators return int
                if(bin.op == CTokenType.EQUAL_EQUAL || bin.op == CTokenType.BANG_EQUAL ||
                   bin.op == CTokenType.LESS || bin.op == CTokenType.LESS_EQUAL ||
                   bin.op == CTokenType.GREATER || bin.op == CTokenType.GREATER_EQUAL)
                    return &TYPE_INT;
                // For arithmetic, result type follows usual arithmetic conversions
                if(lt && lt.is_pointer()) return lt;
                if(rt && rt.is_pointer()) return rt;
                if(lt && lt.is_float()) return lt;
                if(rt && rt.is_float()) return rt;
                return lt ? lt : rt;
            case UNARY:
                auto un = e.as_unary;
                if(un.op == CTokenType.STAR){
                    auto pt = get_expr_type(un.operand);
                    if(pt && pt.is_pointer()) return pt.pointed_to;
                }
                if(un.op == CTokenType.AMP){
                    auto ot = get_expr_type(un.operand);
                    if(ot) return make_pointer_type(allocator, ot);
                }
                if(un.op == CTokenType.BANG)
                    return &TYPE_INT;
                return get_expr_type(un.operand);
            case CALL:
                auto call = cast(CCall*)e;
                if(CIdentifier* id = call.callee.as_identifier()){
                    if(size_t* idx = id.name.lexeme in func_info){
                        return functions[*idx].return_type;
                    }
                }
                return null;
            case ASSIGN:
                return get_expr_type((cast(CAssign*)e).target);
            case SUBSCRIPT:
                auto sub = e.as_subscript;
                auto at = get_expr_type(sub.array);
                if(at && (at.is_pointer() || at.is_array())) return at.pointed_to;
                return null;
            case MEMBER_ACCESS:
                auto ma = e.as_member_access;
                auto obj_type = get_expr_type(ma.object);
                if(obj_type is null) return null;
                if(ma.is_arrow && obj_type.is_pointer())
                    obj_type = obj_type.pointed_to;
                if(obj_type && obj_type.is_struct_or_union()){
                    auto field = obj_type.get_field(ma.member.lexeme);
                    if(field) return field.type;
                }
                return null;
            case CAST:
                return (cast(CCast*)e).cast_type;
            case SIZEOF:
            case ALIGNOF:
            case COUNTOF:
                return &TYPE_LONG;  // size_t is typically unsigned long
            case VA_ARG:
            case GROUPING:
                return null;
            case GENERIC:
                return null;  // Complex to resolve
            case TERNARY:
                auto tern = cast(CTernary*)e;
                auto tt = get_expr_type(tern.if_true);
                if(tt) return tt;
                return get_expr_type(tern.if_false);
            case INIT_LIST:
            case COMPOUND_LITERAL:
                auto cl = cast(CCompoundLiteral*)e;
                return cl.literal_type;
            case EMBED:
            case STMT_EXPR:
                return null;
        }
    }

    // Check if a type needs conversion to target type
    // Returns true if implicit cast is needed
    bool needs_conversion(CType* from, CType* to){
        // FIXME: pointer types??
        if(from is null || to is null) return false;
        if(from is to) return false;
        if(from.kind == to.kind && from.is_signed == to.is_signed) return false;

        // Any arithmetic type can be converted to any other arithmetic type
        if(from.is_arithmetic() && to.is_arithmetic()) return true;

        return false;
    }

    // Wrap expression with implicit cast if needed
    // Returns original expr if no conversion needed
    CExpr* implicit_cast(CExpr* expr, CType* target_type, CToken tok){
        if(expr is null || target_type is null) return expr;

        CType* expr_type = get_expr_type(expr);
        if(expr_type is null) return expr;

        if(!needs_conversion(expr_type, target_type)) return expr;

        // Create implicit cast node
        return CCast.make(allocator, target_type, expr, tok);
    }

    // Apply "usual arithmetic conversions" to binary operands
    // Modifies left and right to have matching types for arithmetic
    void usual_arithmetic_conversions(ref CExpr* left, ref CExpr* right, CToken op_tok){
        CType* lt = get_expr_type(left);
        CType* rt = get_expr_type(right);

        if(lt is null || rt is null) return;

        // If either is a pointer, no conversion (pointer arithmetic handled separately)
        if(lt.is_pointer() || rt.is_pointer()) return;

        // If either is double, convert other to double
        if(lt.kind == CTypeKind.DOUBLE || lt.kind == CTypeKind.LONG_DOUBLE){
            right = implicit_cast(right, lt, op_tok);
            return;
        }
        if(rt.kind == CTypeKind.DOUBLE || rt.kind == CTypeKind.LONG_DOUBLE){
            left = implicit_cast(left, rt, op_tok);
            return;
        }

        // Float handling (FLT_EVAL_METHOD=0 - use native float types)
        if(lt.kind == CTypeKind.FLOAT && rt.kind == CTypeKind.FLOAT){
            // Both float: no conversion needed, backend uses F* instructions
            return;
        }
        if(lt.kind == CTypeKind.FLOAT){
            // float op non-float: promote float to double, convert other if needed
            left = implicit_cast(left, &TYPE_DOUBLE, op_tok);
            if(rt.is_arithmetic() && !rt.is_float())
                right = implicit_cast(right, &TYPE_DOUBLE, op_tok);
            return;
        }
        if(rt.kind == CTypeKind.FLOAT){
            // non-float op float: promote float to double, convert other if needed
            right = implicit_cast(right, &TYPE_DOUBLE, op_tok);
            if(lt.is_arithmetic() && !lt.is_float())
                left = implicit_cast(left, &TYPE_DOUBLE, op_tok);
            return;
        }

        // Integer promotions: char, short -> int
        // For simplicity, we don't change integer types here since
        // our VM uses register-sized ints anyway
    }

    // =========================================================================
    // Top-Level Parsing
    // =========================================================================

    // Helper to add function, merging forward declarations with definitions
    void add_function(ref CFunction func){
        str fname = func.name.lexeme;
        if(auto idx_ptr = fname in func_indices){
            // Already have an entry for this function
            size_t idx = *idx_ptr;
            if(func.is_definition && !functions[idx].is_definition){
                // Current is definition, existing is declaration - replace
                functions[idx] = func;
                // func_info already has the correct index
            }
            // Otherwise (existing is definition or both declarations): skip
        } else {
            // New function
            size_t idx = functions.count;
            func_indices[fname] = idx;
            func_info[fname] = idx;
            functions ~= func;

            // Create and store function type for identifier resolution
            auto param_types = make_barray!(CType*)(allocator);
            foreach(ref p; func.params){
                param_types ~= p.type;
            }
            CType* ftype = make_function_type(allocator, func.return_type, param_types[], func.is_varargs);
            func_types[fname] = ftype;
        }
    }

    int parse(CTranslationUnit* unit){
        // Initialize type tables
        functions = make_barray!CFunction(allocator);
        globals = make_barray!CGlobalVar(allocator);
        structs = make_barray!CStructDef(allocator);
        unions = make_barray!CUnionDef(allocator);
        enums = make_barray!CEnumDef(allocator);

        struct_types.data.allocator = allocator;
        union_types.data.allocator = allocator;
        enum_types.data.allocator = allocator;
        enum_constants.data.allocator = allocator;
        typedef_types.data.allocator = allocator;
        global_var_types.data.allocator = allocator;
        func_types.data.allocator = allocator;
        func_info.data.allocator = allocator;
        func_indices.data.allocator = allocator;
        global_indices.data.allocator = allocator;
        error_sb.allocator = allocator;

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

            // EXTENSION: static if at global scope
            // Both "static if" and bare "if" work (if is unambiguous at global scope)
            if(check(CTokenType.STATIC) && peek_at(1).type == CTokenType.IF){
                advance();  // consume 'static'
                int err = parse_global_static_if();
                if(err) return err;
                continue;
            }
            if(check(CTokenType.IF)){
                int err = parse_global_static_if();
                if(err) return err;
                continue;
            }

            // _Static_assert(expr, "message");
            if(match(CTokenType.STATIC_ASSERT)){
                int err = parse_static_assert();
                if(err) return err;
                continue;
            }

            // Parse any global declaration: typedef, extern, static, inline, functions, variables
            // This handles struct/union/enum definitions, storage class specifiers in any order,
            // and complex declarators (function pointers, arrays, etc.)
            int err = parse_global_declaration();
            if(err) return err;
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

    // Result type for constant expression parsing
    struct ConstExprResult {
        long value;
        bool err;
    }

    // Parse a constant expression using the real expression parser + constant evaluator
    // This follows the C grammar properly and handles all valid constant expressions
    ConstExprResult parse_const_expr(){
        ConstExprResult result;
        result.err = false;

        // Use the regular expression parser (conditional-expression per C grammar)
        CExpr* expr = parse_ternary();
        if(expr is null || ERROR_OCCURRED){
            result.err = true;
            return result;
        }

        // Evaluate as constant
        ConstValue val = try_eval_constant(expr);
        if(!val.is_const()){
            error(expr.token, "Expression is not a compile-time constant");
            result.err = true;
            return result;
        }

        result.value = val.as_long();
        return result;
    }

    // Alias for backwards compatibility with existing call sites
    ConstExprResult parse_enum_const_expr(){
        return parse_const_expr();
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

            // Register in current scope (or global if at file scope)
            if(!declare_enum_constant(const_name.lexeme, value, const_name))
                return 1;

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

    // Unified global declaration parser
    // Handles: typedef, extern, static, inline, functions, variables
    // Grammar: declaration-specifiers declarator [= initializer | { body }] ;
    int parse_global_declaration(){
        alias SC = DeclSpecifiers.StorageClass;

        // Parse all declaration specifiers
        DeclSpecifiers specs = parse_declaration_specifiers();
        finalize_type_specifiers(&specs);

        bool is_typedef = (specs.storage & SC.TYPEDEF) != 0;
        bool is_extern = (specs.storage & SC.EXTERN) != 0;
        bool is_static = (specs.storage & SC.STATIC) != 0;
        bool is_inline = specs.is_inline;

        CType* base_type = specs.base_type;

        // Type-only declaration: struct Foo { }; enum Bar { A, B };
        if(match(CTokenType.SEMICOLON)){
            if(base_type is null){
                error("Expected type specifier");
                return 1;
            }
            return 0;  // Just a type definition, nothing to register
        }

        if(base_type is null){
            error("Expected type specifier");
            return 1;
        }

        // Parse declarator(s) - could be multiple: int x, y, *z;
        do {
            Attributes attrs;
            parse_gnu_attributes(attrs);

            CDeclarator* decl = parse_declarator(false);
            if(decl is null) return 1;

            CType* final_type = apply_declarator_to_type(base_type, decl);
            CToken name = get_declarator_name(decl);

            if(name.lexeme.length == 0){
                error("Expected identifier");
                return 1;
            }

            parse_gnu_attributes(attrs);

            // Handle vector_size attribute
            if(attrs.vector_size.type == CTokenType.NUMBER){
                import dlib.parse_numbers: parse_uint64;
                size_t sz = parse_uint64(attrs.vector_size.lexeme).value;
                final_type = make_vector_type(allocator, final_type, sz);
            }

            // Find the declarator that contains the name - this determines if it's
            // a function or variable. For "int (*fp)(int)", the name "fp" is in a pointer
            // declarator, so it's a function pointer variable, not a function.
            CDeclarator* name_decl = find_name_declarator(decl);
            if(name_decl is null) name_decl = decl;
            bool is_function_decl = name_decl.is_function;

            // Handle asm label for symbol renaming: __asm("symbol_name")
            // This appears after function declarations in system headers
            if(match(CTokenType.ASM)){
                consume(CTokenType.LEFT_PAREN, "Expected '(' after asm");
                if(ERROR_OCCURRED) return 1;
                while(check(CTokenType.STRING)){
                    advance();
                }
                consume(CTokenType.RIGHT_PAREN, "Expected ')' after asm label");
                if(ERROR_OCCURRED) return 1;
            }
            parse_gnu_attributes(attrs);

            if(is_typedef){
                // Typedef: register type alias
                typedef_types[name.lexeme] = final_type;
            } else if(is_function_decl && check(CTokenType.LEFT_BRACE)){
                // Function definition
                CFunction func;
                func.name = name;
                // Build return type from declarator parts outside the function containing the name
                func.return_type = build_function_return_type(base_type, decl, name_decl);
                func.params = name_decl.params;
                func.is_varargs = name_decl.is_varargs;
                func.is_definition = true;
                func.is_inline = is_inline;
                func.is_static = is_static;
                func.library = current_library;

                add_function(func);

                // Parse function body
                push_scope();
                current_function_name = func.name.lexeme;
                current_return_type = func.return_type;
                foreach(ref param; func.params){
                    if(param.name.lexeme.length > 0){
                        current_scope.variables[param.name.lexeme] = param.type;
                    }
                }

                consume(CTokenType.LEFT_BRACE, "Expected '{' for function body");
                if(ERROR_OCCURRED){ pop_scope(); current_function_name = null; return 1; }

                auto body_ = make_barray!(CStmt*)(allocator);
                while(!check(CTokenType.RIGHT_BRACE) && !at_end){
                    CStmt* stmt = parse_statement();
                    if(stmt is null){ pop_scope(); current_function_name = null; return 1; }
                    body_ ~= stmt;
                }

                consume(CTokenType.RIGHT_BRACE, "Expected '}' after function body");
                if(ERROR_OCCURRED){ pop_scope(); current_function_name = null; return 1; }

                pop_scope();
                current_function_name = null;
                current_return_type = null;

                func.body = body_[];
                if(auto idx_ptr = func.name.lexeme in func_indices){
                    functions[*idx_ptr] = func;
                }
                return 0;  // Function definitions can't have multiple declarators
            } else if(is_function_decl){
                // Function declaration
                CFunction func;
                func.name = name;
                func.return_type = build_function_return_type(base_type, decl, name_decl);
                func.params = name_decl.params;
                func.is_varargs = name_decl.is_varargs;
                func.is_definition = false;
                func.is_inline = is_inline;
                func.is_static = is_static;
                func.library = current_library;
                add_function(func);
            } else {
                // Global variable
                CGlobalVar gvar;
                gvar.name = name;
                gvar.var_type = final_type;
                gvar.initializer = null;
                gvar.is_extern = is_extern;
                gvar.library = current_library;

                // Register variable type for sizeof lookups in constant expressions
                global_var_types[name.lexeme] = gvar.var_type;

                if(match(CTokenType.EQUAL)){
                    gvar.initializer = parse_initializer();
                    if(gvar.initializer is null) return 1;
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
                            // Count elements, but __embed contributes its length in bytes
                            size_t total = 0;
                            foreach(elem; ilist.elements){
                                if(CEmbed* emb = elem.value.as_embed()){
                                    total += emb.length;
                                } else {
                                    total++;
                                }
                            }
                            gvar.var_type.array_size = total;
                        }
                    }
                }

                add_or_merge_global(gvar);
            }
        } while(match(CTokenType.COMMA));

        consume(CTokenType.SEMICOLON, "Expected ';' after declaration");
        if(ERROR_OCCURRED) return 1;

        return 0;
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
        Attributes attrs;
        parse_gnu_attributes(attrs);
        // Parse parameters using unified parameter parsing
        consume(CTokenType.LEFT_PAREN, "Expected '(' after function name");
        if(ERROR_OCCURRED) return 1;

        auto param_result = parse_parameter_type_list();
        if(param_result.err) return 1;

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after parameters");
        if(ERROR_OCCURRED) return 1;

        parse_gnu_attributes(attrs);

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
        parse_gnu_attributes(attrs);

        func.name = name;
        func.return_type = ret_type;
        func.params = param_result.params[];
        func.is_varargs = param_result.is_varargs;
        func.is_inline = saw_inline;
        func.library = current_library;

        parse_gnu_attributes(attrs);

        // Check if declaration or definition
        if(match(CTokenType.SEMICOLON)){
            func.is_definition = false;
            add_function(*func);
            return 0;
        }
        // Parse function body - set is_definition BEFORE add_function for recursion
        func.is_definition = true;
        add_function(*func);
        consume(CTokenType.LEFT_BRACE, "Expected '{' for function body");
        if(ERROR_OCCURRED) return 1;

        // Set up type context for function body
        current_return_type = ret_type;
        current_function_name = func.name.lexeme;
        static_local_counter = 0;

        // Push function body scope and add parameters
        push_scope();

        scope(exit) {
            current_function_name = null;
            current_return_type = null;
            // Clear type context after function
            pop_scope();
        }

        foreach(ref param; func.params){
            if(param.name.lexeme.length > 0){
                current_scope.variables[param.name.lexeme] = param.type;
            }
        }

        auto body = make_barray!(CStmt*)(allocator);
        while(!check(CTokenType.RIGHT_BRACE) && !at_end){
            CStmt* stmt = parse_statement();
            if(stmt is null){ return 1; }
            body ~= stmt;
        }

        consume(CTokenType.RIGHT_BRACE, "Expected '}' after function body");
        if(ERROR_OCCURRED){ return 1; }

        func.body = body[];
        // Update the function entry with the parsed body
        // (function was already added before parsing for recursion support)
        if(auto idx = func.name.lexeme in func_indices){
            functions[*idx].body = body[];
        }
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

        // Register variable type for sizeof lookups in constant expressions
        global_var_types[name.lexeme] = gvar.var_type;

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
                    // Count elements, but __embed contributes its length in bytes
                    size_t total = 0;
                    foreach(elem; ilist.elements){
                        if(CEmbed* emb = elem.value.as_embed()){
                            total += emb.length;
                        } else {
                            total++;
                        }
                    }
                    gvar.var_type.array_size = total;
                }
            }
        }


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
        CType* base = parse_specifier_qualifier_list();
        if(base is null) return null;
        return parse_pointer_modifiers(base);
    }

    // Parse a type-name for casts, sizeof, compound literals
    // This includes full abstract declarators (pointers, arrays, function pointers)
    // (6.7.8) type-name: specifier-qualifier-list abstract-declarator_opt
    CType* parse_type_name(){
        CType* base = parse_specifier_qualifier_list();
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
    // This is just a convenience wrapper around parse_declarator with allow_abstract=true
    CDeclarator* parse_abstract_declarator(){
        // Handle postfix qualifiers before pointer (e.g., "char const *")
        while(match(CTokenType.CONST) || match(CTokenType.VOLATILE) ||
               match(CTokenType.RESTRICT) || match(CTokenType.ATOMIC)){}

        // Check if there's actually a declarator to parse
        if(!check(CTokenType.STAR) && !check(CTokenType.LEFT_PAREN) &&
           !check(CTokenType.LEFT_BRACKET)){
            return null;  // No abstract declarator present
        }

        return parse_declarator(true);  // allow_abstract=true
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
        void[] data = allocator.zalloc(CDeclarator.sizeof);
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
        auto dims = make_barray!size_t(allocator);
        while(true){
            if(check(CTokenType.LEFT_BRACKET)){
                // (6.7.7.1) array-declarator:
                //     direct-declarator [ type-qualifier-list_opt assignment-expression_opt ]
                //     direct-declarator [ static type-qualifier-list_opt assignment-expression ]
                //     direct-declarator [ type-qualifier-list static assignment-expression ]
                //     direct-declarator [ type-qualifier-list_opt * ]
                advance();  // consume '['

                // Skip optional 'static' and type qualifiers (const, volatile, restrict)
                skip_array_qualifiers();

                // Parse array size if present
                size_t dim = 0;
                if(!check(CTokenType.RIGHT_BRACKET)){
                    auto size_result = parse_enum_const_expr();
                    if(!size_result.err && size_result.value > 0){
                        dim = cast(size_t)size_result.value;
                    }
                }
                dims ~= dim;

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
                // Store param types and full params
                auto param_types = make_barray!(CType*)(allocator);
                foreach(ref p; param_result.params[]){
                    param_types ~= p.type;
                }
                decl.param_types = param_types[];
                decl.params = param_result.params[];

                consume(CTokenType.RIGHT_PAREN, "Expected ')' after parameters");
                if(ERROR_OCCURRED) return null;

            } else {
                break;
            }
        }

        // Store accumulated array dimensions
        decl.array_dims = dims[];

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

    // Helper: Extract the name and its containing declarator from a declarator tree.
    // For simple declarators like `int x`, the name is directly in decl.name.
    // For complex declarators like `int (*f)(int)`, the name is in a nested declarator.
    // Returns: (name_token, declarator_containing_name)
    static CToken get_declarator_name(CDeclarator* decl){
        if(decl is null) return CToken.init;
        CToken name = decl.name;
        if(name.lexeme.length > 0) return name;
        // Search nested declarators
        CDeclarator* inner = decl.nested;
        while(inner !is null){
            if(inner.name.lexeme.length > 0) return inner.name;
            inner = inner.nested;
        }
        return CToken.init;
    }

    // Helper: Find the declarator that contains the name in a declarator tree.
    // For complex declarators like `int (*f)(int)`, this returns the declarator
    // where is_function=true if f is declared as a function.
    static CDeclarator* find_name_declarator(CDeclarator* decl){
        if(decl is null) return null;
        if(decl.name.lexeme.length > 0) return decl;
        CDeclarator* inner = decl.nested;
        while(inner !is null){
            if(inner.name.lexeme.length > 0) return inner;
            inner = inner.nested;
        }
        return null;
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

        // Apply array suffix(es)
        if(decl.array_dims.length > 0){
            for(size_t i = decl.array_dims.length; i > 0; i--){
                size_t dim = decl.array_dims[i - 1];
                if(dim > 0){
                    result = make_array_type(allocator, result, dim);
                } else {
                    result = make_pointer_type(allocator, result);
                }
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
    //   - Nested declarator: pointer_depth=1, array_dims=[10], name=funcs
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
        // For arrays, apply dimensions in reverse order (rightmost dimension is innermost)
        // e.g., int arr[2][3] -> array of 2 arrays of 3 ints
        if(decl.array_dims.length > 0){
            for(size_t i = decl.array_dims.length; i > 0; i--){
                size_t dim = decl.array_dims[i - 1];
                // Create array type (size 0 for unsized arrays - will be inferred from initializer)
                result = make_array_type(allocator, result, dim);
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
                if(!declare_enum_constant(ename.lexeme, enum_val, ename))
                    return 1;
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
        // parse attributes
        Attributes attrs;
        parse_gnu_attributes(attrs);

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

        // (6.7.7.1) function-declarator: direct-declarator ( parameter-type-list_opt )
        // Parse the parameter-type-list to build proper function pointer type
        CType* fptr_type;
        if(check(CTokenType.LEFT_PAREN)){
            consume(CTokenType.LEFT_PAREN, "Expected '(' for function parameters");
            if(ERROR_OCCURRED) return 1;
            auto func_param_result = parse_parameter_type_list();
            if(func_param_result.err) return 1;
            // Extract types from parameters
            auto func_param_types = make_barray!(CType*)(allocator);
            foreach(ref p; func_param_result.params[]){
                func_param_types ~= p.type;
            }
            consume(CTokenType.RIGHT_PAREN, "Expected ')' after function parameters");
            if(ERROR_OCCURRED) return 1;
            CType* func_type = make_function_type(allocator, return_type, func_param_types[], func_param_result.is_varargs);
            fptr_type = make_pointer_type(allocator, func_type);
        } else {
            // No parameter list - function pointer with no params
            fptr_type = make_pointer_type(allocator, make_function_type(allocator, return_type, null, false));
        }

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
        // Check for (void) - means no parameters
        if(check(CTokenType.VOID)){
            CToken void_tok = advance();
            if(check(CTokenType.RIGHT_PAREN)){
                // Just "void" alone at end means no parameters
                return 0;
            }
            if(check(CTokenType.COMMA)){
                error("'void' parameter must be the only parameter");
                return 1;
            }
            // void* or void(...) - put back and parse normally
            current--;  // un-consume VOID
        }

        // Parse specifier-qualifier-list (base type)
        CType* base_type = parse_specifier_qualifier_list();
        if(base_type is null) return 1;

        // Skip postfix qualifiers (e.g., "int const *" -> qualifiers between type and declarator)
        while(match(CTokenType.CONST) || match(CTokenType.VOLATILE) ||
               match(CTokenType.RESTRICT) || match(CTokenType.ATOMIC)){}

        // Parse declarator (name is optional in parameters - allow_abstract=true)
        // Check if there's a declarator to parse: *, (, [, or identifier
        CDeclarator* decl = null;
        if(check(CTokenType.STAR) || check(CTokenType.LEFT_PAREN) ||
           check(CTokenType.LEFT_BRACKET) || check(CTokenType.IDENTIFIER)){
            decl = parse_declarator(true);  // allow_abstract=true
        }

        CParam param;
        if(decl !is null){
            param.type = apply_declarator_to_type(base_type, decl);
            param.name = get_declarator_name(decl);

            // Array parameters decay to pointers (C99 6.7.6.3p7)
            if(param.type !is null && param.type.kind == CTypeKind.ARRAY){
                param.type = make_pointer_type(allocator, param.type.pointed_to);
            }
        } else {
            // No declarator - just the base type
            param.type = base_type;
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
    // Uses add_or_merge_global to handle extern->definition merging.
    // Does NOT consume the final semicolon - caller must do that.
    // is_extern: true if 'extern' keyword was present (declaration only, object defined elsewhere)
    // library: from #pragma library, specifies which library contains the extern object
    int parse_init_declarator_list(CType* first_type, CToken first_name,
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
        add_or_merge_global(gvar);

        // Parse remaining comma-separated declarators
        while(match(CTokenType.COMMA)){
            // Each declarator can have its own pointer modifiers
            CType* decl_type = parse_pointer_modifiers(base_type);
            CToken next_name = consume(CTokenType.IDENTIFIER, "Expected variable name");
            if(ERROR_OCCURRED) return 1;

            CGlobalVar gvar2;
            err = parse_global_var_rest(decl_type, next_name, &gvar2, is_extern, library);
            if(err) return err;
            add_or_merge_global(gvar2);
        }

        return 0;
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

        // Handle static_assert in statement context (C11)
        if(match(CTokenType.STATIC_ASSERT)){
            int err = parse_static_assert();
            if(err) return null;
            return CEmptyStmt.get();  // Evaluated at compile-time, no runtime effect
        }

        // Handle case/default labels inside switch (including nested statements like Duff's device)
        if(switch_depth > 0){
            if(match(CTokenType.CASE)) return parse_case_label();
            if(match(CTokenType.DEFAULT)) return parse_default_label();
        }

        // Check for label: identifier followed by colon
        if(check(CTokenType.IDENTIFIER) && peek_at(1).type == CTokenType.COLON){
            return parse_label();
        }

        // EXTENSION: static if - compile-time conditional
        if(check(CTokenType.STATIC) && peek_at(1).type == CTokenType.IF){
            advance();  // consume 'static'
            return parse_static_if();
        }

        // Check for variable declaration (starts with type or storage class)
        // Also handle auto/__auto_type/constexpr for type inference, typedef for block-scope typedefs
        if(check(CTokenType.STATIC) || check(CTokenType.AUTO) || check(CTokenType.CONSTEXPR) || check(CTokenType.TYPEDEF) || is_type_specifier(peek())){
            return parse_var_decl();
        }
        if(check(CTokenType.IDENTIFIER) && peek().lexeme == "__auto_type"){
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
                tok.type == DECIMAL128 || tok.type == TYPEOF){
                return true;
            }
            // Check for typedef names and special identifier keywords
            // But local variables shadow typedef names, so check local scope first
            if(tok.type == IDENTIFIER){
                // If there's a local variable with this name, it's not a type
                if(lookup_local_variable(tok.lexeme) !is null) return false;
                if(tok.lexeme in typedef_types) return true;
                // Special type keywords that may not have token types
                str lex = tok.lexeme;
                if(lex == "_Complex" || lex == "_Bool" || lex == "__int128") return true;
                return false;
            }
            return false;
        }
    }

    // Skip 'static' and type qualifiers inside array brackets (C99 6.7.6.3)
    // Handles: [static const N], [const static N], [restrict], [*], etc.
    void skip_array_qualifiers(){
        while(true){
            if(match(CTokenType.STATIC)){}
            else if(match(CTokenType.CONST)){}
            else if(match(CTokenType.VOLATILE)){}
            else if(match(CTokenType.RESTRICT)){}
            else if(match(CTokenType.STAR)){} // VLA with unspecified size
            else break;
        }
    }

    // Result type for parse_type_or_expr - either a type or an expression (compound literal)
    static struct TypeOrExpr {
        CType* type;   // Set if it's (type) without compound literal
        CExpr* expr;   // Set if it's a compound literal (type){...}
    }

    // Parse (type) or (type){...} for sizeof/_Alignof/_Countof operators.
    // Returns: .type set if just a type, .expr set if compound literal, both null if not (type).
    TypeOrExpr parse_type_or_expr(){
        // Check for (type) pattern
        if(!check(CTokenType.LEFT_PAREN)){
            return TypeOrExpr(null, null);  // No parens, caller should parse expr
        }

        // Peek ahead to see if it's a type
        if(!is_type_specifier(peek_at(1))){
            return TypeOrExpr(null, null);  // Not a type, caller should parse expr
        }

        // It's (type) - parse it
        CToken paren_tok = advance();  // consume '('
        CType* type = parse_type_name();
        if(type is null) return TypeOrExpr(null, null);
        consume(CTokenType.RIGHT_PAREN, "Expected ')' after type");
        if(ERROR_OCCURRED) return TypeOrExpr(null, null);

        // Check for compound literal: (type){...}
        if(check(CTokenType.LEFT_BRACE)){
            CExpr* init_expr = parse_initializer();
            if(init_expr is null) return TypeOrExpr(null, null);
            CExpr* compound = CCompoundLiteral.make(allocator, type, init_expr, paren_tok);
            return TypeOrExpr(null, compound);
        }

        return TypeOrExpr(type, null);
    }

    // (6.8.7.1) return statement:
    //     return expression_opt ;
    CStmt* parse_return(){
        CToken keyword = previous();
        CExpr* value = null;

        if(!check(CTokenType.SEMICOLON)){
            value = parse_expression();
            if(value is null) return null;

            // Apply implicit cast to function's return type
            if(current_return_type !is null){
                value = implicit_cast(value, current_return_type, keyword);
            }
        }

        consume(CTokenType.SEMICOLON, "Expected ';' after return");
        if(ERROR_OCCURRED) return null;

        return CReturnStmt.make(allocator, value, keyword);
    }

    // EXTENSION: static if at global scope - compile-time conditional for declarations
    // static if(const-expr) declaration
    // static if(const-expr) { declarations }
    // The skipped branch doesn't need to be valid C, just balanced brackets.
    int parse_global_static_if(){
        advance();  // consume 'if'

        consume(CTokenType.LEFT_PAREN, "Expected '(' after 'static if'");
        if(ERROR_OCCURRED) return 1;

        auto result = parse_const_expr();
        if(result.err) return 1;

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after static if condition");
        if(ERROR_OCCURRED) return 1;

        bool condition = result.value != 0;

        if(condition){
            // Parse the taken branch
            int err = parse_global_static_if_body();
            if(err) return err;

            // Skip else branch if present
            if(match(CTokenType.ELSE)){
                skip_global_static_if_body();
            }
        } else {
            // Skip the false branch
            skip_global_static_if_body();

            // Check for else branch
            if(match(CTokenType.ELSE)){
                int err = parse_global_static_if_body();
                if(err) return err;
            }
        }

        return 0;
    }

    // Parse the body of a global static if (braced or single declaration)
    int parse_global_static_if_body(){
        if(check(CTokenType.LEFT_BRACE)){
            advance();  // consume '{'

            // Parse declarations until '}'
            while(!check(CTokenType.RIGHT_BRACE) && !at_end){
                // Recursively handle nested static if
                if(check(CTokenType.STATIC) && peek_at(1).type == CTokenType.IF){
                    advance();  // consume 'static'
                    int err = parse_global_static_if();
                    if(err) return err;
                    continue;
                }

                // Skip semicolons
                if(match(CTokenType.SEMICOLON)) continue;

                // Handle pragma
                if(check(CTokenType.HASH) && peek_at(1).type == CTokenType.IDENTIFIER && peek_at(1).lexeme == "pragma"){
                    handle_pragma();
                    continue;
                }

                // Parse a declaration
                int err = parse_one_global_declaration();
                if(err) return err;
            }

            consume(CTokenType.RIGHT_BRACE, "Expected '}' after static if body");
            if(ERROR_OCCURRED) return 1;
        } else {
            // Single declaration (no braces)
            // Handle nested static if
            if(check(CTokenType.STATIC) && peek_at(1).type == CTokenType.IF){
                advance();  // consume 'static'
                return parse_global_static_if();
            }

            return parse_one_global_declaration();
        }

        return 0;
    }

    // Skip the body of a global static if (braced or single declaration)
    void skip_global_static_if_body(){
        if(check(CTokenType.LEFT_BRACE)){
            skip_balanced_braces();
        } else {
            // Single declaration - skip until semicolon or function body
            skip_global_declaration();
        }
    }

    // Skip a single global declaration (for skipped static if branches)
    void skip_global_declaration(){
        // Skip until we see a semicolon at depth 0, or a closing brace after an opening one
        int brace_depth = 0;
        while(!at_end){
            CTokenType t = peek().type;
            if(t == CTokenType.LEFT_BRACE){
                brace_depth++;
                advance();
            } else if(t == CTokenType.RIGHT_BRACE){
                if(brace_depth > 0){
                    brace_depth--;
                    advance();
                    if(brace_depth == 0) return;  // End of function body
                } else {
                    return;  // Don't consume - might be end of enclosing block
                }
            } else if(t == CTokenType.SEMICOLON && brace_depth == 0){
                advance();  // consume ';'
                return;
            } else {
                advance();
            }
        }
    }

    // Parse a single global declaration (for use in static if bodies)
    // Delegates to parse_global_declaration which handles typedef, extern, static, etc.
    int parse_one_global_declaration(){
        // Handle _Static_assert specially
        if(match(CTokenType.STATIC_ASSERT)){
            return parse_static_assert();
        }

        return parse_global_declaration();
    }

    // EXTENSION: static if - compile-time conditional
    // static if(const-expr) statement
    // static if(const-expr) statement else statement
    // The braces do NOT introduce a new scope.
    // The skipped branch doesn't need to be valid C, just balanced brackets.
    CStmt* parse_static_if(){
        CToken keyword = previous();  // 'static'
        advance();  // consume 'if'

        consume(CTokenType.LEFT_PAREN, "Expected '(' after 'static if'");
        if(ERROR_OCCURRED) return null;

        auto result = parse_const_expr();
        if(result.err) return null;

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after static if condition");
        if(ERROR_OCCURRED) return null;

        bool condition = result.value != 0;

        if(condition){
            // Parse the taken branch
            CStmt* then_branch;
            if(check(CTokenType.LEFT_BRACE)){
                // Parse block WITHOUT introducing new scope
                advance();  // consume '{'
                then_branch = parse_block_body();
                if(then_branch is null) return null;
            } else {
                then_branch = parse_statement();
                if(then_branch is null) return null;
            }

            // Skip else branch if present
            if(match(CTokenType.ELSE)){
                skip_balanced_statement();
            }

            return then_branch;
        } else {
            // Skip the false branch
            skip_balanced_statement();

            // Check for else branch
            if(match(CTokenType.ELSE)){
                if(check(CTokenType.LEFT_BRACE)){
                    // Parse block WITHOUT introducing new scope
                    advance();  // consume '{'
                    return parse_block_body();
                } else {
                    return parse_statement();
                }
            }

            // No else, return empty statement
            return CEmptyStmt.get();
        }
    }

    // Skip a balanced statement (for static if/else skipping)
    // Handles single statements or braced blocks
    void skip_balanced_statement(){
        if(check(CTokenType.LEFT_BRACE)){
            skip_balanced_braces();  // consumes '{' and matching '}'
        } else {
            // Single statement - skip until semicolon, but handle nested constructs
            skip_until_semicolon();
        }
    }

    // Skip until semicolon, handling nested (), [], {}
    void skip_until_semicolon(){
        int paren_depth = 0;
        int bracket_depth = 0;
        int brace_depth = 0;
        while(!at_end){
            CTokenType t = peek().type;
            if(t == CTokenType.LEFT_PAREN) paren_depth++;
            else if(t == CTokenType.RIGHT_PAREN) paren_depth--;
            else if(t == CTokenType.LEFT_BRACKET) bracket_depth++;
            else if(t == CTokenType.RIGHT_BRACKET) bracket_depth--;
            else if(t == CTokenType.LEFT_BRACE) brace_depth++;
            else if(t == CTokenType.RIGHT_BRACE) brace_depth--;
            else if(t == CTokenType.SEMICOLON && paren_depth == 0 && bracket_depth == 0 && brace_depth == 0){
                advance();  // consume ';'
                return;
            }
            advance();
        }
    }

    // Parse block body without the surrounding scope push/pop
    // Used for static if where braces don't introduce scope
    CStmt* parse_block_body(){
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

        // C99: for loop has its own scope for variables declared in initializer
        push_scope();

        // Initializer
        CStmt* init = null;
        if(!check(CTokenType.SEMICOLON)){
            if(is_type_specifier(peek())){
                init = parse_var_decl();
            } else {
                init = parse_expr_stmt();
            }
            if(init is null){ pop_scope(); return null; }
        } else {
            advance();  // consume ';'
        }

        // Condition
        CExpr* condition = null;
        if(!check(CTokenType.SEMICOLON)){
            condition = parse_expression();
            if(condition is null){ pop_scope(); return null; }
        }
        consume(CTokenType.SEMICOLON, "Expected ';' after for condition");
        if(ERROR_OCCURRED){ pop_scope(); return null; }

        // Increment
        CExpr* increment = null;
        if(!check(CTokenType.RIGHT_PAREN)){
            increment = parse_expression();
            if(increment is null){ pop_scope(); return null; }
        }

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after for clauses");
        if(ERROR_OCCURRED){ pop_scope(); return null; }

        CStmt* body = parse_statement();
        if(body is null){ pop_scope(); return null; }

        pop_scope();
        return CForStmt.make(allocator, init, condition, increment, body, keyword);
    }

    // (6.8.3) compound-statement:
    //     { block-item-list_opt }
    CStmt* parse_block(){
        CToken brace = previous();
        auto statements = make_barray!(CStmt*)(allocator);

        push_scope();  // Create new scope for block
        scope(exit) pop_scope();  // Restore on exit (even on error)

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
    //
    // EXTENSION: Inner function definition (like GCC nested functions)
    // At block scope, also allows:
    //     declaration-specifiers declarator compound-statement
    // This is the same as (6.9.2) function-definition but at block scope.
    // Desugars to a static function with mangled name.
    CStmt* parse_var_decl(){
        CToken type_tok = peek();

        // Parse all declaration specifiers (storage class, qualifiers, type)
        DeclSpecifiers specs = parse_declaration_specifiers();
        alias SC = DeclSpecifiers.StorageClass;

        // Finalize primitive type flags into base_type
        finalize_type_specifiers(&specs);

        bool is_static = (specs.storage & SC.STATIC) != 0;
        bool is_typedef = (specs.storage & SC.TYPEDEF) != 0;
        bool saw_auto = (specs.storage & SC.AUTO) != 0;
        bool saw_constexpr = (specs.storage & SC.CONSTEXPR) != 0;
        CType* base_type = specs.base_type;

        // Block-scope typedef: register type alias and return empty statement
        if(is_typedef){
            if(base_type is null){
                error("Expected type specifier in typedef");
                return null;
            }
            CDeclarator* decl = parse_declarator(false);
            if(decl is null) return null;
            CType* final_type = apply_declarator_to_type(base_type, decl);
            CToken typedef_name = get_declarator_name(decl);
            if(typedef_name.lexeme.length == 0){
                error("Expected typedef name");
                return null;
            }
            consume(CTokenType.SEMICOLON, "Expected ';' after typedef");
            if(ERROR_OCCURRED) return null;
            typedef_types[typedef_name.lexeme] = final_type;
            return CEmptyStmt.get();
        }

        // Type inference: auto/const/static/constexpr without type specifier
        // - auto x = 3;           → infer type
        // - static auto x = 3;    → static + infer type
        // - const x = "hello";    → infer const type (extension)
        // - static x = 3;         → static + infer type (extension)
        // - constexpr x = 3;      → constexpr (implies const) + infer type
        bool infer_type = false;
        bool infer_const = specs.is_const || saw_constexpr;

        if(base_type is null){
            if(saw_auto || specs.is_const || is_static || saw_constexpr){
                infer_type = true;
            } else {
                error("Expected type specifier");
                return null;
            }
        }

        // Handle postfix qualifiers (e.g., "int const *" instead of "const int *")
        while(match(CTokenType.CONST) || match(CTokenType.VOLATILE) || match(CTokenType.RESTRICT)){
            // Just skip - we don't track these
        }

        // Handle type-only declarations (e.g., "enum Foo { A, B };")
        if(match(CTokenType.SEMICOLON)){
            return CEmptyStmt.get();
        }

        auto decls = make_barray!(CStmt*)(allocator);

        do {
            // Use parse_declarator to handle all declarator types including:
            // - Simple: int x
            // - Pointers: int *p
            // - Arrays: int arr[2][3]
            // - Function pointers: int (*fp)(int)
            // - Complex: int *(*fp[10])(int, int)
            CDeclarator* decl = parse_declarator(false);
            if(decl is null) return null;

            CToken name = get_declarator_name(decl);

            if(name.lexeme.length == 0){
                error("Expected variable name");
                return null;
            }

            // Check for inner function definition (not allowed with type inference)
            if(decl.is_function && check(CTokenType.LEFT_BRACE)){
                if(infer_type){
                    error(name, "Cannot use type inference with inner function definitions");
                    return null;
                }
                return parse_inner_function(base_type, decl, name);
            }

            CType* var_type;
            CExpr* initializer = null;

            if(infer_type){
                // Type inference mode: require initializer, infer type from it
                if(!match(CTokenType.EQUAL)){
                    error(name, "Type inference requires an initializer");
                    return null;
                }

                initializer = parse_initializer();
                if(initializer is null) return null;

                // Cannot infer type from initializer list
                if(initializer.kind == CExprKind.INIT_LIST){
                    error(name, "Cannot infer type from initializer list");
                    return null;
                }

                // Infer type from initializer
                var_type = initializer.type;
                if(var_type is null){
                    error(name, "Cannot infer type: initializer has no type");
                    return null;
                }

                // Apply declarator modifiers (pointers, arrays) to inferred type
                var_type = apply_declarator_to_type(var_type, decl);

                // Declare variable in scope (after we know its type)
                if(!declare_variable(name.lexeme, var_type, name)){
                    ERROR_OCCURRED = true;
                }
            } else {
                // Normal mode: build type from base_type + declarator
                var_type = apply_declarator_to_type(base_type, decl);

                // Declare variable in scope BEFORE parsing initializer
                // (C requires variable to be in scope for its own initializer, e.g. int x = sizeof x;)
                if(!declare_variable(name.lexeme, var_type, name)){
                    ERROR_OCCURRED = true;
                }

                if(match(CTokenType.EQUAL)){
                    initializer = parse_initializer();
                    if(initializer is null) return null;

                    // Apply implicit cast for scalar initializers (not init lists)
                    if(initializer.kind != CExprKind.INIT_LIST){
                        initializer = implicit_cast(initializer, var_type, name);
                    }
                }
            }

            if(is_static){
                // Static local: create global with mangled name, register in local scope
                // Mangled name: funcname/varname$counter
                str mangled_name = mwritef(allocator, "%/%$%",
                    current_function_name, name.lexeme, static_local_counter++)[];

                // Create a modified token with the mangled name for the global
                CToken mangled_tok = name;
                mangled_tok.lexeme = mangled_name;

                // Create global variable
                CGlobalVar gvar;
                gvar.name = mangled_tok;
                gvar.var_type = var_type;
                gvar.initializer = initializer;
                gvar.is_extern = false;
                add_or_merge_global(gvar);

                // Register name mapping so identifier lookup can find the mangled name
                current_scope.static_local_names[name.lexeme] = mangled_name;

                // No runtime initialization needed - static vars are initialized at program start
            } else {
                // Regular local variable (already declared above)
                decls ~= CVarDecl.make(allocator, var_type, name, initializer, type_tok);
            }

        } while(match(CTokenType.COMMA));

        consume(CTokenType.SEMICOLON, "Expected ';' after variable declaration");
        if(ERROR_OCCURRED) return null;

        // If no runtime declarations (all static), return empty statement
        if(decls[].length == 0){
            return CEmptyStmt.get();
        }

        // If only one declaration, return it directly
        if(decls[].length == 1){
            return decls[0];
        }

        // Multiple declarations - wrap in a block
        return CBlock.make(allocator, decls[], type_tok);
    }

    // EXTENSION: Inner (nested) function definition
    //
    // Grammar (extending block-item at block scope):
    // (6.8.3) block-item:
    //     declaration
    //     unlabeled-statement
    //     label
    //     function-definition              <-- EXTENSION
    //
    // (6.9.2) function-definition:
    //     declaration-specifiers declarator function-body
    // (6.9.2) function-body:
    //     compound-statement
    //
    // Standard C only allows function-definition at file scope (6.9.1).
    // This extension allows it at block scope (like GCC nested functions).
    //
    // Semantics:
    // - Desugars to a static function with mangled name (outer/inner$N)
    // - Inner function has access to outer scope for type resolution (typeof works)
    // - Inner function cannot access outer scope variables at runtime
    // - Inner function is only visible within the enclosing function
    CStmt* parse_inner_function(CType* return_type, CDeclarator* decl, CToken name){
        // Generate mangled name: outer_func/inner_func$counter
        str mangled_name = mwritef(allocator, "%/%$%",
            current_function_name, name.lexeme, static_local_counter++)[];

        // Create function structure with mangled name
        CFunction func;
        CToken mangled_tok = name;
        mangled_tok.lexeme = mangled_name;
        func.name = mangled_tok;
        func.return_type = return_type;
        func.params = decl.params;
        func.is_varargs = decl.is_varargs;
        func.is_inline = false;
        func.is_definition = true;

        // Register function globally with mangled name (before parsing body for recursion)
        add_function(func);

        // Register mapping in local scope so inner function can be called by its local name
        current_scope.inner_function_names[name.lexeme] = mangled_name;

        // Save outer function context
        str saved_function_name = current_function_name;
        CType* saved_return_type = current_return_type;
        int saved_counter = static_local_counter;

        // Set up inner function context
        current_function_name = mangled_name;
        current_return_type = return_type;
        static_local_counter = 0;

        // Parse function body - keep scope chain for type resolution!
        // Push scope for parameters only (don't clear outer scope)
        push_scope();

        // Add parameters to inner function's scope
        foreach(ref param; decl.params){
            if(param.name.lexeme.length > 0){
                current_scope.variables[param.name.lexeme] = param.type;
            }
        }

        // Consume '{' and parse body
        consume(CTokenType.LEFT_BRACE, "Expected '{' for function body");
        if(ERROR_OCCURRED){ pop_scope(); return null; }

        auto body_ = make_barray!(CStmt*)(allocator);
        while(!check(CTokenType.RIGHT_BRACE) && !at_end){
            CStmt* stmt = parse_statement();
            if(stmt is null){ pop_scope(); return null; }
            body_ ~= stmt;
        }

        consume(CTokenType.RIGHT_BRACE, "Expected '}' after function body");
        pop_scope();

        // Restore outer function context
        current_function_name = saved_function_name;
        current_return_type = saved_return_type;
        static_local_counter = saved_counter;

        // Update function with body
        func.body = body_[];
        if(auto idx = mangled_name in func_indices){
            functions[*idx].body = body_[];
        }

        return CEmptyStmt.get();  // No runtime statement needed
    }

    // =========================================================================
    // EXTENSION: Function Literals
    // =========================================================================
    //
    // Grammar (extending primary-expression):
    //
    // (6.5.2) primary-expression:
    //     identifier
    //     constant
    //     string-literal
    //     ( expression )
    //     generic-selection
    //     function-literal                    <-- EXTENSION
    //
    // function-literal:
    //     type-specifier pointer_opt ( parameter-type-list_opt ) compound-statement
    //
    // Examples:
    //     int(int x, int y) { return x + y; }
    //     void*() { return malloc(10); }
    //     void(void) { puts("hello"); }
    //
    // Disambiguation:
    //     - Type keywords (int, void, etc.) cannot start expressions in C
    //     - Typedef names: '{' after ')' distinguishes from function call
    //       (like compound literals are distinguished from casts)

    // Check if we're at the start of a function literal.
    // Returns true if: type-specifier [*...] ( ... ) {
    bool is_function_literal_start(){
        // Must start with a type specifier
        if(!is_type_specifier(peek())) return false;

        // For type keywords (int, void, etc.), they can't start expressions,
        // so if we see type-keyword followed by optional * and (, it must be a function literal
        // For typedef names, we need to verify '{' follows the parameter list

        // Scan ahead: skip type-specifier, pointers, find '(', match to ')', check for '{'
        int pos = current;

        // Skip the type specifier token(s)
        // Handle: int, unsigned int, struct Foo, etc.
        if(tokens[pos].type == CTokenType.STRUCT ||
           tokens[pos].type == CTokenType.UNION ||
           tokens[pos].type == CTokenType.ENUM){
            pos++;  // skip struct/union/enum
            if(pos < tokens.length && tokens[pos].type == CTokenType.IDENTIFIER)
                pos++;  // skip tag name
        } else if(tokens[pos].type == CTokenType.UNSIGNED ||
                  tokens[pos].type == CTokenType.SIGNED){
            pos++;  // skip unsigned/signed
            // Skip optional int/long/short/char
            if(pos < tokens.length){
                auto t = tokens[pos].type;
                if(t == CTokenType.INT || t == CTokenType.LONG ||
                   t == CTokenType.SHORT || t == CTokenType.CHAR)
                    pos++;
            }
        } else {
            pos++;  // skip simple type (int, void, typedef-name, etc.)
        }

        // Skip pointer modifiers: * const volatile restrict
        while(pos < tokens.length){
            auto t = tokens[pos].type;
            if(t == CTokenType.STAR || t == CTokenType.CONST ||
               t == CTokenType.VOLATILE || t == CTokenType.RESTRICT){
                pos++;
            } else {
                break;
            }
        }

        // Must have '(' for parameter list
        if(pos >= tokens.length || tokens[pos].type != CTokenType.LEFT_PAREN)
            return false;
        pos++;  // skip '('

        // Find matching ')'
        int depth = 1;
        while(pos < tokens.length && depth > 0){
            if(tokens[pos].type == CTokenType.LEFT_PAREN) depth++;
            else if(tokens[pos].type == CTokenType.RIGHT_PAREN) depth--;
            pos++;
        }

        // After ')', must have '{' for function body
        if(pos >= tokens.length || tokens[pos].type != CTokenType.LEFT_BRACE)
            return false;

        return true;
    }

    // Parse a function literal expression
    // Caller has verified this is a function literal via is_function_literal_start()
    CExpr* parse_function_literal(){
        CToken start_tok = peek();

        // Parse return type (base type + optional pointers)
        CType* return_type = parse_type();
        if(return_type is null) return null;

        // Consume '('
        consume(CTokenType.LEFT_PAREN, "Expected '(' in function literal");
        if(ERROR_OCCURRED) return null;

        // Parse parameter list
        ParameterListResult param_result = parse_parameter_type_list();
        if(param_result.err) return null;

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after parameters");
        if(ERROR_OCCURRED) return null;

        // Generate anonymous function name
        str lambda_name;
        if(current_function_name.length > 0){
            // Inside a function: use outer$lambda$N
            lambda_name = mwritef(allocator, "%$lambda$%",
                current_function_name, lambda_counter++)[];
        } else {
            // At global scope (e.g., in global initializer)
            lambda_name = mwritef(allocator, "__lambda$%", lambda_counter++)[];
        }

        // Create function structure
        CFunction func;
        CToken name_tok = start_tok;
        name_tok.lexeme = lambda_name;
        func.name = name_tok;
        func.return_type = return_type;
        func.params = param_result.params[];
        func.is_varargs = param_result.is_varargs;
        func.is_inline = false;
        func.is_definition = true;

        // Register function before parsing body (for potential recursion, though unlikely)
        add_function(func);

        // Save outer function context
        str saved_function_name = current_function_name;
        CType* saved_return_type = current_return_type;
        int saved_counter = static_local_counter;

        // Set up lambda function context
        current_function_name = lambda_name;
        current_return_type = return_type;
        static_local_counter = 0;

        // Parse function body
        push_scope();

        // Add parameters to scope
        foreach(ref param; param_result.params[]){
            if(param.name.lexeme.length > 0){
                current_scope.variables[param.name.lexeme] = param.type;
            }
        }

        // Consume '{' and parse body
        consume(CTokenType.LEFT_BRACE, "Expected '{' for function literal body");
        if(ERROR_OCCURRED){ pop_scope(); return null; }

        auto body_ = make_barray!(CStmt*)(allocator);
        while(!check(CTokenType.RIGHT_BRACE) && !at_end){
            CStmt* stmt = parse_statement();
            if(stmt is null){ pop_scope(); return null; }
            body_ ~= stmt;
        }

        consume(CTokenType.RIGHT_BRACE, "Expected '}' after function literal body");
        pop_scope();

        // Restore outer function context
        current_function_name = saved_function_name;
        current_return_type = saved_return_type;
        static_local_counter = saved_counter;

        // Update function with body
        func.body = body_[];
        if(auto idx = lambda_name in func_indices){
            functions[*idx].body = body_[];
        }

        // Create function type and return pointer to function
        auto param_types = make_barray!(CType*)(allocator);
        foreach(ref p; param_result.params[]){
            param_types ~= p.type;
        }
        CType* func_type = make_function_type(allocator, return_type, param_types[], param_result.is_varargs);
        CType* func_ptr_type = make_pointer_type(allocator, func_type);

        // Return identifier expression for the function (acts as function pointer)
        return CIdentifier.make_func(allocator, name_tok, func_type);
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
                // EXTENSION: __unpack(s) -> s.x, s.y, ... (for structs)
                // EXTENSION: __unpack(arr) -> arr[0], arr[1], ... (for arrays)
                if(match_id("__unpack")){
                    CToken unpack = previous();
                    if(!match(CTokenType.LEFT_PAREN)){
                        error(peek(), "Need '(' for __unpack");
                        return null;
                    }
                    CToken tok = peek();
                    CExpr* e = parse_assignment();
                    if(e is null) return null;
                    if(!match(CTokenType.RIGHT_PAREN)){
                        error(peek(), "Need ')' for __unpack");
                        return null;
                    }
                    if(e.type.is_struct || (e.type.is_pointer && e.type.pointed_to.is_struct)){
                        // Struct unpacking
                        CType* st = e.type.is_struct ? e.type : e.type.pointed_to;
                        foreach(ref StructField f; st.fields){
                            CToken member = unpack;
                            member.type = CTokenType.IDENTIFIER;
                            member.lexeme = f.name;
                            CInitElement elem;
                            elem.value = CMemberAccess.make(allocator, e, member, e.type.is_pointer, unpack, f.type);
                            elements ~= elem;
                        }
                    } else if(e.type.is_array){
                        // Array unpacking
                        if(e.type.array_size == 0){
                            error(tok, "__unpack requires fixed-size array");
                            return null;
                        }
                        CType* elem_type = e.type.pointed_to;
                        foreach(i; 0 .. e.type.array_size){
                            CExpr* idx = CLiteral.make_int(allocator, cast(int)i, unpack);
                            CInitElement elem;
                            elem.value = CSubscript.make(allocator, e, idx, unpack, elem_type);
                            elements ~= elem;
                        }
                    } else {
                        error(tok, "argument of __unpack must be a struct, pointer to struct, or fixed-size array");
                        return null;
                    }
                } else {
                    CInitElement elem;

                    // Parse optional designation
                    elem.designators = parse_designation();
                    if(ERROR_OCCURRED) return null;

                    // Parse initializer (recursive for nested braces)
                    elem.value = parse_initializer();
                    if(elem.value is null) return null;

                    elements ~= elem;
                }
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
            expr = CBinary.make(allocator, expr, op.type, right, op, right.type);
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

            // Apply implicit cast for assignments
            // CType* target_type = get_expr_type(expr);
            CType* target_type = expr.type;
            if(target_type.is_float() && op_tok.type != CTokenType.EQUAL){
                // Compound assignment with float LHS: cast RHS to double for arithmetic
                value = implicit_cast(value, &TYPE_DOUBLE, op_tok);
                if(!value) return null;
            }
            else {
                // Simple assignment: cast RHS to LHS type
                value = implicit_cast(value, target_type, op_tok);
                if(!value) return null;
            }
            return CAssign.make(allocator, expr, op_tok.type, value, op_tok, target_type);
        }

        return expr;
    }

    // (6.5.16) conditional-expression:
    //     logical-OR-expression
    //     logical-OR-expression ? expression : conditional-expression
    CExpr* parse_ternary(){
        CToken tok = peek();
        CExpr* condition = parse_logical_or();
        if(condition is null) return null;

        if(!check(CTokenType.QUESTION)){
            return condition;
        }
        if(!condition.type.is_boolable){
            error(tok, "condition of ?: must be a boolable condition");
            return null;
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
        // FIXME: unify types for other than arithmetic
        usual_arithmetic_conversions(if_true, if_false, question_tok);
        // FIXME: pointers etc.

        return CTernary.make(allocator, condition, if_true, if_false, question_tok, if_true.type);
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
            if(!expr.type.is_boolable()){
                error(expr.token, "lhs of || needs to be boolable");
                return null;
            }
            if(!right.type.is_boolable()){
                error(expr.token, "lhs of || needs to be boolable");
                return null;
            }
            // TODO: insert implicit casts here.
            expr = CBinary.make(allocator, expr, op.type, right, op, &TYPE_INT);
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
            if(!expr.type.is_boolable()){
                error(expr.token, "lhs of || needs to be boolable");
                return null;
            }
            if(!right.type.is_boolable()){
                error(expr.token, "lhs of || needs to be boolable");
                return null;
            }
            // TODO: implicit casts
            expr = CBinary.make(allocator, expr, op.type, right, op, &TYPE_INT);
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
            usual_arithmetic_conversions(expr, right, op);
            // TODO: implicit casts
            expr = CBinary.make(allocator, expr, op.type, right, op, expr.type);
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
            usual_arithmetic_conversions(expr, right, op);
            // TODO: implicit casts
            expr = CBinary.make(allocator, expr, op.type, right, op, expr.type);
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
            usual_arithmetic_conversions(expr, right, op);
            // TODO: implicit casts
            expr = CBinary.make(allocator, expr, op.type, right, op, expr.type);
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
            usual_arithmetic_conversions(expr, right, op);
            // TODO: implicit casts
            // Comparison operators always return int
            expr = CBinary.make(allocator, expr, op.type, right, op, &TYPE_INT);
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
            // TODO: this can fail, emit error and return null
            usual_arithmetic_conversions(expr, right, op);
            // Relational operators always return int
            expr = CBinary.make(allocator, expr, op.type, right, op, &TYPE_INT);
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
            // TODO: implicit casts
            expr = CBinary.make(allocator, expr, op.type, right, op, expr.type);
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
            // TODO: this can fail, emit error and return null
            usual_arithmetic_conversions(expr, right, op);
            expr = CBinary.make(allocator, expr, op.type, right, op, expr.type);
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
            // TODO: this can fail, emit error and return null
            usual_arithmetic_conversions(expr, right, op);
            expr = CBinary.make(allocator, expr, op.type, right, op, expr.type);
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
    //     _Countof unary-expression
    //     _Countof ( type-name )
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
            CType* type;
            switch(op.type)with(CTokenType){
                case BANG:
                    // Logical not always produces int (0 or 1)
                    type = &TYPE_INT;
                    break;
                case MINUS, PLUS, TILDE:
                    // Unary arithmetic: result is operand type (after integer promotion)
                    type = operand.type;
                    break;
                case STAR:
                    // Dereference: result is pointed-to type
                    if(!operand.type.is_pointer && !operand.type.is_array){
                        error(op, "Cannot dereference non-pointer type");
                        return null;
                    }
                    type = operand.type.pointed_to;
                    // Dereferencing pointer-to-array produces array type.
                    // Arrays decay to addresses, so this is a no-op type change, not a memory read.
                    if(type.is_array){
                        return CCast.make(allocator, type, operand, op);
                    }
                    break;
                case AMP:
                    // Address-of: result is pointer to operand type
                    type = make_pointer_type(allocator, operand.type);
                    break;
                case PLUS_PLUS, MINUS_MINUS:
                    // Prefix inc/dec: result is operand type
                    type = operand.type;
                    break;
                default:
                    assert(0, "Unhandled prefix unary operator");
            }
            return CUnary.make(allocator, op.type, operand, true, op, type);
        }

        // sizeof operator: sizeof(type) or sizeof expr
        if(match(CTokenType.SIZEOF)){
            CToken op = previous();
            TypeOrExpr result = parse_type_or_expr();
            if(ERROR_OCCURRED) return null;
            if(result.type !is null){
                return CSizeof.make(allocator, result.type, result.type.size_of(), op);
            }
            if(result.expr is null){
                result.expr = parse_unary();
                if(result.expr is null) return null;
            }
            return CSizeof.make_expr(allocator, result.expr, op);
        }

        // _Alignof operator: _Alignof(type) or _Alignof expr (GNU extension)
        if(match(CTokenType.ALIGNOF)){
            CToken op = previous();
            TypeOrExpr result = parse_type_or_expr();
            if(ERROR_OCCURRED) return null;
            if(result.type !is null){
                return CAlignof.make(allocator, result.type, result.type.align_of(), op);
            }
            if(result.expr is null){
                result.expr = parse_unary();
                if(result.expr is null) return null;
            }
            return CAlignof.make_expr(allocator, result.expr, op);
        }

        // _Countof operator: _Countof(type) or _Countof expr
        if(match(CTokenType.COUNTOF)){
            CToken op = previous();
            TypeOrExpr result = parse_type_or_expr();
            if(ERROR_OCCURRED) return null;
            if(result.type !is null){
                if(!result.type.is_array()){
                    error("_Countof requires an array type");
                    return null;
                }
                return CCountof.make(allocator, result.type, result.type.array_size, op);
            }
            if(result.expr is null){
                result.expr = parse_unary();
                if(result.expr is null) return null;
            }
            return CCountof.make_expr(allocator, result.expr, 0, op);
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
    // (6.5.3.6) compound-literal:
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
                if(expr.type.is_struct_or_union){
                    ConstValue v = try_eval_constant(index);
                    if(v.kind != v.kind.INTEGER){
                        errorf(index.token, "(Extension): Indexing a struct or union type requires a compile-time evaluable constant integer, idx is type: ", str_for(index.type.kind));
                        return null;
                    }
                    if(v.int_val < 0 || v.int_val >= expr.type.fields.length){
                        errorf(index.token, "(Extension): Index of struct type out of bounds: idx = ", v.int_val, ", fields.length = ", expr.type.fields.length);
                        return null;
                    }
                    CToken member = index.token;
                    member.type = CTokenType.IDENTIFIER;
                    member.lexeme = expr.type.fields[v.int_val].name;
                    CToken fake_dot = bracket;
                    fake_dot.type = CTokenType.DOT;
                    expr = CMemberAccess.make(allocator, expr, member, false, fake_dot, expr.type.fields[v.int_val].type);
                }
                else {
                    if(!expr.type.is_indexable){
                        errorf(bracket, "Indexing requires an array or pointer type, type is ", str_for(expr.type.kind));
                        return null;
                    }
                    expr = CSubscript.make(allocator, expr, index, bracket, expr.type.element_type());
                }
            } else if(match(CTokenType.DOT)){
                // Member access: expr.member
                CToken dot = previous();
                CToken member = consume(CTokenType.IDENTIFIER, "Expected member name after '.'");
                if(ERROR_OCCURRED) return null;

                // EXTENSION: allow dot operator on pointers - treat as arrow
                bool use_arrow = false;
                CType* obj_type = expr.type;
                if(obj_type.is_pointer() && obj_type.pointed_to.is_struct_or_union()){
                    use_arrow = true;
                    obj_type = obj_type.pointed_to;
                }

                if(!obj_type.is_struct_or_union()){
                    // EXTENSION: UFCS for non-struct types (e.g., int.to_string())
                    if(check(CTokenType.LEFT_PAREN)){
                        if(CExpr* func_id = try_lookup_function(member)){
                            advance(); // consume '('
                            expr = finish_call(func_id, expr);
                            if(expr is null) return null;
                            continue;
                        }
                    }
                    error(dot, "'.' operator requires a struct or union type");
                    return null;
                }
                // Set type from struct field
                if(StructField *field = obj_type.get_field(member.lexeme)){
                    expr = CMemberAccess.make(allocator, expr, member, use_arrow, dot, field.type);
                }
                else {
                    // EXTENSION: UFCS - x.foo(args) -> foo(x, args)
                    if(check(CTokenType.LEFT_PAREN)){
                        if(CExpr* func_id = try_lookup_function(member)){
                            advance(); // consume '('
                            expr = finish_call(func_id, expr);
                            if(expr is null) return null;
                            continue;
                        }
                    }
                    error(member, "Unknown field name");
                    return null;
                }
            } else if(match(CTokenType.ARROW)){
                // Pointer member access: expr->member
                CToken arrow = previous();
                CToken member = consume(CTokenType.IDENTIFIER, "Expected member name after '->'");
                if(ERROR_OCCURRED) return null;
                CType* obj_type = expr.type;
                if(!obj_type.is_pointer){
                    // EXTENSION: UFCS for non-pointer types
                    if(check(CTokenType.LEFT_PAREN)){
                        if(CExpr* func_id = try_lookup_function(member)){
                            advance(); // consume '('
                            expr = finish_call(func_id, expr);
                            if(expr is null) return null;
                            continue;
                        }
                    }
                    error(arrow, "'->' operator requires a pointer type");
                    return null;
                }
                if(!obj_type.pointed_to.is_struct_or_union){
                    // EXTENSION: UFCS for pointer to non-struct
                    if(check(CTokenType.LEFT_PAREN)){
                        if(CExpr* func_id = try_lookup_function(member)){
                            advance(); // consume '('
                            expr = finish_call(func_id, expr);
                            if(expr is null) return null;
                            continue;
                        }
                    }
                    error(arrow, "'->' operator requires a pointer to a structure or union");
                    return null;
                }
                if(StructField *field = obj_type.pointed_to.get_field(member.lexeme)){
                    CType* type = field.type;
                    expr = CMemberAccess.make(allocator, expr, member, true, arrow, field.type);
                }
                else {
                    // EXTENSION: UFCS - ptr->foo(args) -> foo(ptr, args)
                    if(check(CTokenType.LEFT_PAREN)){
                        if(CExpr* func_id = try_lookup_function(member)){
                            advance(); // consume '('
                            expr = finish_call(func_id, expr);
                            if(expr is null) return null;
                            continue;
                        }
                    }
                    error(member, "Unknown field name");
                    return null;
                }
            } else if(match(CTokenType.PLUS_PLUS) || match(CTokenType.MINUS_MINUS)){
                // Postfix increment/decrement: result is operand type (original value)
                CToken op = previous();
                expr = CUnary.make(allocator, op.type, expr, false, op, expr.type);
            } else {
                break;
            }
        }

        return expr;
    }

    // (6.5.3.1) argument-expression-list:
    //     assignment-expression
    //     argument-expression-list , assignment-expression
    // ufcs_receiver: if non-null, this is a UFCS call and receiver is prepended as first arg
    CExpr* finish_call(CExpr* callee, CExpr* ufcs_receiver = null){
        CToken paren = previous();
        auto args = make_barray!(CExpr*)(allocator);

        // Try to get function parameter types for implicit casts
        CType* function_type = callee.type;
        if(function_type.is_pointer)
            function_type = function_type.pointed_to;
        if(!function_type.is_function){
            errorf(paren, "Attempt to call a non-function type, type: ", str_for(function_type.kind));
            return null;
        }
        if(function_type.is_magic_builtin){
            current--;
            skip_balanced_parens();
            current--;
        }
        else {
            // UFCS: prepend receiver as first argument
            size_t arg_idx = 0;
            if(ufcs_receiver !is null){
                if(function_type.param_types.length == 0){
                    error(paren, "UFCS: function has no parameters to receive the object");
                    return null;
                }
                CType* param0_type = function_type.param_types[0];
                CExpr* receiver = ufcs_receiver;

                // Auto-ref: if param expects pointer to receiver's type, take address
                if(param0_type.is_pointer && !receiver.type.is_pointer){
                    if(types_compatible(param0_type.pointed_to, receiver.type)){
                        // Insert & operator
                        receiver = CUnary.make(allocator, CTokenType.AMP, receiver, true, paren, make_pointer_type(allocator, receiver.type));
                    }
                }

                receiver = implicit_cast(receiver, param0_type, paren);
                if(receiver is null) return null;
                args ~= receiver;
                arg_idx = 1;
            }

            if(!check(CTokenType.RIGHT_PAREN)){
                CExpr* do_arg_cast(CToken tok, CExpr* arg){
                    if(arg is null) return null;

                    // Apply implicit cast if we have parameter type info
                    if(arg_idx < function_type.param_types.length){
                        CType* param_type = function_type.param_types[arg_idx];
                        arg = implicit_cast(arg, param_type, tok);
                        if(!arg) return null;
                        // FIXME: type check??
                    } else if(function_type.is_varargs && arg_idx >= function_type.param_types.length){
                        // Varargs: float promotes to double
                        final switch(arg.type.kind) with(arg.type.kind){
                            case UNSET:
                                error(tok, "ICE: type unset");
                                return null;
                            case VOID:
                                error(tok, "Void-returning arguments to functions are illlegal");
                                return null;
                            case CHAR:
                            case SHORT:
                            case INT:
                                arg = implicit_cast(arg, arg.type.is_signed?&TYPE_INT:&TYPE_UINT, tok);
                                if(!arg) return null;
                                break;
                            case LONG:
                            case LONG_LONG:
                                // no conversion needed
                                break;
                            case INT128:
                                error(tok, "int128 cannot be passed as varargs");
                                // Or can it?
                                return null;
                            case FLOAT:
                                arg = implicit_cast(arg, &TYPE_DOUBLE, tok);
                                if(!arg) return null;
                                break;
                            case DOUBLE:
                                // no conversion needed
                                break;
                            case LONG_DOUBLE:
                                // Idk if this is correct.
                                error(tok, "long double cannot be passed as varargs");
                                return null;
                            case POINTER:
                                // No conversion
                                break;
                            case ARRAY:
                                // Do we need array decay here or is this ok?
                                break;
                            case FUNCTION:
                                 // Do we need function pointer decay here?
                            case STRUCT:
                                error(tok, "Cannot pass a struct to varags");
                                return null;
                            case UNION:
                                error(tok, "Cannot pass a union to varags");
                                return null;
                            case EMBED:
                                error(tok, "UNIMPLEMENTED: EMBED");
                                return null;
                            case VECTOR:
                                error(tok, "UNIMPLEMENTED: VECTOR");
                                return null;
                            case INIT_LIST:
                                error(tok, "ICE: INIT_LIST ESCAPED");
                                return null;
                            case ENUM:
                                break;
                            case ANY:
                                break;
                        }
                    }
                    else {
                        error(tok, "Too many arguments to function");
                        return null;
                    }
                    return arg;
                }
                do {
                    // EXTENSION: __unpack(s) -> s.x, s.y, ... (for structs)
                    // EXTENSION: __unpack(arr) -> arr[0], arr[1], ... (for arrays)
                    if(match_id("__unpack")){
                        CToken unpack = previous();
                        if(!match(CTokenType.LEFT_PAREN)){
                            error(peek(), "Need '(' for __unpack");
                            return null;
                        }
                        CToken tok = peek();
                        CExpr* e = parse_assignment();
                        if(e is null) return null;
                        if(!match(CTokenType.RIGHT_PAREN)){
                            error(peek(), "Need ')' for __unpack");
                            return null;
                        }
                        if(e.type.is_struct || (e.type.is_pointer && e.type.pointed_to.is_struct)){
                            // Struct unpacking
                            CType* st = e.type.is_struct ? e.type : e.type.pointed_to;
                            foreach(ref StructField f; st.fields){
                                CToken member = unpack;
                                member.type = CTokenType.IDENTIFIER;
                                member.lexeme = f.name;
                                CExpr* arg = CMemberAccess.make(allocator, e, member, e.type.is_pointer, unpack, f.type);
                                arg = do_arg_cast(unpack, arg);
                                if(arg is null) return null;
                                args ~= arg;
                                arg_idx++;
                            }
                        } else if(e.type.is_array){
                            // Array unpacking
                            if(e.type.array_size == 0){
                                error(tok, "__unpack requires fixed-size array");
                                return null;
                            }
                            CType* elem_type = e.type.pointed_to;
                            foreach(i; 0 .. e.type.array_size){
                                CExpr* idx = CLiteral.make_int(allocator, cast(int)i, unpack);
                                CExpr* arg = CSubscript.make(allocator, e, idx, unpack, elem_type);
                                arg = do_arg_cast(unpack, arg);
                                if(arg is null) return null;
                                args ~= arg;
                                arg_idx++;
                            }
                        } else {
                            error(tok, "argument of __unpack must be a struct, pointer to struct, or fixed-size array");
                            return null;
                        }
                    }
                    else {
                        CToken tok = peek();
                        // Use parse_assignment, not parse_expression, to avoid comma operator
                        CExpr* arg = parse_assignment();
                        if(arg is null) return null;
                        arg = do_arg_cast(tok, arg);
                        if(arg is null) return null;
                        args ~= arg;
                        arg_idx++;
                    }
                } while(match(CTokenType.COMMA));
            }
        }

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after arguments");
        if(ERROR_OCCURRED) return null;

        return CCall.make(allocator, callee, args[], paren, function_type.return_type);
    }

    // GNU statement expression: ({ stmt; stmt; expr; })
    // The value of the expression is the value of the last expression
    CExpr* parse_statement_expr(CToken tok){
        consume(CTokenType.LEFT_BRACE, "Expected '{' after '(' in statement expression");
        if(ERROR_OCCURRED) return null;

        auto statements = make_barray!(CStmt*)(allocator);

        while(!check(CTokenType.RIGHT_BRACE) && !at_end){
            // Try to parse a statement
            CStmt* stmt = parse_statement();
            if(stmt is null) return null;
            statements ~= stmt;
        }

        consume(CTokenType.RIGHT_BRACE, "Expected '}' in statement expression");
        if(ERROR_OCCURRED) return null;
        consume(CTokenType.RIGHT_PAREN, "Expected ')' after statement expression");
        if(ERROR_OCCURRED) return null;

        CExpr* result_expr = null;
        if(statements.count){
            if(CExprStmt *s = statements[$-1].as_expr_stmt)
                result_expr = s.expression;
        }

        return CStmtExpr.make(allocator, statements[], tok, result_expr?result_expr.type:&TYPE_VOID);
    }

    // (6.5.2) primary-expression:
    //     identifier
    //     constant
    //     string-literal
    //     ( expression )
    //     generic-selection
    //     ({ statements opt})     EXTENSION: gnu statement expression
    //     type(params) { body } - EXTENSION: function literal
    CExpr* parse_primary(){
        CToken tok = peek();

        if(match(CTokenType.NUMBER) || match(CTokenType.HEX)){
            return CLiteral.make(allocator, previous(), &TYPE_INT);
        }

        if(match(CTokenType.FLOAT_LITERAL)){
            // Check suffix to determine float vs double
            str lex = previous().lexeme;
            CType* type = &TYPE_DOUBLE;
            if(lex.length > 0){
                char last = lex[$-1];
                if(last == 'f' || last == 'F')
                    type = &TYPE_FLOAT;
            }
            return CLiteral.make(allocator, previous(), type);
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

        if(match(CTokenType.FUNC)){
            if(!current_function_name.length){
                error(previous(), "__func__ used outside of a function");
                return null;
            }
            // Per C standard, __func__ is: static const char __func__[] = "function-name";
            // So it's an array type, not a pointer - sizeof(__func__) == strlen(name) + 1
            CType* array_type = make_array_type(allocator, &TYPE_CHAR, current_function_name.length + 1);
            CLiteral* lit = CLiteral.make(allocator, previous(), array_type).as_literal;
            lit.value.lexeme = mwritef(allocator, "\"%\"", current_function_name)[];
            return &lit.expr;
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
            CExpr* picked = resolve_generic(assocs[], ctrl.type);
            if(!picked){
                error(gen_tok, "None of the types match the _Generic association list");
                return null;
            }
            return CGeneric.make(allocator, ctrl, assocs[], gen_tok, picked, picked.type);
        }

        // EXTENSION: Function literal
        // type(params) { body } - like compound literals but for functions
        if(is_function_literal_start()){
            return parse_function_literal();
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
                return CLiteral.make_size_t(allocator, offset, off_tok);
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
            CToken id_tok = advance();
            // Resolve identifier and create appropriate expression
            // Check local scope first
            if(auto t = lookup_local_variable(id_tok.lexeme)){
                // Check if this is a static local - use mangled name for codegen
                // (static locals are stored as globals but resolved from local scope)
                if(auto mangled = lookup_static_local_name(id_tok.lexeme)){
                    CToken mangled_tok = id_tok;
                    mangled_tok.lexeme = *mangled;
                    // Static locals are stored as globals
                    if(auto idx = *mangled in global_indices){
                        CGlobalVar* gvar = &globals[*idx];
                        if(gvar.is_extern && gvar.library.length > 0)
                            return CIdentifier.make_extern_var(allocator, mangled_tok, t, gvar.library);
                        return CIdentifier.make_global_var(allocator, mangled_tok, t);
                    }
                    return CIdentifier.make_global_var(allocator, mangled_tok, t);
                }
                return CIdentifier.make_local_var(allocator, id_tok, t);
            }
            // Check global scope
            else if(auto t = id_tok.lexeme in global_var_types){
                if(auto idx = id_tok.lexeme in global_indices){
                    CGlobalVar* gvar = &globals[*idx];
                    if(gvar.is_extern && gvar.library.length > 0)
                        return CIdentifier.make_extern_var(allocator, id_tok, *t, gvar.library);
                }
                return CIdentifier.make_global_var(allocator, id_tok, *t);
            }
            // Check inner functions (before global functions)
            else if(auto mangled = lookup_inner_function_name(id_tok.lexeme)){
                if(auto t = *mangled in func_types){
                    CToken mangled_tok = id_tok;
                    mangled_tok.lexeme = *mangled;
                    return CIdentifier.make_func(allocator, mangled_tok, *t);
                }
            }
            // Check functions
            else if(auto t = id_tok.lexeme in func_types){
                if(auto idx = id_tok.lexeme in func_info){
                    CFunction* func = &functions[*idx];
                    // Extern func if it has a library and is not a definition
                    if(func.library.length > 0 && !func.is_definition)
                        return CIdentifier.make_extern_func(allocator, id_tok, *t, func.library);
                }
                return CIdentifier.make_func(allocator, id_tok, *t);
            }
            else if(long* e = lookup_enum_constant(id_tok.lexeme)){
                return CIdentifier.make_enum_const(allocator, id_tok, *e);
            }
            else if(id_tok.lexeme.startswith("__builtin_")){
                return CIdentifier.make_builtin(allocator, id_tok, &TYPE_UNIMPLEMENTED_BUILTIN);
            }
            else if(id_tok.lexeme.startswith("__atomic_")){
                return CIdentifier.make_builtin(allocator, id_tok, &TYPE_UNIMPLEMENTED_BUILTIN);
            }
            else {
                error(id_tok, "Unknown identifier");
                return null;
            }
        }

        if(match(CTokenType.LEFT_PAREN)){
            // Check for GNU statement expression: ({ ... })
            if(check(CTokenType.LEFT_BRACE)){
                return parse_statement_expr(tok);
            }

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
    bool match(CTokenType type, ref CToken tok){
        if(check(type)){
            tok = peek;
            advance();
            return true;
        }
        return false;
    }

    bool match_id(str name){
        if(check(CTokenType.IDENTIFIER)){
            if(peek().lexeme == name){
                advance();
                return true;
            }
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
    static struct Attributes {
        CToken vector_size;
    };

    void parse_gnu_attributes(ref Attributes attr){
        if(match_id("__attribute__")){
            if(!match(CTokenType.LEFT_PAREN)) return;
            int depth = 1;
            while(depth > 0 && !at_end){
                if(match(CTokenType.LEFT_PAREN)){
                    depth++;
                    continue;
                }
                if(match(CTokenType.RIGHT_PAREN)){
                    depth--;
                    continue;
                }
                if(match_id("vector_size") || match_id("__vector_size__")){
                    if(match(CTokenType.LEFT_PAREN)){
                        depth++;
                    }
                    if(match(CTokenType.NUMBER, attr.vector_size)){
                        if(0) errorf(attr.vector_size, "found a vector size: ", attr.vector_size.lexeme);
                    }
                    while(!match(CTokenType.RIGHT_PAREN) && !at_end){
                        advance();
                    }
                    depth--;
                    continue;
                }
                advance();
            }
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

    // Dump all type tables for --debug-types flag
    void dump_type_tables(){
        import core.stdc.stdio : fprintf, stderr;

        if(struct_types.count > 0){
            fprintf(stderr, "\nStruct types (%zu):\n", struct_types.count);
            foreach(ref entry; struct_types.items()){
                str name = entry.key;
                CType* t = entry.value;
                fprintf(stderr, "  struct %.*s: size=%zu",
                    cast(int)name.length, name.ptr,
                    t.struct_size);
                if(t.fields.length > 0){
                    fprintf(stderr, " { ");
                    foreach(i, ref f; t.fields){
                        if(i > 0) fprintf(stderr, ", ");
                        fprintf(stderr, "%.*s", cast(int)f.name.length, f.name.ptr);
                    }
                    fprintf(stderr, " }");
                }
                fprintf(stderr, "\n");
            }
        }

        if(union_types.count > 0){
            fprintf(stderr, "\nUnion types (%zu):\n", union_types.count);
            foreach(ref entry; union_types.items()){
                str name = entry.key;
                CType* t = entry.value;
                fprintf(stderr, "  union %.*s: size=%zu",
                    cast(int)name.length, name.ptr,
                    t.struct_size);
                if(t.fields.length > 0){
                    fprintf(stderr, " { ");
                    foreach(i, ref f; t.fields){
                        if(i > 0) fprintf(stderr, ", ");
                        fprintf(stderr, "%.*s", cast(int)f.name.length, f.name.ptr);
                    }
                    fprintf(stderr, " }");
                }
                fprintf(stderr, "\n");
            }
        }

        if(enum_types.count > 0){
            fprintf(stderr, "\nEnum types (%zu):\n", enum_types.count);
            foreach(ref entry; enum_types.items()){
                str name = entry.key;
                CType* t = entry.value;
                fprintf(stderr, "  enum %.*s\n", cast(int)name.length, name.ptr);
            }
        }

        if(typedef_types.count > 0){
            fprintf(stderr, "\nTypedef types (%zu):\n", typedef_types.count);
            foreach(ref entry; typedef_types.items()){
                str name = entry.key;
                CType* t = entry.value;
                fprintf(stderr, "  typedef %.*s -> ", cast(int)name.length, name.ptr);
                print_type_brief(t);
                fprintf(stderr, "\n");
            }
        }
    }

    void print_type_brief(CType* t){
        import core.stdc.stdio : fprintf, stderr;
        if(t is null){
            fprintf(stderr, "(null)");
            return;
        }
        final switch(t.kind) with(CTypeKind) {
            case UNSET: fprintf(stderr, "UNSET"); break;
            case VOID: fprintf(stderr, "void"); break;
            case CHAR:
                if(!t.is_signed) fprintf(stderr, "unsigned ");
                fprintf(stderr, "char");
                break;
            case SHORT:
                if(!t.is_signed) fprintf(stderr, "unsigned ");
                fprintf(stderr, "short");
                break;
            case INT:
                if(!t.is_signed) fprintf(stderr, "unsigned ");
                fprintf(stderr, "int");
                break;
            case LONG:
                if(!t.is_signed) fprintf(stderr, "unsigned ");
                fprintf(stderr, "long");
                break;
            case LONG_LONG:
                if(!t.is_signed) fprintf(stderr, "unsigned ");
                fprintf(stderr, "long long");
                break;
            case INT128:
                if(!t.is_signed) fprintf(stderr, "unsigned ");
                fprintf(stderr, "__int128");
                break;
            case FLOAT: fprintf(stderr, "float"); break;
            case DOUBLE: fprintf(stderr, "double"); break;
            case LONG_DOUBLE: fprintf(stderr, "long double"); break;
            case POINTER:
                print_type_brief(t.pointed_to);
                fprintf(stderr, "*");
                break;
            case ARRAY:
                print_type_brief(t.pointed_to);
                fprintf(stderr, "[%zu]", t.array_size);
                break;
            case STRUCT:
                fprintf(stderr, "struct %.*s", cast(int)t.struct_name.length, t.struct_name.ptr);
                break;
            case UNION:
                fprintf(stderr, "union %.*s", cast(int)t.struct_name.length, t.struct_name.ptr);
                break;
            case ENUM:
                fprintf(stderr, "enum %.*s", cast(int)t.struct_name.length, t.struct_name.ptr);
                break;
            case FUNCTION:
                fprintf(stderr, "function");
                break;
            case EMBED:
                fprintf(stderr, "EMBED");
                break;
            case INIT_LIST:
                fprintf(stderr, "INIT_LIST");
                break;
            case ANY:
                fprintf(stderr, "ANY");
                break;
            case VECTOR:
                fprintf(stderr, "VECTOR");
                break;
        }
    }
}
