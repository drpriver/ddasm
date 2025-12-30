/*
 * C Front-End Parser for ddasm
 * Copyright 2025, David Priver
 */
module cfront.c_parser;

import core.stdc.stdio : fprintf, stderr;
import dlib.aliases;
import dlib.allocator : Allocator;
import dlib.barray : Barray, make_barray;
import dlib.table : Table;

import cfront.c_tokenizer : CToken, CTokenType;
import cfront.c_ast;

struct CParser {
    Allocator allocator;
    CToken[] tokens;
    int current = 0;
    bool ERROR_OCCURRED = false;
    str current_library;  // Set by #pragma library("...")
    Table!(str, CType*) struct_types;  // Defined struct types
    Table!(str, CType*) union_types;   // Defined union types
    Table!(str, CType*) enum_types;    // Defined enum types
    Table!(str, long) enum_constants;  // Enum constant values (name -> value)
    Table!(str, CType*) typedef_types; // Typedef aliases (name -> type)

    void error(CToken token, str message) {
        ERROR_OCCURRED = true;
        fprintf(stderr, "%.*s:%d:%d: Parse Error at '%.*s': %.*s\n",
                    cast(int)token.file.length, token.file.ptr,
                    token.line, token.column,
                    cast(int)token.lexeme.length, token.lexeme.ptr,
                    cast(int)message.length, message.ptr);
    }

    void error(str message) {
        error(peek(), message);
    }

    // =========================================================================
    // Top-Level Parsing
    // =========================================================================

    int parse(CTranslationUnit* unit) {
        auto functions = make_barray!CFunction(allocator);
        auto externs = make_barray!CExternDecl(allocator);
        auto globals = make_barray!CGlobalVar(allocator);
        auto structs = make_barray!CStructDef(allocator);
        auto unions = make_barray!CUnionDef(allocator);
        auto enums = make_barray!CEnumDef(allocator);

        // Initialize type tables
        struct_types.data.allocator = allocator;
        union_types.data.allocator = allocator;
        enum_types.data.allocator = allocator;
        enum_constants.data.allocator = allocator;
        typedef_types.data.allocator = allocator;

        // Add predefined typedefs for common C types
        typedef_types["size_t"] = &TYPE_ULONG;
        typedef_types["ssize_t"] = &TYPE_LONG;
        typedef_types["ptrdiff_t"] = &TYPE_LONG;
        typedef_types["intptr_t"] = &TYPE_LONG;
        typedef_types["uintptr_t"] = &TYPE_ULONG;
        typedef_types["wchar_t"] = &TYPE_INT;
        typedef_types["wint_t"] = &TYPE_UINT;
        typedef_types["va_list"] = &TYPE_VOID_PTR;

        while (!at_end) {
            // Handle #pragma
            if (check(CTokenType.PRAGMA)) {
                handle_pragma();
                continue;
            }

            // Skip empty statements (from macros that expand to nothing)
            if (match(CTokenType.SEMICOLON)) {
                continue;
            }

            // Parse extern or function/global
            if (check(CTokenType.EXTERN)) {
                CExternDecl ext;
                int err = parse_extern_decl(&ext);
                if (err) return err;
                externs ~= ext;
            } else if (check(CTokenType.STRUCT)) {
                // Check if this is a struct definition or forward declaration
                // Look ahead: struct Name { ... means definition
                //             struct Name ; means forward declaration
                //             struct Name var... means variable of struct type
                if (peek_at(1).type == CTokenType.IDENTIFIER &&
                    peek_at(2).type == CTokenType.LEFT_BRACE) {
                    CStructDef sdef;
                    int err = parse_struct_def(&sdef);
                    if (err) return err;
                    structs ~= sdef;
                } else if (peek_at(1).type == CTokenType.IDENTIFIER &&
                           peek_at(2).type == CTokenType.SEMICOLON) {
                    // Forward declaration: struct Name;
                    advance();  // consume 'struct'
                    CToken struct_name = advance();  // consume name
                    advance();  // consume ';'

                    // Create incomplete struct type if not already defined
                    if ((struct_name.lexeme in struct_types) is null) {
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
                    if (type_ is null) return 1;

                    CToken name = consume(CTokenType.IDENTIFIER, "Expected identifier");
                    if (ERROR_OCCURRED) return 1;

                    if (check(CTokenType.LEFT_PAREN)) {
                        CFunction func;
                        int err = parse_function_rest(type_, name, &func);
                        if (err) return err;
                        functions ~= func;
                    } else {
                        CGlobalVar gvar;
                        int err = parse_global_var_rest(type_, name, &gvar);
                        if (err) return err;
                        globals ~= gvar;
                    }
                }
            } else if (check(CTokenType.UNION)) {
                // Check if this is a union definition or forward declaration
                // Look ahead: union Name { ... means definition
                //             union Name ; means forward declaration
                //             union Name var... means variable of union type
                if (peek_at(1).type == CTokenType.IDENTIFIER &&
                    peek_at(2).type == CTokenType.LEFT_BRACE) {
                    CUnionDef udef;
                    int err = parse_union_def(&udef);
                    if (err) return err;
                    unions ~= udef;
                } else if (peek_at(1).type == CTokenType.IDENTIFIER &&
                           peek_at(2).type == CTokenType.SEMICOLON) {
                    // Forward declaration: union Name;
                    advance();  // consume 'union'
                    CToken union_name = advance();  // consume name
                    advance();  // consume ';'

                    // Create incomplete union type if not already defined
                    if ((union_name.lexeme in union_types) is null) {
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
                    if (type_ is null) return 1;

                    CToken name = consume(CTokenType.IDENTIFIER, "Expected identifier");
                    if (ERROR_OCCURRED) return 1;

                    if (check(CTokenType.LEFT_PAREN)) {
                        CFunction func;
                        int err = parse_function_rest(type_, name, &func);
                        if (err) return err;
                        functions ~= func;
                    } else {
                        CGlobalVar gvar;
                        int err = parse_global_var_rest(type_, name, &gvar);
                        if (err) return err;
                        globals ~= gvar;
                    }
                }
            } else if (check(CTokenType.ENUM)) {
                // Check if this is an enum definition
                // Look ahead: enum Name { ... means named definition
                //             enum { ... means anonymous definition
                //             enum Name var... means variable of enum type
                bool is_anon_enum = peek_at(1).type == CTokenType.LEFT_BRACE;
                bool is_named_enum = peek_at(1).type == CTokenType.IDENTIFIER &&
                                     peek_at(2).type == CTokenType.LEFT_BRACE;
                if (is_anon_enum || is_named_enum) {
                    CEnumDef edef;
                    int err = parse_enum_def(&edef);
                    if (err) return err;
                    enums ~= edef;
                } else {
                    // It's a variable/function with enum type
                    CType* type_ = parse_type();
                    if (type_ is null) return 1;

                    CToken name = consume(CTokenType.IDENTIFIER, "Expected identifier");
                    if (ERROR_OCCURRED) return 1;

                    if (check(CTokenType.LEFT_PAREN)) {
                        CFunction func;
                        int err = parse_function_rest(type_, name, &func);
                        if (err) return err;
                        functions ~= func;
                    } else {
                        CGlobalVar gvar;
                        int err = parse_global_var_rest(type_, name, &gvar);
                        if (err) return err;
                        globals ~= gvar;
                    }
                }
            } else if (check(CTokenType.TYPEDEF)) {
                // Parse typedef declaration
                int err = parse_typedef(&enums);
                if (err) return err;
            } else if (match(CTokenType.STATIC_ASSERT)) {
                // _Static_assert(expr, "message");
                int err = parse_static_assert();
                if (err) return err;
            } else if (match(CTokenType.STATIC)) {
                // Static function or variable - skip for now (glibc inline functions)
                // Just consume until semicolon or closing brace
                int brace_depth = 0;
                while (!at_end) {
                    if (check(CTokenType.LEFT_BRACE)) {
                        brace_depth++;
                        advance();
                    } else if (check(CTokenType.RIGHT_BRACE)) {
                        brace_depth--;
                        advance();
                        if (brace_depth == 0) break;
                    } else if (check(CTokenType.SEMICOLON) && brace_depth == 0) {
                        advance();
                        break;
                    } else {
                        advance();
                    }
                }
            } else if (check(CTokenType.IDENTIFIER) && peek_at(1).type == CTokenType.LEFT_PAREN &&
                       (peek().lexeme in typedef_types) is null) {
                // Unknown identifier (not a typedef) followed by ( - likely a function-like macro invocation
                // Skip it: identifier(...);
                advance();  // consume identifier
                skip_balanced_parens();
                match(CTokenType.SEMICOLON);  // consume optional semicolon
            } else {
                // Parse type and name, then decide if it's a function or global
                CType* type_ = parse_type();
                if (type_ is null) return 1;

                CToken name = consume(CTokenType.IDENTIFIER, "Expected identifier");
                if (ERROR_OCCURRED) return 1;

                if (check(CTokenType.LEFT_PAREN)) {
                    // It's a function
                    CFunction func;
                    int err = parse_function_rest(type_, name, &func);
                    if (err) return err;
                    functions ~= func;
                } else {
                    // It's a global variable
                    CGlobalVar gvar;
                    int err = parse_global_var_rest(type_, name, &gvar);
                    if (err) return err;
                    globals ~= gvar;
                }
            }
        }

        unit.functions = functions[];
        unit.externs = externs[];
        unit.globals = globals[];
        unit.structs = structs[];
        unit.unions = unions[];
        unit.enums = enums[];
        unit.current_library = current_library;
        return 0;
    }

    void handle_pragma() {
        CToken pragma_tok = advance();  // consume PRAGMA
        // The pragma lexeme includes everything after #pragma
        // Look for: library("...")
        str content = pragma_tok.lexeme;
        // Skip "#pragma "
        if (content.length > 8) {
            content = content[8 .. $];
            // Trim leading whitespace
            while (content.length && (content[0] == ' ' || content[0] == '\t'))
                content = content[1 .. $];
            // Check for library("...")
            if (content.length > 9 && content[0 .. 8] == "library(") {
                content = content[8 .. $];
                // Find the string
                if (content.length > 2 && content[0] == '"') {
                    size_t end = 1;
                    while (end < content.length && content[end] != '"') end++;
                    if (end < content.length) {
                        current_library = content[1 .. end];
                    }
                }
            }
        }
    }

    int parse_struct_def(CStructDef* sdef) {
        advance();  // consume 'struct'
        CToken name = consume(CTokenType.IDENTIFIER, "Expected struct name");
        if (ERROR_OCCURRED) return 1;

        consume(CTokenType.LEFT_BRACE, "Expected '{' after struct name");
        if (ERROR_OCCURRED) return 1;

        // Parse fields
        auto fields = make_barray!StructField(allocator);
        size_t offset = 0;

        while (!check(CTokenType.RIGHT_BRACE) && !at_end) {
            // Check for anonymous union/struct: union { ... } or struct { ... }
            if ((check(CTokenType.UNION) || check(CTokenType.STRUCT)) &&
                peek_at(1).type == CTokenType.LEFT_BRACE) {
                // Skip anonymous union/struct - consume until matching brace and semicolon
                advance();  // consume union/struct
                advance();  // consume {
                int brace_depth = 1;
                while (!at_end && brace_depth > 0) {
                    if (check(CTokenType.LEFT_BRACE)) brace_depth++;
                    else if (check(CTokenType.RIGHT_BRACE)) brace_depth--;
                    advance();
                }
                if (check(CTokenType.SEMICOLON)) advance();  // optional trailing ;
                continue;
            }

            // Check for anonymous enum field: enum { ... } field_name;
            if (check(CTokenType.ENUM) && peek_at(1).type == CTokenType.LEFT_BRACE) {
                advance();  // consume 'enum'
                advance();  // consume '{'
                // Parse and register enum values
                long enum_val = 0;
                while (!check(CTokenType.RIGHT_BRACE) && !at_end) {
                    CToken ename = consume(CTokenType.IDENTIFIER, "Expected enum constant name");
                    if (ERROR_OCCURRED) return 1;
                    if (match(CTokenType.EQUAL)) {
                        auto result = parse_enum_const_expr();
                        if (result.err) return 1;
                        enum_val = result.value;
                    }
                    enum_constants[ename.lexeme] = enum_val;
                    enum_val++;
                    if (!match(CTokenType.COMMA)) break;
                }
                consume(CTokenType.RIGHT_BRACE, "Expected '}'");
                if (ERROR_OCCURRED) return 1;
                // Now parse the field name
                CToken field_name = consume(CTokenType.IDENTIFIER, "Expected field name");
                if (ERROR_OCCURRED) return 1;
                StructField field;
                field.name = field_name.lexeme;
                field.type = &TYPE_INT;  // enum is int
                field.offset = offset;
                fields ~= field;
                offset += TYPE_INT.size_of();
                consume(CTokenType.SEMICOLON, "Expected ';'");
                if (ERROR_OCCURRED) return 1;
                continue;
            }

            // Parse field type
            CType* field_type = parse_type();
            if (field_type is null) return 1;

            // Check for function pointer field: type (CONV * name) (params)
            CToken field_name;
            if (check(CTokenType.LEFT_PAREN)) {
                advance();  // consume '('
                // Skip calling convention identifiers until we hit '*'
                while (check(CTokenType.IDENTIFIER)) {
                    advance();
                }
                if (!match(CTokenType.STAR)) {
                    error("Expected '*' in function pointer field");
                    return 1;
                }
                // Skip const after * (e.g., (*const funcptr))
                match(CTokenType.CONST);
                field_name = consume(CTokenType.IDENTIFIER, "Expected field name");
                if (ERROR_OCCURRED) return 1;
                consume(CTokenType.RIGHT_PAREN, "Expected ')' after field name");
                if (ERROR_OCCURRED) return 1;
                // Skip the parameter list
                if (check(CTokenType.LEFT_PAREN)) {
                    skip_balanced_parens();
                }
                // Treat as void* for simplicity
                field_type = make_pointer_type(allocator, &TYPE_VOID);
            } else {
                // Check for anonymous bit field (e.g., unsigned int :24;)
                if (check(CTokenType.COLON)) {
                    advance();  // consume :
                    auto width_result = parse_enum_const_expr();
                    if (width_result.err) return 1;
                    consume(CTokenType.SEMICOLON, "Expected ';' after anonymous bit field");
                    if (ERROR_OCCURRED) return 1;
                    // Skip anonymous bit fields - they're just padding
                    continue;
                }

                // Regular field name
                field_name = consume(CTokenType.IDENTIFIER, "Expected field name");
                if (ERROR_OCCURRED) return 1;

                // Handle arrays
                if (match(CTokenType.LEFT_BRACKET)) {
                    auto size_result = parse_enum_const_expr();
                    if (size_result.err) return 1;
                    if (size_result.value <= 0) {
                        error("Array size must be positive");
                        return 1;
                    }
                    consume(CTokenType.RIGHT_BRACKET, "Expected ']' after array size");
                    if (ERROR_OCCURRED) return 1;
                    field_type = make_array_type(allocator, field_type, cast(size_t) size_result.value);
                }

                // Handle bit fields (e.g., unsigned int field:2;)
                if (match(CTokenType.COLON)) {
                    // Just skip the bit width - we'll use the full type size
                    auto width_result = parse_enum_const_expr();
                    if (width_result.err) return 1;
                    // Bit field - just use the underlying type (imprecise but ok for now)
                }
            }

            consume(CTokenType.SEMICOLON, "Expected ';' after field declaration");
            if (ERROR_OCCURRED) return 1;

            // Add field with current offset
            StructField field;
            field.name = field_name.lexeme;
            field.type = field_type;
            field.offset = offset;
            fields ~= field;

            // Update offset (simple packing, no alignment)
            offset += field_type.size_of();
        }

        consume(CTokenType.RIGHT_BRACE, "Expected '}' after struct fields");
        if (ERROR_OCCURRED) return 1;

        consume(CTokenType.SEMICOLON, "Expected ';' after struct definition");
        if (ERROR_OCCURRED) return 1;

        // Create the struct type
        CType* struct_type = make_struct_type(allocator, name.lexeme, fields[], offset);

        // Register the struct type
        struct_types[name.lexeme] = struct_type;

        sdef.name = name;
        sdef.struct_type = struct_type;
        return 0;
    }

    int parse_union_def(CUnionDef* udef) {
        advance();  // consume 'union'
        CToken name = consume(CTokenType.IDENTIFIER, "Expected union name");
        if (ERROR_OCCURRED) return 1;

        consume(CTokenType.LEFT_BRACE, "Expected '{' after union name");
        if (ERROR_OCCURRED) return 1;

        // Parse fields
        auto fields = make_barray!StructField(allocator);
        size_t max_size = 0;

        while (!check(CTokenType.RIGHT_BRACE) && !at_end) {
            // Check for anonymous union/struct: union { ... } or struct { ... }
            if ((check(CTokenType.UNION) || check(CTokenType.STRUCT)) &&
                peek_at(1).type == CTokenType.LEFT_BRACE) {
                // Skip anonymous union/struct - consume until matching brace and semicolon
                advance();  // consume union/struct
                advance();  // consume {
                int brace_depth = 1;
                while (!at_end && brace_depth > 0) {
                    if (check(CTokenType.LEFT_BRACE)) brace_depth++;
                    else if (check(CTokenType.RIGHT_BRACE)) brace_depth--;
                    advance();
                }
                if (check(CTokenType.SEMICOLON)) advance();  // optional trailing ;
                continue;
            }

            // Parse field type
            CType* field_type = parse_type();
            if (field_type is null) return 1;

            // Check for anonymous bit field
            if (check(CTokenType.COLON)) {
                advance();  // consume :
                auto width_result = parse_enum_const_expr();
                if (width_result.err) return 1;
                consume(CTokenType.SEMICOLON, "Expected ';' after anonymous bit field");
                if (ERROR_OCCURRED) return 1;
                continue;  // Skip anonymous bit fields
            }

            // Parse field name
            CToken field_name = consume(CTokenType.IDENTIFIER, "Expected field name");
            if (ERROR_OCCURRED) return 1;

            // Handle arrays
            if (match(CTokenType.LEFT_BRACKET)) {
                auto size_result = parse_enum_const_expr();
                if (size_result.err) return 1;
                if (size_result.value <= 0) {
                    error("Array size must be positive");
                    return 1;
                }
                consume(CTokenType.RIGHT_BRACKET, "Expected ']' after array size");
                if (ERROR_OCCURRED) return 1;
                field_type = make_array_type(allocator, field_type, cast(size_t) size_result.value);
            }

            // Handle bit fields
            if (match(CTokenType.COLON)) {
                auto width_result = parse_enum_const_expr();
                if (width_result.err) return 1;
            }

            consume(CTokenType.SEMICOLON, "Expected ';' after field declaration");
            if (ERROR_OCCURRED) return 1;

            // Add field - all union fields have offset 0
            StructField field;
            field.name = field_name.lexeme;
            field.type = field_type;
            field.offset = 0;  // All union members start at offset 0
            fields ~= field;

            // Track max size
            size_t field_size = field_type.size_of();
            if (field_size > max_size) max_size = field_size;
        }

        consume(CTokenType.RIGHT_BRACE, "Expected '}' after union fields");
        if (ERROR_OCCURRED) return 1;

        consume(CTokenType.SEMICOLON, "Expected ';' after union definition");
        if (ERROR_OCCURRED) return 1;

        // Create the union type
        CType* union_type = make_union_type(allocator, name.lexeme, fields[], max_size);

        // Register the union type
        union_types[name.lexeme] = union_type;

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
    ConstExprResult parse_enum_const_expr() {
        return parse_enum_const_ternary();
    }

    // Ternary operator - lowest precedence
    ConstExprResult parse_enum_const_ternary() {
        auto result = parse_enum_const_or();
        if (result.err) return result;

        if (check(CTokenType.QUESTION)) {
            advance();  // consume ?
            auto if_true = parse_enum_const_expr();  // Recursive for nested ternary
            if (if_true.err) return if_true;

            consume(CTokenType.COLON, "Expected ':' in ternary expression");
            if (ERROR_OCCURRED) {
                result.err = true;
                return result;
            }

            auto if_false = parse_enum_const_ternary();
            if (if_false.err) return if_false;

            result.value = result.value != 0 ? if_true.value : if_false.value;
        }
        return result;
    }

    // Bitwise OR - lowest precedence of bitwise ops
    ConstExprResult parse_enum_const_or() {
        auto result = parse_enum_const_xor();
        if (result.err) return result;

        while (check(CTokenType.PIPE)) {
            advance();
            auto right = parse_enum_const_xor();
            if (right.err) return right;
            result.value = result.value | right.value;
        }
        return result;
    }

    // Bitwise XOR
    ConstExprResult parse_enum_const_xor() {
        auto result = parse_enum_const_and();
        if (result.err) return result;

        while (check(CTokenType.CARET)) {
            advance();
            auto right = parse_enum_const_and();
            if (right.err) return right;
            result.value = result.value ^ right.value;
        }
        return result;
    }

    // Bitwise AND
    ConstExprResult parse_enum_const_and() {
        auto result = parse_enum_const_equality();
        if (result.err) return result;

        while (check(CTokenType.AMP)) {
            advance();
            auto right = parse_enum_const_equality();
            if (right.err) return right;
            result.value = result.value & right.value;
        }
        return result;
    }

    ConstExprResult parse_enum_const_equality() {
        auto result = parse_enum_const_relational();
        if (result.err) return result;

        while (check(CTokenType.EQUAL_EQUAL) || check(CTokenType.BANG_EQUAL)) {
            bool is_eq = check(CTokenType.EQUAL_EQUAL);
            advance();
            auto right = parse_enum_const_relational();
            if (right.err) return right;
            if (is_eq) {
                result.value = result.value == right.value ? 1 : 0;
            } else {
                result.value = result.value != right.value ? 1 : 0;
            }
        }
        return result;
    }

    ConstExprResult parse_enum_const_relational() {
        auto result = parse_enum_const_shift();
        if (result.err) return result;

        while (check(CTokenType.LESS) || check(CTokenType.GREATER) ||
               check(CTokenType.LESS_EQUAL) || check(CTokenType.GREATER_EQUAL)) {
            CTokenType op = peek().type;
            advance();
            auto right = parse_enum_const_shift();
            if (right.err) return right;
            if (op == CTokenType.LESS) {
                result.value = result.value < right.value ? 1 : 0;
            } else if (op == CTokenType.GREATER) {
                result.value = result.value > right.value ? 1 : 0;
            } else if (op == CTokenType.LESS_EQUAL) {
                result.value = result.value <= right.value ? 1 : 0;
            } else {
                result.value = result.value >= right.value ? 1 : 0;
            }
        }
        return result;
    }

    // Shift operators
    ConstExprResult parse_enum_const_shift() {
        auto result = parse_enum_const_additive();
        if (result.err) return result;

        while (check(CTokenType.LESS_LESS) || check(CTokenType.GREATER_GREATER)) {
            bool is_left = check(CTokenType.LESS_LESS);
            advance();
            auto right = parse_enum_const_additive();
            if (right.err) return right;
            if (is_left) {
                result.value = result.value << right.value;
            } else {
                result.value = result.value >> right.value;
            }
        }
        return result;
    }

    ConstExprResult parse_enum_const_additive() {
        auto result = parse_enum_const_multiplicative();
        if (result.err) return result;

        while (check(CTokenType.PLUS) || check(CTokenType.MINUS)) {
            bool is_plus = check(CTokenType.PLUS);
            advance();
            auto right = parse_enum_const_multiplicative();
            if (right.err) return right;
            if (is_plus) {
                result.value = result.value + right.value;
            } else {
                result.value = result.value - right.value;
            }
        }
        return result;
    }

    ConstExprResult parse_enum_const_multiplicative() {
        auto result = parse_enum_const_unary();
        if (result.err) return result;

        while (check(CTokenType.STAR) || check(CTokenType.SLASH) || check(CTokenType.PERCENT)) {
            CTokenType op = peek().type;
            advance();
            auto right = parse_enum_const_unary();
            if (right.err) return right;
            if (op == CTokenType.STAR) {
                result.value = result.value * right.value;
            } else if (op == CTokenType.SLASH) {
                if (right.value != 0) {
                    result.value = result.value / right.value;
                } else {
                    result.value = 0;  // Avoid division by zero
                }
            } else {
                if (right.value != 0) {
                    result.value = result.value % right.value;
                } else {
                    result.value = 0;
                }
            }
        }
        return result;
    }

    ConstExprResult parse_enum_const_unary() {
        if (match(CTokenType.MINUS)) {
            auto result = parse_enum_const_primary();
            if (result.err) return result;
            result.value = -result.value;
            return result;
        }
        if (match(CTokenType.PLUS)) {
            return parse_enum_const_primary();
        }
        return parse_enum_const_primary();
    }

    ConstExprResult parse_enum_const_primary() {
        ConstExprResult result;
        result.err = false;

        if (match(CTokenType.NUMBER)) {
            CToken tok = previous();
            long value = 0;
            foreach (c; tok.lexeme) {
                value = value * 10 + (c - '0');
            }
            result.value = value;
            return result;
        }

        if (match(CTokenType.HEX)) {
            CToken tok = previous();
            long value = 0;
            str lexeme = tok.lexeme;
            // Skip "0x" or "0X" prefix
            foreach (c; lexeme[2 .. $]) {
                if (c >= '0' && c <= '9') {
                    value = value * 16 + (c - '0');
                } else if (c >= 'a' && c <= 'f') {
                    value = value * 16 + (c - 'a' + 10);
                } else if (c >= 'A' && c <= 'F') {
                    value = value * 16 + (c - 'A' + 10);
                }
            }
            result.value = value;
            return result;
        }

        if (match(CTokenType.CHAR_LITERAL)) {
            CToken tok = previous();
            str lexeme = tok.lexeme;
            // Lexeme is 'c' or '\x' or '\nnn' (with quotes)
            if (lexeme.length >= 3 && lexeme[1] == '\\') {
                // Escape sequence
                char escape_char = lexeme[2];
                switch (escape_char) {
                    case 'n': result.value = '\n'; break;
                    case 'r': result.value = '\r'; break;
                    case 't': result.value = '\t'; break;
                    case 'b': result.value = '\b'; break;
                    case '\\': result.value = '\\'; break;
                    case '\'': result.value = '\''; break;
                    case '"': result.value = '"'; break;
                    case '0':
                        // Could be \0 or \0nn (octal)
                        if (lexeme.length == 4) {
                            result.value = 0;
                        } else {
                            // Octal escape
                            long val = 0;
                            foreach (c; lexeme[2 .. $ - 1]) {
                                if (c >= '0' && c <= '7') {
                                    val = val * 8 + (c - '0');
                                }
                            }
                            result.value = val;
                        }
                        break;
                    default:
                        // May be octal \nnn
                        if (escape_char >= '0' && escape_char <= '7') {
                            long val = 0;
                            foreach (c; lexeme[2 .. $ - 1]) {
                                if (c >= '0' && c <= '7') {
                                    val = val * 8 + (c - '0');
                                }
                            }
                            result.value = val;
                        } else {
                            result.value = escape_char;
                        }
                }
            } else if (lexeme.length >= 3) {
                // Simple character 'c'
                result.value = lexeme[1];
            }
            return result;
        }

        if (match(CTokenType.IDENTIFIER)) {
            CToken tok = previous();
            // Look up in already-defined enum constants
            if (long* val = tok.lexeme in enum_constants) {
                result.value = *val;
                return result;
            }
            // Check for function-like macro call (unknown macro with parens)
            if (check(CTokenType.LEFT_PAREN)) {
                // Skip the function-like macro call, treat as 0
                skip_balanced_parens();
                result.value = 0;
                return result;
            }
            error(tok, "Unknown enum constant in expression");
            result.err = true;
            return result;
        }

        if (match(CTokenType.LEFT_PAREN)) {
            // Check if this is a cast expression: (type)value
            if (is_type_specifier(peek())) {
                // It's a cast - parse and ignore the type, then parse the value
                CType* cast_type = parse_type();
                if (cast_type is null) {
                    result.err = true;
                    return result;
                }
                consume(CTokenType.RIGHT_PAREN, "Expected ')' after cast type");
                if (ERROR_OCCURRED) {
                    result.err = true;
                    return result;
                }
                // Parse the value being cast
                return parse_enum_const_unary();
            }
            // Regular parenthesized expression
            result = parse_enum_const_expr();
            if (result.err) return result;
            consume(CTokenType.RIGHT_PAREN, "Expected ')' after expression");
            if (ERROR_OCCURRED) {
                result.err = true;
            }
            return result;
        }

        // sizeof(type) in constant expression
        if (match(CTokenType.SIZEOF)) {
            consume(CTokenType.LEFT_PAREN, "Expected '(' after sizeof");
            if (ERROR_OCCURRED) {
                result.err = true;
                return result;
            }

            CType* type = parse_type();
            if (type is null) {
                result.err = true;
                return result;
            }

            consume(CTokenType.RIGHT_PAREN, "Expected ')' after type");
            if (ERROR_OCCURRED) {
                result.err = true;
                return result;
            }

            result.value = cast(long) type.size_of();
            return result;
        }

        error("Expected constant value in enum expression");
        result.err = true;
        return result;
    }

    int parse_enum_def(CEnumDef* edef) {
        advance();  // consume 'enum'

        // Name is optional (anonymous enum)
        CToken name;
        bool has_name = false;
        if (check(CTokenType.IDENTIFIER)) {
            name = advance();
            has_name = true;
        }

        consume(CTokenType.LEFT_BRACE, "Expected '{' after enum");
        if (ERROR_OCCURRED) return 1;

        // Parse enum constants
        auto constants = make_barray!EnumConstant(allocator);
        long next_value = 0;

        while (!check(CTokenType.RIGHT_BRACE) && !at_end) {
            // Parse constant name
            CToken const_name = consume(CTokenType.IDENTIFIER, "Expected enum constant name");
            if (ERROR_OCCURRED) return 1;

            // Check for explicit value assignment
            long value = next_value;
            if (match(CTokenType.EQUAL)) {
                // Parse constant expression (supports literals, enum constants, +/-)
                auto result = parse_enum_const_expr();
                if (result.err) return 1;
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
            if (!check(CTokenType.RIGHT_BRACE)) {
                if (!match(CTokenType.COMMA)) {
                    // Allow trailing comma or no comma before }
                    if (!check(CTokenType.RIGHT_BRACE)) {
                        error("Expected ',' or '}' after enum constant");
                        return 1;
                    }
                }
            }
        }

        consume(CTokenType.RIGHT_BRACE, "Expected '}' after enum constants");
        if (ERROR_OCCURRED) return 1;

        consume(CTokenType.SEMICOLON, "Expected ';' after enum definition");
        if (ERROR_OCCURRED) return 1;

        // Create the enum type
        CType* enum_type = make_enum_type(allocator, has_name ? name.lexeme : "");

        // Register the enum type if named
        if (has_name) {
            enum_types[name.lexeme] = enum_type;
        }

        edef.name = has_name ? name : CToken.init;
        edef.enum_type = enum_type;
        edef.constants = constants[];
        return 0;
    }

    // Parse _Static_assert(constant_expr, "message");
    int parse_static_assert() {
        consume(CTokenType.LEFT_PAREN, "Expected '(' after _Static_assert");
        if (ERROR_OCCURRED) return 1;

        // Parse constant expression using the enum constant expression parser
        auto result = parse_enum_const_expr();
        if (result.err) return 1;

        consume(CTokenType.COMMA, "Expected ',' after expression");
        if (ERROR_OCCURRED) return 1;

        CToken message = consume(CTokenType.STRING, "Expected string message");
        if (ERROR_OCCURRED) return 1;

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after message");
        if (ERROR_OCCURRED) return 1;

        consume(CTokenType.SEMICOLON, "Expected ';' after _Static_assert");
        if (ERROR_OCCURRED) return 1;

        // Check the assertion
        if (result.value == 0) {
            // Strip quotes from message for error output
            str msg = message.lexeme;
            if (msg.length >= 2 && msg[0] == '"' && msg[$ - 1] == '"') {
                msg = msg[1 .. $ - 1];
            }
            error(message, msg);
            return 1;
        }

        return 0;
    }

    // Parse typedef declaration
    // Supports: typedef <type> <name>;
    //           typedef struct { ... } Name;
    //           typedef struct Name { ... } Alias;
    //           typedef enum { ... } Name;
    int parse_typedef(Barray!(CEnumDef)* enums_out) {
        advance();  // consume 'typedef'

        // Check for struct/union/enum definition within typedef
        if (check(CTokenType.STRUCT)) {
            advance();  // consume 'struct'

            // Check if there's a name and/or brace
            CToken struct_name;
            bool has_name = false;
            bool has_body = false;

            if (check(CTokenType.IDENTIFIER)) {
                struct_name = advance();
                has_name = true;
            }
            if (check(CTokenType.LEFT_BRACE)) {
                has_body = true;
            }

            CType* struct_type;

            if (has_body) {
                // Parse struct body
                consume(CTokenType.LEFT_BRACE, "Expected '{'");
                if (ERROR_OCCURRED) return 1;

                auto fields = make_barray!StructField(allocator);
                size_t offset = 0;

                while (!check(CTokenType.RIGHT_BRACE) && !at_end) {
                    // Check for anonymous enum field: enum { ... } field_name;
                    if (check(CTokenType.ENUM)) {
                        advance();  // consume 'enum'
                        if (check(CTokenType.LEFT_BRACE)) {
                            advance();  // consume '{'
                            // Parse and skip enum values
                            long enum_val = 0;
                            while (!check(CTokenType.RIGHT_BRACE) && !at_end) {
                                CToken name = consume(CTokenType.IDENTIFIER, "Expected enum constant name");
                                if (ERROR_OCCURRED) return 1;
                                if (match(CTokenType.EQUAL)) {
                                    auto result = parse_enum_const_expr();
                                    if (result.err) return 1;
                                    enum_val = result.value;
                                }
                                enum_constants[name.lexeme] = enum_val;
                                enum_val++;
                                if (!match(CTokenType.COMMA)) break;
                            }
                            consume(CTokenType.RIGHT_BRACE, "Expected '}'");
                            if (ERROR_OCCURRED) return 1;
                            // Now parse the field name
                            CToken field_name = consume(CTokenType.IDENTIFIER, "Expected field name");
                            if (ERROR_OCCURRED) return 1;
                            StructField field;
                            field.name = field_name.lexeme;
                            field.type = &TYPE_INT;  // enum is int
                            field.offset = offset;
                            fields ~= field;
                            offset += TYPE_INT.size_of();
                            consume(CTokenType.SEMICOLON, "Expected ';'");
                            if (ERROR_OCCURRED) return 1;
                            continue;
                        } else {
                            // Named enum, put token back and let parse_type handle it
                            current--;
                        }
                    }

                    CType* field_type = parse_type();
                    if (field_type is null) return 1;

                    // Check for function pointer field: type (CONV * name) (params)
                    if (check(CTokenType.LEFT_PAREN)) {
                        advance();  // consume '('
                        while (check(CTokenType.IDENTIFIER)) advance();
                        if (!match(CTokenType.STAR)) { error("Expected '*' in function pointer field"); return 1; }
                        match(CTokenType.CONST);  // Skip const after * (e.g., (*const funcptr))
                        CToken field_name = consume(CTokenType.IDENTIFIER, "Expected field name");
                        if (ERROR_OCCURRED) return 1;
                        consume(CTokenType.RIGHT_PAREN, "Expected ')' after field name");
                        if (ERROR_OCCURRED) return 1;
                        if (check(CTokenType.LEFT_PAREN)) skip_balanced_parens();

                        StructField field;
                        field.name = field_name.lexeme;
                        field.type = make_pointer_type(allocator, &TYPE_VOID);
                        field.offset = offset;
                        fields ~= field;
                        offset += field.type.size_of();
                    } else {
                        // Check for anonymous bit field first (before comma loop)
                        if (check(CTokenType.COLON)) {
                            advance();  // consume :
                            auto width_result = parse_enum_const_expr();
                            if (width_result.err) return 1;
                            consume(CTokenType.SEMICOLON, "Expected ';' after anonymous bit field");
                            if (ERROR_OCCURRED) return 1;
                            continue;  // Skip to next field
                        }

                        // Handle comma-separated field names: int x, y; or int *x, *y;
                        do {
                            CType* this_field_type = field_type;

                            // Each comma-separated name can have its own pointer levels
                            while (match(CTokenType.STAR)) {
                                this_field_type = make_pointer_type(allocator, this_field_type);
                            }

                            CToken field_name = consume(CTokenType.IDENTIFIER, "Expected field name");
                            if (ERROR_OCCURRED) return 1;

                            // Handle arrays
                            if (match(CTokenType.LEFT_BRACKET)) {
                                auto size_result = parse_enum_const_expr();
                                if (size_result.err) return 1;
                                if (size_result.value <= 0) {
                                    error("Array size must be positive");
                                    return 1;
                                }
                                consume(CTokenType.RIGHT_BRACKET, "Expected ']'");
                                if (ERROR_OCCURRED) return 1;
                                this_field_type = make_array_type(allocator, this_field_type, cast(size_t) size_result.value);
                            }

                            // Handle bit fields
                            if (match(CTokenType.COLON)) {
                                auto width_result = parse_enum_const_expr();
                                if (width_result.err) return 1;
                            }

                            StructField field;
                            field.name = field_name.lexeme;
                            field.type = this_field_type;
                            field.offset = offset;
                            fields ~= field;
                            offset += this_field_type.size_of();
                        } while (match(CTokenType.COMMA));
                    }

                    consume(CTokenType.SEMICOLON, "Expected ';' after field");
                    if (ERROR_OCCURRED) return 1;
                }

                consume(CTokenType.RIGHT_BRACE, "Expected '}'");
                if (ERROR_OCCURRED) return 1;

                struct_type = make_struct_type(allocator, has_name ? struct_name.lexeme : "", fields[], offset);

                // If named, also register as struct type
                if (has_name) {
                    struct_types[struct_name.lexeme] = struct_type;
                }
            } else {
                // Just referencing existing struct
                if (!has_name) {
                    error("Expected struct name or body");
                    return 1;
                }
                if (CType** found = struct_name.lexeme in struct_types) {
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
            while (match(CTokenType.STAR)) {
                struct_type = make_pointer_type(allocator, struct_type);
                // Skip pointer qualifiers
                while (match(CTokenType.CONST) || match(CTokenType.VOLATILE)) {}
            }

            // Check for function pointer typedef: typedef struct X *(*name)(...)
            if (check(CTokenType.LEFT_PAREN)) {
                advance();  // consume '('

                // Skip any calling convention identifiers until we hit '*'
                while (check(CTokenType.IDENTIFIER)) {
                    advance();
                }

                if (!match(CTokenType.STAR)) {
                    error("Expected '*' in function pointer typedef");
                    return 1;
                }

                CToken typedef_name = consume(CTokenType.IDENTIFIER, "Expected typedef name");
                if (ERROR_OCCURRED) return 1;

                consume(CTokenType.RIGHT_PAREN, "Expected ')' after function pointer name");
                if (ERROR_OCCURRED) return 1;

                // Now parse the parameter list
                consume(CTokenType.LEFT_PAREN, "Expected '(' for function parameters");
                if (ERROR_OCCURRED) return 1;

                auto param_types = make_barray!(CType*)(allocator);
                bool is_varargs = false;

                if (!check(CTokenType.RIGHT_PAREN)) {
                    do {
                        if (check(CTokenType.ELLIPSIS)) {
                            advance();
                            is_varargs = true;
                            break;
                        }
                        if (check(CTokenType.VOID)) {
                            CToken void_tok = advance();
                            if (check(CTokenType.RIGHT_PAREN)) break;
                            CType* param_type = &TYPE_VOID;
                            while (match(CTokenType.STAR)) {
                                param_type = make_pointer_type(allocator, param_type);
                            }
                            if (check(CTokenType.IDENTIFIER)) advance();
                            param_types ~= param_type;
                        } else {
                            CType* param_type = parse_type();
                            if (param_type is null) return 1;
                            if (check(CTokenType.IDENTIFIER)) advance();
                            param_types ~= param_type;
                        }
                    } while (match(CTokenType.COMMA));
                }

                consume(CTokenType.RIGHT_PAREN, "Expected ')' after parameters");
                if (ERROR_OCCURRED) return 1;

                consume(CTokenType.SEMICOLON, "Expected ';' after typedef");
                if (ERROR_OCCURRED) return 1;

                CType* func_type = make_function_type(allocator, struct_type, param_types[], is_varargs);
                CType* func_ptr_type = make_pointer_type(allocator, func_type);

                typedef_types[typedef_name.lexeme] = func_ptr_type;
                return 0;
            }

            // Skip attribute macros (like SDL_AUDIOCVT_PACKED, __attribute__((packed))) before typedef name
            while (check(CTokenType.IDENTIFIER)) {
                auto cur_lexeme = peek().lexeme;
                auto next = peek_at(1).type;

                // Only skip __attribute__(...) or known attribute macros
                if (cur_lexeme == "__attribute__" && next == CTokenType.LEFT_PAREN) {
                    advance();
                    skip_balanced_parens();
                } else if (next == CTokenType.IDENTIFIER) {
                    // Check if next token is __attribute__ - if so, current is the typedef name
                    auto next_lexeme = peek_at(1).lexeme;
                    if (next_lexeme == "__attribute__") {
                        break;  // Current is typedef name, stop skipping
                    }
                    // Skip simple attribute identifier (like SDL_AUDIOCVT_PACKED)
                    advance();
                } else if (next == CTokenType.LEFT_PAREN) {
                    // Skip function-like macro attribute
                    advance();
                    skip_balanced_parens();
                } else {
                    break;  // Next token is the typedef name followed by ;
                }
            }

            // Now get the typedef name
            CToken typedef_name = consume(CTokenType.IDENTIFIER, "Expected typedef name");
            if (ERROR_OCCURRED) return 1;

            // Skip __attribute__((xxx)) after typedef name
            if (check(CTokenType.IDENTIFIER) && peek().lexeme == "__attribute__") {
                advance();
                skip_balanced_parens();
            }

            consume(CTokenType.SEMICOLON, "Expected ';' after typedef");
            if (ERROR_OCCURRED) return 1;

            typedef_types[typedef_name.lexeme] = struct_type;
            return 0;

        } else if (check(CTokenType.UNION)) {
            advance();  // consume 'union'

            CToken union_name;
            bool has_name = false;
            bool has_body = false;

            if (check(CTokenType.IDENTIFIER)) {
                union_name = advance();
                has_name = true;
            }
            if (check(CTokenType.LEFT_BRACE)) {
                has_body = true;
            }

            CType* union_type;

            if (has_body) {
                consume(CTokenType.LEFT_BRACE, "Expected '{'");
                if (ERROR_OCCURRED) return 1;

                auto fields = make_barray!StructField(allocator);
                size_t max_size = 0;

                while (!check(CTokenType.RIGHT_BRACE) && !at_end) {
                    CType* field_type = parse_type();
                    if (field_type is null) return 1;

                    // Check for anonymous bit field
                    if (check(CTokenType.COLON)) {
                        advance();  // consume :
                        auto width_result = parse_enum_const_expr();
                        if (width_result.err) return 1;
                        consume(CTokenType.SEMICOLON, "Expected ';' after anonymous bit field");
                        if (ERROR_OCCURRED) return 1;
                        continue;  // Skip anonymous bit fields
                    }

                    CToken field_name = consume(CTokenType.IDENTIFIER, "Expected field name");
                    if (ERROR_OCCURRED) return 1;

                    if (match(CTokenType.LEFT_BRACKET)) {
                        auto size_result = parse_enum_const_expr();
                        if (size_result.err) return 1;
                        if (size_result.value <= 0) {
                            error("Array size must be positive");
                            return 1;
                        }
                        consume(CTokenType.RIGHT_BRACKET, "Expected ']'");
                        if (ERROR_OCCURRED) return 1;
                        field_type = make_array_type(allocator, field_type, cast(size_t) size_result.value);
                    }

                    // Handle bit fields
                    if (match(CTokenType.COLON)) {
                        auto width_result = parse_enum_const_expr();
                        if (width_result.err) return 1;
                    }

                    consume(CTokenType.SEMICOLON, "Expected ';' after field");
                    if (ERROR_OCCURRED) return 1;

                    StructField field;
                    field.name = field_name.lexeme;
                    field.type = field_type;
                    field.offset = 0;  // All union fields at offset 0
                    fields ~= field;
                    size_t field_size = field_type.size_of();
                    if (field_size > max_size) max_size = field_size;
                }

                consume(CTokenType.RIGHT_BRACE, "Expected '}'");
                if (ERROR_OCCURRED) return 1;

                union_type = make_union_type(allocator, has_name ? union_name.lexeme : "", fields[], max_size);

                if (has_name) {
                    union_types[union_name.lexeme] = union_type;
                }
            } else {
                if (!has_name) {
                    error("Expected union name or body");
                    return 1;
                }
                if (CType** found = union_name.lexeme in union_types) {
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
            while (match(CTokenType.STAR)) {
                union_type = make_pointer_type(allocator, union_type);
                // Skip pointer qualifiers
                while (match(CTokenType.CONST) || match(CTokenType.VOLATILE)) {}
            }

            CToken typedef_name = consume(CTokenType.IDENTIFIER, "Expected typedef name");
            if (ERROR_OCCURRED) return 1;

            // Skip __attribute__((xxx)) after typedef name
            if (check(CTokenType.IDENTIFIER) && peek().lexeme == "__attribute__") {
                advance();
                skip_balanced_parens();
            }

            consume(CTokenType.SEMICOLON, "Expected ';' after typedef");
            if (ERROR_OCCURRED) return 1;

            typedef_types[typedef_name.lexeme] = union_type;
            return 0;

        } else if (check(CTokenType.ENUM)) {
            advance();  // consume 'enum'

            CToken enum_name;
            bool has_name = false;
            bool has_body = false;

            if (check(CTokenType.IDENTIFIER)) {
                enum_name = advance();
                has_name = true;
            }
            if (check(CTokenType.LEFT_BRACE)) {
                has_body = true;
            }

            CType* enum_type;

            if (has_body) {
                consume(CTokenType.LEFT_BRACE, "Expected '{'");
                if (ERROR_OCCURRED) return 1;

                // Parse enum constants
                auto constants = make_barray!EnumConstant(allocator);
                long next_value = 0;

                while (!check(CTokenType.RIGHT_BRACE) && !at_end) {
                    CToken const_name = consume(CTokenType.IDENTIFIER, "Expected enum constant name");
                    if (ERROR_OCCURRED) return 1;

                    long value = next_value;
                    if (match(CTokenType.EQUAL)) {
                        auto result = parse_enum_const_expr();
                        if (result.err) return 1;
                        value = result.value;
                    }

                    EnumConstant ec;
                    ec.name = const_name.lexeme;
                    ec.value = value;
                    constants ~= ec;
                    enum_constants[const_name.lexeme] = value;
                    next_value = value + 1;

                    if (!check(CTokenType.RIGHT_BRACE)) {
                        if (!match(CTokenType.COMMA)) {
                            if (!check(CTokenType.RIGHT_BRACE)) {
                                error("Expected ',' or '}' after enum constant");
                                return 1;
                            }
                        }
                    }
                }

                consume(CTokenType.RIGHT_BRACE, "Expected '}'");
                if (ERROR_OCCURRED) return 1;

                enum_type = make_enum_type(allocator, has_name ? enum_name.lexeme : "");

                if (has_name) {
                    enum_types[enum_name.lexeme] = enum_type;
                }

                // Add enum definition to translation unit so code generator sees constants
                CEnumDef edef;
                edef.name = has_name ? enum_name : CToken.init;
                edef.enum_type = enum_type;
                edef.constants = constants[];
                *enums_out ~= edef;
            } else {
                if (!has_name) {
                    error("Expected enum name or body");
                    return 1;
                }
                if (CType** found = enum_name.lexeme in enum_types) {
                    enum_type = *found;
                } else {
                    error("Unknown enum type");
                    return 1;
                }
            }

            CToken typedef_name = consume(CTokenType.IDENTIFIER, "Expected typedef name");
            if (ERROR_OCCURRED) return 1;

            consume(CTokenType.SEMICOLON, "Expected ';' after typedef");
            if (ERROR_OCCURRED) return 1;

            typedef_types[typedef_name.lexeme] = enum_type;
            return 0;

        } else {
            // Could be simple typedef or function pointer typedef
            // Simple: typedef <type> <name>;
            // Function pointer: typedef <ret_type> (* <name>)(<params>);
            // Function pointer with calling conv: typedef <ret_type> (SDLCALL * <name>)(<params>);

            CType* base_type = parse_type();
            if (base_type is null) return 1;

            // Check for function pointer syntax: starts with '('
            if (check(CTokenType.LEFT_PAREN)) {
                advance();  // consume '('

                // Skip any calling convention identifiers until we hit '*'
                while (check(CTokenType.IDENTIFIER)) {
                    advance();  // skip calling convention like SDLCALL
                }

                if (!match(CTokenType.STAR)) {
                    error("Expected '*' in function pointer typedef");
                    return 1;
                }

                CToken typedef_name = consume(CTokenType.IDENTIFIER, "Expected typedef name");
                if (ERROR_OCCURRED) return 1;

                consume(CTokenType.RIGHT_PAREN, "Expected ')' after function pointer name");
                if (ERROR_OCCURRED) return 1;

                // Now parse the parameter list
                consume(CTokenType.LEFT_PAREN, "Expected '(' for function parameters");
                if (ERROR_OCCURRED) return 1;

                auto param_types = make_barray!(CType*)(allocator);
                bool is_varargs = false;

                if (!check(CTokenType.RIGHT_PAREN)) {
                    do {
                        // Check for ...
                        if (check(CTokenType.ELLIPSIS)) {
                            advance();
                            is_varargs = true;
                            break;
                        }

                        // Check for void with no name (means no parameters)
                        if (check(CTokenType.VOID)) {
                            CToken void_tok = advance();
                            if (check(CTokenType.RIGHT_PAREN)) {
                                // Just "void" - no parameters
                                break;
                            }
                            // void* or void *name - put it back conceptually and parse as type
                            // Actually we already consumed void, so we need to handle this
                            CType* param_type = &TYPE_VOID;
                            // Check for pointer
                            while (match(CTokenType.STAR)) {
                                param_type = make_pointer_type(allocator, param_type);
                            }
                            // Skip optional parameter name
                            if (check(CTokenType.IDENTIFIER)) {
                                advance();
                            }
                            param_types ~= param_type;
                        } else {
                            CType* param_type = parse_type();
                            if (param_type is null) return 1;

                            // Skip optional parameter name
                            if (check(CTokenType.IDENTIFIER)) {
                                advance();
                            }

                            // Handle array parameters: char *argv[]
                            if (check(CTokenType.LEFT_BRACKET)) {
                                while (check(CTokenType.LEFT_BRACKET)) {
                                    advance();  // consume '['
                                    while (!check(CTokenType.RIGHT_BRACKET) && !at_end) {
                                        advance();
                                    }
                                    if (check(CTokenType.RIGHT_BRACKET)) advance();
                                }
                                // Convert to pointer type
                                param_type = make_pointer_type(allocator, param_type);
                            }

                            param_types ~= param_type;
                        }
                    } while (match(CTokenType.COMMA));
                }

                consume(CTokenType.RIGHT_PAREN, "Expected ')' after parameters");
                if (ERROR_OCCURRED) return 1;

                consume(CTokenType.SEMICOLON, "Expected ';' after typedef");
                if (ERROR_OCCURRED) return 1;

                // Create function type then wrap in pointer
                CType* func_type = make_function_type(allocator, base_type, param_types[], is_varargs);
                CType* func_ptr_type = make_pointer_type(allocator, func_type);

                typedef_types[typedef_name.lexeme] = func_ptr_type;
                return 0;
            }

            CToken typedef_name = consume(CTokenType.IDENTIFIER, "Expected typedef name");
            if (ERROR_OCCURRED) return 1;

            // Handle array typedefs: typedef int array_t[10];
            if (match(CTokenType.LEFT_BRACKET)) {
                auto size_result = parse_enum_const_expr();
                if (size_result.err) return 1;
                if (size_result.value <= 0) {
                    error("Array size must be positive");
                    return 1;
                }
                consume(CTokenType.RIGHT_BRACKET, "Expected ']'");
                if (ERROR_OCCURRED) return 1;
                base_type = make_array_type(allocator, base_type, cast(size_t) size_result.value);
            }

            // Skip __attribute__((xxx)) after typedef name
            if (check(CTokenType.IDENTIFIER) && peek().lexeme == "__attribute__") {
                advance();
                skip_balanced_parens();
            }

            consume(CTokenType.SEMICOLON, "Expected ';' after typedef");
            if (ERROR_OCCURRED) return 1;

            typedef_types[typedef_name.lexeme] = base_type;
            return 0;
        }
    }

    int parse_extern_decl(CExternDecl* decl) {
        advance();  // consume 'extern'

        // Skip __attribute__((...)) if present (e.g., from DECLSPEC)
        while (check(CTokenType.IDENTIFIER) && peek().lexeme == "__attribute__") {
            advance();  // skip __attribute__
            if (check(CTokenType.LEFT_PAREN)) {
                skip_balanced_parens();
            }
        }

        // Parse return type
        CType* ret_type = parse_type();
        if (ret_type is null) return 1;

        // Check for function pointer variable: extern type (*name)(...);
        if (check(CTokenType.LEFT_PAREN)) {
            advance();  // consume '('
            if (!match(CTokenType.STAR)) {
                error("Expected '*' in function pointer declaration");
                return 1;
            }
            CToken name = consume(CTokenType.IDENTIFIER, "Expected function pointer name");
            if (ERROR_OCCURRED) return 1;
            consume(CTokenType.RIGHT_PAREN, "Expected ')' after function pointer name");
            if (ERROR_OCCURRED) return 1;
            // Skip the parameter list and semicolon
            if (check(CTokenType.LEFT_PAREN)) {
                skip_balanced_parens();
            }
            consume(CTokenType.SEMICOLON, "Expected ';' after extern declaration");
            if (ERROR_OCCURRED) return 1;
            decl.name.lexeme = "";  // Mark as skipped (function pointer variable)
            return 0;
        }

        // Parse function/variable name
        CToken name = consume(CTokenType.IDENTIFIER, "Expected identifier");
        if (ERROR_OCCURRED) return 1;

        // Check if this is a function or variable
        if (check(CTokenType.SEMICOLON)) {
            // extern variable - skip it
            advance();  // consume ';'
            decl.name.lexeme = "";  // Mark as empty/skipped
            return 0;
        }

        // Handle comma-separated extern variable declarations: extern type a, b;
        if (check(CTokenType.COMMA)) {
            while (match(CTokenType.COMMA)) {
                if (!check(CTokenType.IDENTIFIER)) break;
                advance();  // skip identifier
            }
            consume(CTokenType.SEMICOLON, "Expected ';' after extern declaration");
            if (ERROR_OCCURRED) return 1;
            decl.name.lexeme = "";  // Mark as skipped
            return 0;
        }

        // Handle extern array declarations: extern int foo[];
        if (check(CTokenType.LEFT_BRACKET)) {
            // Skip array brackets
            while (!check(CTokenType.SEMICOLON) && !at_end) {
                advance();
            }
            consume(CTokenType.SEMICOLON, "Expected ';' after extern declaration");
            if (ERROR_OCCURRED) return 1;
            decl.name.lexeme = "";  // Mark as skipped
            return 0;
        }

        // Parse parameters
        consume(CTokenType.LEFT_PAREN, "Expected '(' after function name");
        if (ERROR_OCCURRED) return 1;

        auto params = make_barray!CParam(allocator);
        bool is_varargs = false;

        if (!check(CTokenType.RIGHT_PAREN)) {
            do {
                // Check for ...
                if (check(CTokenType.ELLIPSIS)) {
                    advance();
                    is_varargs = true;
                    break;
                }

                // Skip SAL-like annotations: SDL_OUT_BYTECAP(len), etc.
                // These are unknown identifiers followed by (...)
                while (check(CTokenType.IDENTIFIER) && peek_at(1).type == CTokenType.LEFT_PAREN &&
                       !is_type_specifier(peek())) {
                    advance();  // consume annotation name
                    skip_balanced_parens();  // skip arguments
                }

                CParam param;
                param.type = parse_type();
                if (param.type is null) return 1;

                // Check for function pointer parameter: type (*name)(params)
                if (check(CTokenType.LEFT_PAREN)) {
                    advance();  // consume '('
                    if (match(CTokenType.STAR)) {
                        // Function pointer parameter
                        if (check(CTokenType.IDENTIFIER)) {
                            param.name = advance();
                        }
                        consume(CTokenType.RIGHT_PAREN, "Expected ')' after function pointer name");
                        if (ERROR_OCCURRED) return 1;
                        // Skip the parameter list
                        if (check(CTokenType.LEFT_PAREN)) {
                            skip_balanced_parens();
                        }
                        // Treat as void* for simplicity
                        param.type = make_pointer_type(allocator, &TYPE_VOID);
                        params ~= param;
                        continue;
                    } else {
                        // Not a function pointer - error
                        error("Unexpected '(' in parameter");
                        return 1;
                    }
                }

                // Skip const/volatile qualifiers between type and name (e.g., char *const name)
                while (match(CTokenType.CONST) || match(CTokenType.VOLATILE)) {}

                // Parameter name is optional in declarations
                if (check(CTokenType.IDENTIFIER)) {
                    param.name = advance();
                }

                // Handle array parameters: type name[size]
                if (check(CTokenType.LEFT_BRACKET)) {
                    // Skip the array brackets - treat as pointer
                    while (check(CTokenType.LEFT_BRACKET)) {
                        advance();  // consume '['
                        // Skip everything until ']'
                        while (!check(CTokenType.RIGHT_BRACKET) && !at_end) {
                            advance();
                        }
                        if (check(CTokenType.RIGHT_BRACKET)) advance();
                    }
                    // Convert to pointer type
                    param.type = make_pointer_type(allocator, param.type);
                }

                params ~= param;
            } while (match(CTokenType.COMMA));
        }

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after parameters");
        if (ERROR_OCCURRED) return 1;

        // Skip attributes/modifiers until we reach the semicolon
        // This handles __attribute__((...)), __nonnull(...), etc.
        while (!check(CTokenType.SEMICOLON) && !at_end) {
            if (check(CTokenType.LEFT_PAREN)) {
                skip_balanced_parens();
            } else {
                advance();
            }
        }

        consume(CTokenType.SEMICOLON, "Expected ';' after extern declaration");
        if (ERROR_OCCURRED) return 1;

        decl.name = name;
        decl.return_type = ret_type;
        decl.params = params[];
        decl.is_varargs = is_varargs;
        decl.library = current_library;
        return 0;
    }

    int parse_function(CFunction* func) {
        // Parse return type
        CType* ret_type = parse_type();
        if (ret_type is null) return 1;

        // Parse function name
        CToken name = consume(CTokenType.IDENTIFIER, "Expected function name");
        if (ERROR_OCCURRED) return 1;

        return parse_function_rest(ret_type, name, func);
    }

    // Parse function after type and name have been consumed
    int parse_function_rest(CType* ret_type, CToken name, CFunction* func) {
        // Parse parameters
        consume(CTokenType.LEFT_PAREN, "Expected '(' after function name");
        if (ERROR_OCCURRED) return 1;

        auto params = make_barray!CParam(allocator);
        bool is_varargs = false;

        if (!check(CTokenType.RIGHT_PAREN)) {
            do {
                if (check(CTokenType.ELLIPSIS)) {
                    advance();
                    is_varargs = true;
                    break;
                }

                // Check for (void) - means no parameters
                if (check(CTokenType.VOID)) {
                    CToken void_tok = advance();
                    if (check(CTokenType.RIGHT_PAREN)) {
                        // Just "void" - no parameters
                        break;
                    }
                    // void* or void type - put back conceptually
                    CParam param;
                    param.type = &TYPE_VOID;
                    // Check for pointer
                    while (match(CTokenType.STAR)) {
                        param.type = make_pointer_type(allocator, param.type);
                    }

                    // Check for function pointer parameter: void (*name)(params)
                    if (check(CTokenType.LEFT_PAREN)) {
                        advance();  // consume '('
                        if (match(CTokenType.STAR)) {
                            if (check(CTokenType.IDENTIFIER)) {
                                param.name = advance();
                            }
                            consume(CTokenType.RIGHT_PAREN, "Expected ')' after function pointer");
                            if (ERROR_OCCURRED) return 1;
                            if (check(CTokenType.LEFT_PAREN)) {
                                skip_balanced_parens();
                            }
                            param.type = make_pointer_type(allocator, &TYPE_VOID);
                            params ~= param;
                            continue;
                        } else {
                            error("Unexpected '(' in void parameter");
                            return 1;
                        }
                    }

                    // Parameter name is optional in declarations
                    if (check(CTokenType.IDENTIFIER)) {
                        param.name = advance();
                    }
                    params ~= param;
                } else {
                    CParam param;
                    param.type = parse_type();
                    if (param.type is null) return 1;

                    // Check for function pointer parameter: type (*name)(params) or type (*)(params)
                    if (check(CTokenType.LEFT_PAREN)) {
                        advance();  // consume '('
                        if (match(CTokenType.STAR)) {
                            // Function pointer parameter - name is optional
                            if (check(CTokenType.IDENTIFIER)) {
                                param.name = advance();
                            }
                            consume(CTokenType.RIGHT_PAREN, "Expected ')' after function pointer");
                            if (ERROR_OCCURRED) return 1;
                            // Skip the parameter list
                            if (check(CTokenType.LEFT_PAREN)) {
                                skip_balanced_parens();
                            }
                            // Treat as void* for simplicity
                            param.type = make_pointer_type(allocator, &TYPE_VOID);
                            params ~= param;
                            continue;
                        } else {
                            error("Unexpected '(' in parameter");
                            return 1;
                        }
                    }

                    // Skip const/volatile qualifiers between type and name (e.g., char *const name)
                    while (match(CTokenType.CONST) || match(CTokenType.VOLATILE)) {}

                    // Parameter name is optional in declarations
                    if (check(CTokenType.IDENTIFIER)) {
                        param.name = advance();
                    }

                    // Handle array parameters: type name[size]
                    if (check(CTokenType.LEFT_BRACKET)) {
                        while (check(CTokenType.LEFT_BRACKET)) {
                            advance();  // consume '['
                            while (!check(CTokenType.RIGHT_BRACKET) && !at_end) {
                                advance();
                            }
                            if (check(CTokenType.RIGHT_BRACKET)) advance();
                        }
                        param.type = make_pointer_type(allocator, param.type);
                    }

                    params ~= param;
                }
            } while (match(CTokenType.COMMA));
        }

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after parameters");
        if (ERROR_OCCURRED) return 1;

        func.name = name;
        func.return_type = ret_type;
        func.params = params[];
        func.is_varargs = is_varargs;

        // Check if declaration or definition
        if (match(CTokenType.SEMICOLON)) {
            func.is_definition = false;
            return 0;
        }

        // Parse function body
        func.is_definition = true;
        consume(CTokenType.LEFT_BRACE, "Expected '{' for function body");
        if (ERROR_OCCURRED) return 1;

        auto body = make_barray!(CStmt*)(allocator);
        while (!check(CTokenType.RIGHT_BRACE) && !at_end) {
            CStmt* stmt = parse_statement();
            if (stmt is null) return 1;
            body ~= stmt;
        }

        consume(CTokenType.RIGHT_BRACE, "Expected '}' after function body");
        if (ERROR_OCCURRED) return 1;

        func.body = body[];
        return 0;
    }

    // Parse global variable after type and name have been consumed
    int parse_global_var_rest(CType* var_type, CToken name, CGlobalVar* gvar) {
        gvar.name = name;
        gvar.var_type = var_type;
        gvar.initializer = null;

        // Check for initializer
        if (match(CTokenType.EQUAL)) {
            gvar.initializer = parse_expression();
            if (gvar.initializer is null) return 1;
        }

        consume(CTokenType.SEMICOLON, "Expected ';' after global variable declaration");
        if (ERROR_OCCURRED) return 1;

        return 0;
    }

    // =========================================================================
    // Type Parsing
    // =========================================================================

    CType* parse_type() {
        CType* base = parse_base_type();
        if (base is null) return null;

        // Handle pointer types
        while (check(CTokenType.STAR)) {
            advance();
            base = make_pointer_type(allocator, base);
            // Skip pointer qualifiers (const, volatile)
            while (match(CTokenType.CONST) || match(CTokenType.VOLATILE)) {
                // Just skip - we don't track pointer constness
            }
        }

        return base;
    }

    CType* parse_base_type() {
        bool is_unsigned = false;
        bool is_const = false;

        // Handle qualifiers and modifiers
        while (true) {
            if (match(CTokenType.CONST)) {
                is_const = true;
            } else if (match(CTokenType.VOLATILE)) {
                // Skip volatile - we don't track it
            } else if (match(CTokenType.UNSIGNED)) {
                is_unsigned = true;
            } else if (match(CTokenType.SIGNED)) {
                is_unsigned = false;
            } else {
                break;
            }
        }

        CType* result;

        if (match(CTokenType.VOID)) {
            result = &TYPE_VOID;
        } else if (match(CTokenType.CHAR)) {
            result = is_unsigned ? &TYPE_UCHAR : &TYPE_CHAR;
        } else if (match(CTokenType.SHORT)) {
            // Handle trailing modifiers (e.g., 'short unsigned int')
            while (match(CTokenType.UNSIGNED)) is_unsigned = true;
            while (match(CTokenType.SIGNED)) is_unsigned = false;
            match(CTokenType.INT);  // optional trailing int
            // Allocate new type for short
            auto data = allocator.alloc(CType.sizeof);
            result = cast(CType*)data.ptr;
            result.kind = CTypeKind.SHORT;
            result.is_unsigned = is_unsigned;
        } else if (match(CTokenType.INT)) {
            // Handle trailing modifiers (e.g., 'int unsigned')
            while (match(CTokenType.UNSIGNED)) is_unsigned = true;
            while (match(CTokenType.SIGNED)) is_unsigned = false;
            result = is_unsigned ? &TYPE_UINT : &TYPE_INT;
        } else if (match(CTokenType.LONG)) {
            // Check for 'long long' or 'long double'
            // Also handle trailing unsigned/signed and int (e.g., 'long unsigned int')
            if (match(CTokenType.LONG)) {
                // long long - same as long on 64-bit
                // Consume trailing unsigned/signed/int
                while (match(CTokenType.UNSIGNED)) is_unsigned = true;
                while (match(CTokenType.SIGNED)) is_unsigned = false;
                match(CTokenType.INT);  // optional trailing int
                result = is_unsigned ? &TYPE_ULONG : &TYPE_LONG;
            } else if (match(CTokenType.DOUBLE)) {
                // long double - treat as double for simplicity
                result = &TYPE_DOUBLE;
            } else {
                // Consume trailing unsigned/signed/int (e.g., 'long unsigned int')
                while (match(CTokenType.UNSIGNED)) is_unsigned = true;
                while (match(CTokenType.SIGNED)) is_unsigned = false;
                match(CTokenType.INT);  // optional trailing int
                result = is_unsigned ? &TYPE_ULONG : &TYPE_LONG;
            }
        } else if (match(CTokenType.FLOAT)) {
            result = &TYPE_FLOAT;
        } else if (match(CTokenType.DOUBLE)) {
            result = &TYPE_DOUBLE;
        } else if (match(CTokenType.STRUCT)) {
            // struct Name or struct { ... }
            bool has_name = check(CTokenType.IDENTIFIER);
            CToken struct_name;
            if (has_name) {
                struct_name = advance();
            }

            // Check for inline definition
            if (check(CTokenType.LEFT_BRACE)) {
                advance();  // consume '{'
                auto fields = make_barray!StructField(allocator);
                size_t total_size = 0;

                while (!check(CTokenType.RIGHT_BRACE) && !at_end) {
                    CType* field_type = parse_type();
                    if (field_type is null) return null;

                    // Check for anonymous bit field
                    if (check(CTokenType.COLON)) {
                        advance();  // consume :
                        auto width_result = parse_enum_const_expr();
                        if (width_result.err) return null;
                        consume(CTokenType.SEMICOLON, "Expected ';' after anonymous bit field");
                        if (ERROR_OCCURRED) return null;
                        continue;  // Skip anonymous bit fields
                    }

                    CToken field_name = consume(CTokenType.IDENTIFIER, "Expected field name");
                    if (ERROR_OCCURRED) return null;

                    if (match(CTokenType.LEFT_BRACKET)) {
                        auto size_result = parse_enum_const_expr();
                        if (size_result.err) return null;
                        if (size_result.value <= 0) {
                            error("Array size must be positive");
                            return null;
                        }
                        consume(CTokenType.RIGHT_BRACKET, "Expected ']'");
                        if (ERROR_OCCURRED) return null;
                        field_type = make_array_type(allocator, field_type, cast(size_t) size_result.value);
                    }

                    // Handle bit fields
                    if (match(CTokenType.COLON)) {
                        auto width_result = parse_enum_const_expr();
                        if (width_result.err) return null;
                    }

                    consume(CTokenType.SEMICOLON, "Expected ';' after field");
                    if (ERROR_OCCURRED) return null;

                    StructField field;
                    field.name = field_name.lexeme;
                    field.type = field_type;
                    field.offset = total_size;
                    fields ~= field;
                    total_size += field_type.size_of();
                }

                consume(CTokenType.RIGHT_BRACE, "Expected '}'");
                if (ERROR_OCCURRED) return null;

                result = make_struct_type(allocator, has_name ? struct_name.lexeme : "", fields[], total_size);

                if (has_name) {
                    struct_types[struct_name.lexeme] = result;
                }
            } else {
                // Just a reference - need name
                if (!has_name) {
                    error("Expected struct name or body");
                    return null;
                }
                // Look up struct in defined structs
                if (CType** found = struct_name.lexeme in struct_types) {
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
        } else if (match(CTokenType.UNION)) {
            // union Name or union { ... }
            bool has_name = check(CTokenType.IDENTIFIER);
            CToken union_name;
            if (has_name) {
                union_name = advance();
            }

            // Check for inline definition
            if (check(CTokenType.LEFT_BRACE)) {
                advance();  // consume '{'
                auto fields = make_barray!StructField(allocator);
                size_t max_size = 0;

                while (!check(CTokenType.RIGHT_BRACE) && !at_end) {
                    CType* field_type = parse_type();
                    if (field_type is null) return null;

                    // Check for anonymous bit field
                    if (check(CTokenType.COLON)) {
                        advance();  // consume :
                        auto width_result = parse_enum_const_expr();
                        if (width_result.err) return null;
                        consume(CTokenType.SEMICOLON, "Expected ';' after anonymous bit field");
                        if (ERROR_OCCURRED) return null;
                        continue;  // Skip anonymous bit fields
                    }

                    CToken field_name = consume(CTokenType.IDENTIFIER, "Expected field name");
                    if (ERROR_OCCURRED) return null;

                    if (match(CTokenType.LEFT_BRACKET)) {
                        auto size_result = parse_enum_const_expr();
                        if (size_result.err) return null;
                        if (size_result.value <= 0) {
                            error("Array size must be positive");
                            return null;
                        }
                        consume(CTokenType.RIGHT_BRACKET, "Expected ']'");
                        if (ERROR_OCCURRED) return null;
                        field_type = make_array_type(allocator, field_type, cast(size_t) size_result.value);
                    }

                    // Handle bit fields
                    if (match(CTokenType.COLON)) {
                        auto width_result = parse_enum_const_expr();
                        if (width_result.err) return null;
                    }

                    consume(CTokenType.SEMICOLON, "Expected ';' after field");
                    if (ERROR_OCCURRED) return null;

                    StructField field;
                    field.name = field_name.lexeme;
                    field.type = field_type;
                    field.offset = 0;  // All union fields at offset 0
                    fields ~= field;
                    size_t field_size = field_type.size_of();
                    if (field_size > max_size) max_size = field_size;
                }

                consume(CTokenType.RIGHT_BRACE, "Expected '}'");
                if (ERROR_OCCURRED) return null;

                result = make_union_type(allocator, has_name ? union_name.lexeme : "", fields[], max_size);

                if (has_name) {
                    union_types[union_name.lexeme] = result;
                }
            } else {
                // Just a reference - need name
                if (!has_name) {
                    error("Expected union name or body");
                    return null;
                }
                // Look up union in defined unions
                if (CType** found = union_name.lexeme in union_types) {
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
        } else if (match(CTokenType.ENUM)) {
            // enum Name
            CToken name = consume(CTokenType.IDENTIFIER, "Expected enum name");
            // Look up enum in defined enums
            if (CType** found = name.lexeme in enum_types) {
                result = *found;
            } else {
                error("Unknown enum type");
                return null;
            }
        } else if (check(CTokenType.IDENTIFIER)) {
            // Check for typedef name
            CToken name = peek();
            if (CType** found = name.lexeme in typedef_types) {
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
        if (result.kind == CTypeKind.LONG || result.kind == CTypeKind.SHORT) {
            match(CTokenType.INT);  // Optional 'int' after long/short
        }

        if (is_const && result !is &TYPE_VOID) {
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

    CStmt* parse_statement() {
        if (match(CTokenType.SEMICOLON)) {
            return CEmptyStmt.get();
        }
        if (match(CTokenType.RETURN)) return parse_return();
        if (match(CTokenType.IF)) return parse_if();
        if (match(CTokenType.WHILE)) return parse_while();
        if (match(CTokenType.FOR)) return parse_for();
        if (match(CTokenType.LEFT_BRACE)) return parse_block();
        if (match(CTokenType.BREAK)) return parse_break();
        if (match(CTokenType.CONTINUE)) return parse_continue();

        // Check for variable declaration (starts with type)
        if (is_type_specifier(peek())) {
            return parse_var_decl();
        }

        // Expression statement
        return parse_expr_stmt();
    }

    bool is_type_specifier(CToken tok) {
        with (CTokenType) {
            if (tok.type == VOID || tok.type == CHAR || tok.type == SHORT ||
                tok.type == INT || tok.type == LONG || tok.type == FLOAT ||
                tok.type == DOUBLE || tok.type == UNSIGNED ||
                tok.type == SIGNED || tok.type == CONST || tok.type == STRUCT ||
                tok.type == UNION || tok.type == ENUM) {
                return true;
            }
            // Check for typedef names
            if (tok.type == IDENTIFIER) {
                return (tok.lexeme in typedef_types) !is null;
            }
            return false;
        }
    }

    CStmt* parse_return() {
        CToken keyword = previous();
        CExpr* value = null;

        if (!check(CTokenType.SEMICOLON)) {
            value = parse_expression();
            if (value is null) return null;
        }

        consume(CTokenType.SEMICOLON, "Expected ';' after return");
        if (ERROR_OCCURRED) return null;

        return CReturnStmt.make(allocator, value, keyword);
    }

    CStmt* parse_if() {
        CToken keyword = previous();

        consume(CTokenType.LEFT_PAREN, "Expected '(' after 'if'");
        if (ERROR_OCCURRED) return null;

        CExpr* condition = parse_expression();
        if (condition is null) return null;

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after if condition");
        if (ERROR_OCCURRED) return null;

        CStmt* then_branch = parse_statement();
        if (then_branch is null) return null;

        CStmt* else_branch = null;
        if (match(CTokenType.ELSE)) {
            else_branch = parse_statement();
            if (else_branch is null) return null;
        }

        return CIfStmt.make(allocator, condition, then_branch, else_branch, keyword);
    }

    CStmt* parse_while() {
        CToken keyword = previous();

        consume(CTokenType.LEFT_PAREN, "Expected '(' after 'while'");
        if (ERROR_OCCURRED) return null;

        CExpr* condition = parse_expression();
        if (condition is null) return null;

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after while condition");
        if (ERROR_OCCURRED) return null;

        CStmt* body = parse_statement();
        if (body is null) return null;

        return CWhileStmt.make(allocator, condition, body, keyword);
    }

    CStmt* parse_for() {
        CToken keyword = previous();

        consume(CTokenType.LEFT_PAREN, "Expected '(' after 'for'");
        if (ERROR_OCCURRED) return null;

        // Initializer
        CStmt* init = null;
        if (!check(CTokenType.SEMICOLON)) {
            if (is_type_specifier(peek())) {
                init = parse_var_decl();
            } else {
                init = parse_expr_stmt();
            }
            if (init is null) return null;
        } else {
            advance();  // consume ';'
        }

        // Condition
        CExpr* condition = null;
        if (!check(CTokenType.SEMICOLON)) {
            condition = parse_expression();
            if (condition is null) return null;
        }
        consume(CTokenType.SEMICOLON, "Expected ';' after for condition");
        if (ERROR_OCCURRED) return null;

        // Increment
        CExpr* increment = null;
        if (!check(CTokenType.RIGHT_PAREN)) {
            increment = parse_expression();
            if (increment is null) return null;
        }

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after for clauses");
        if (ERROR_OCCURRED) return null;

        CStmt* body = parse_statement();
        if (body is null) return null;

        return CForStmt.make(allocator, init, condition, increment, body, keyword);
    }

    CStmt* parse_block() {
        CToken brace = previous();
        auto statements = make_barray!(CStmt*)(allocator);

        while (!check(CTokenType.RIGHT_BRACE) && !at_end) {
            CStmt* stmt = parse_statement();
            if (stmt is null) return null;
            statements ~= stmt;
        }

        consume(CTokenType.RIGHT_BRACE, "Expected '}' after block");
        if (ERROR_OCCURRED) return null;

        return CBlock.make(allocator, statements[], brace);
    }

    CStmt* parse_break() {
        CToken keyword = previous();
        consume(CTokenType.SEMICOLON, "Expected ';' after 'break'");
        if (ERROR_OCCURRED) return null;
        return CBreakStmt.make(allocator, keyword);
    }

    CStmt* parse_continue() {
        CToken keyword = previous();
        consume(CTokenType.SEMICOLON, "Expected ';' after 'continue'");
        if (ERROR_OCCURRED) return null;
        return CContinueStmt.make(allocator, keyword);
    }

    CStmt* parse_var_decl() {
        CToken type_tok = peek();
        CType* var_type = parse_type();
        if (var_type is null) return null;

        CToken name = consume(CTokenType.IDENTIFIER, "Expected variable name");
        if (ERROR_OCCURRED) return null;

        // Check for array declaration: int arr[10] or int arr[SIZE]
        if (match(CTokenType.LEFT_BRACKET)) {
            auto size_result = parse_enum_const_expr();
            if (size_result.err) return null;
            if (size_result.value <= 0) {
                error("Array size must be positive");
                return null;
            }

            consume(CTokenType.RIGHT_BRACKET, "Expected ']' after array size");
            if (ERROR_OCCURRED) return null;

            // Create array type
            var_type = make_array_type(allocator, var_type, cast(size_t) size_result.value);
        }

        CExpr* initializer = null;
        if (match(CTokenType.EQUAL)) {
            initializer = parse_expression();
            if (initializer is null) return null;
        }

        consume(CTokenType.SEMICOLON, "Expected ';' after variable declaration");
        if (ERROR_OCCURRED) return null;

        return CVarDecl.make(allocator, var_type, name, initializer, type_tok);
    }

    CStmt* parse_expr_stmt() {
        CToken tok = peek();
        CExpr* expr = parse_expression();
        if (expr is null) return null;

        consume(CTokenType.SEMICOLON, "Expected ';' after expression");
        if (ERROR_OCCURRED) return null;

        return CExprStmt.make(allocator, expr, tok);
    }

    // =========================================================================
    // Expression Parsing (Pratt/precedence climbing)
    // =========================================================================

    CExpr* parse_expression() {
        return parse_assignment();
    }

    CExpr* parse_assignment() {
        CExpr* expr = parse_logical_or();
        if (expr is null) return null;

        if (check(CTokenType.EQUAL) || check(CTokenType.PLUS_EQUAL) ||
            check(CTokenType.MINUS_EQUAL) || check(CTokenType.STAR_EQUAL) ||
            check(CTokenType.SLASH_EQUAL)) {
            CToken op_tok = advance();
            CExpr* value = parse_assignment();  // Right associative
            if (value is null) return null;
            return CAssign.make(allocator, expr, op_tok.type, value, op_tok);
        }

        return expr;
    }

    CExpr* parse_logical_or() {
        CExpr* expr = parse_logical_and();
        if (expr is null) return null;

        while (check(CTokenType.PIPE_PIPE)) {
            CToken op = advance();
            CExpr* right = parse_logical_and();
            if (right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    CExpr* parse_logical_and() {
        CExpr* expr = parse_bitwise_or();
        if (expr is null) return null;

        while (check(CTokenType.AMP_AMP)) {
            CToken op = advance();
            CExpr* right = parse_bitwise_or();
            if (right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    CExpr* parse_bitwise_or() {
        CExpr* expr = parse_bitwise_xor();
        if (expr is null) return null;

        while (check(CTokenType.PIPE) && !check_next(CTokenType.PIPE)) {
            CToken op = advance();
            CExpr* right = parse_bitwise_xor();
            if (right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    CExpr* parse_bitwise_xor() {
        CExpr* expr = parse_bitwise_and();
        if (expr is null) return null;

        while (check(CTokenType.CARET)) {
            CToken op = advance();
            CExpr* right = parse_bitwise_and();
            if (right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    CExpr* parse_bitwise_and() {
        CExpr* expr = parse_equality();
        if (expr is null) return null;

        while (check(CTokenType.AMP) && !check_next(CTokenType.AMP)) {
            CToken op = advance();
            CExpr* right = parse_equality();
            if (right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    CExpr* parse_equality() {
        CExpr* expr = parse_comparison();
        if (expr is null) return null;

        while (check(CTokenType.EQUAL_EQUAL) || check(CTokenType.BANG_EQUAL)) {
            CToken op = advance();
            CExpr* right = parse_comparison();
            if (right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    CExpr* parse_comparison() {
        CExpr* expr = parse_shift();
        if (expr is null) return null;

        while (check(CTokenType.LESS) || check(CTokenType.LESS_EQUAL) ||
               check(CTokenType.GREATER) || check(CTokenType.GREATER_EQUAL)) {
            CToken op = advance();
            CExpr* right = parse_shift();
            if (right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    CExpr* parse_shift() {
        CExpr* expr = parse_additive();
        if (expr is null) return null;

        while (check(CTokenType.LESS_LESS) || check(CTokenType.GREATER_GREATER)) {
            CToken op = advance();
            CExpr* right = parse_additive();
            if (right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    CExpr* parse_additive() {
        CExpr* expr = parse_multiplicative();
        if (expr is null) return null;

        while (check(CTokenType.PLUS) || check(CTokenType.MINUS)) {
            CToken op = advance();
            CExpr* right = parse_multiplicative();
            if (right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    CExpr* parse_multiplicative() {
        CExpr* expr = parse_unary();
        if (expr is null) return null;

        while (check(CTokenType.STAR) || check(CTokenType.SLASH) || check(CTokenType.PERCENT)) {
            CToken op = advance();
            CExpr* right = parse_unary();
            if (right is null) return null;
            expr = CBinary.make(allocator, expr, op.type, right, op);
        }

        return expr;
    }

    CExpr* parse_unary() {
        // Prefix operators
        if (check(CTokenType.BANG) || check(CTokenType.MINUS) ||
            check(CTokenType.TILDE) || check(CTokenType.STAR) ||
            check(CTokenType.AMP) || check(CTokenType.PLUS_PLUS) ||
            check(CTokenType.MINUS_MINUS)) {
            CToken op = advance();
            CExpr* operand = parse_unary();
            if (operand is null) return null;
            return CUnary.make(allocator, op.type, operand, true, op);
        }

        // sizeof operator: sizeof(type) or sizeof expr
        if (match(CTokenType.SIZEOF)) {
            CToken op = previous();

            // Check for sizeof(type) - requires parens
            if (check(CTokenType.LEFT_PAREN)) {
                // Peek ahead to see if it's a type
                if (peek_at(1).type == CTokenType.VOID ||
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
                     (peek_at(1).lexeme in typedef_types) !is null)) {
                    // sizeof(type)
                    advance();  // consume '('
                    CType* type = parse_type();
                    if (type is null) return null;
                    consume(CTokenType.RIGHT_PAREN, "Expected ')' after type");
                    if (ERROR_OCCURRED) return null;
                    size_t size = type.size_of();
                    return CSizeof.make(allocator, type, size, op);
                }
            }

            // sizeof expr (unary operator on expression)
            CExpr* expr = parse_unary();
            if (expr is null) return null;
            return CSizeof.make_expr(allocator, expr, op);
        }

        return parse_postfix();
    }

    CExpr* parse_postfix() {
        CExpr* expr = parse_primary();
        if (expr is null) return null;

        while (true) {
            if (match(CTokenType.LEFT_PAREN)) {
                // Function call
                expr = finish_call(expr);
                if (expr is null) return null;
            } else if (match(CTokenType.LEFT_BRACKET)) {
                // Array subscript
                CToken bracket = previous();
                CExpr* index = parse_expression();
                if (index is null) return null;
                consume(CTokenType.RIGHT_BRACKET, "Expected ']' after subscript");
                if (ERROR_OCCURRED) return null;
                expr = CSubscript.make(allocator, expr, index, bracket);
            } else if (match(CTokenType.DOT)) {
                // Member access: expr.member
                CToken dot = previous();
                CToken member = consume(CTokenType.IDENTIFIER, "Expected member name after '.'");
                if (ERROR_OCCURRED) return null;
                expr = CMemberAccess.make(allocator, expr, member, false, dot);
            } else if (match(CTokenType.ARROW)) {
                // Pointer member access: expr->member
                CToken arrow = previous();
                CToken member = consume(CTokenType.IDENTIFIER, "Expected member name after '->'");
                if (ERROR_OCCURRED) return null;
                expr = CMemberAccess.make(allocator, expr, member, true, arrow);
            } else if (match(CTokenType.PLUS_PLUS) || match(CTokenType.MINUS_MINUS)) {
                // Postfix increment/decrement
                CToken op = previous();
                expr = CUnary.make(allocator, op.type, expr, false, op);
            } else {
                break;
            }
        }

        return expr;
    }

    CExpr* finish_call(CExpr* callee) {
        CToken paren = previous();
        auto args = make_barray!(CExpr*)(allocator);

        if (!check(CTokenType.RIGHT_PAREN)) {
            do {
                CExpr* arg = parse_expression();
                if (arg is null) return null;
                args ~= arg;
            } while (match(CTokenType.COMMA));
        }

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after arguments");
        if (ERROR_OCCURRED) return null;

        return CCall.make(allocator, callee, args[], paren);
    }

    CExpr* parse_primary() {
        CToken tok = peek();

        if (match(CTokenType.NUMBER) || match(CTokenType.HEX)) {
            return CLiteral.make(allocator, previous(), &TYPE_INT);
        }

        if (match(CTokenType.STRING)) {
            // String literal is a char*
            return CLiteral.make(allocator, previous(), make_pointer_type(allocator, &TYPE_CHAR));
        }

        if (match(CTokenType.CHAR_LITERAL)) {
            return CLiteral.make(allocator, previous(), &TYPE_CHAR);
        }

        if (match(CTokenType.IDENTIFIER)) {
            return CIdentifier.make(allocator, previous());
        }

        if (match(CTokenType.LEFT_PAREN)) {
            // Check if this is a cast expression: (type)value
            if (is_type_specifier(peek())) {
                // It's a cast
                CType* cast_type = parse_type();
                if (cast_type is null) return null;
                consume(CTokenType.RIGHT_PAREN, "Expected ')' after cast type");
                if (ERROR_OCCURRED) return null;
                // Parse the value being cast
                CExpr* operand = parse_unary();
                if (operand is null) return null;
                return CCast.make(allocator, cast_type, operand, tok);
            }
            // Regular parenthesized expression
            CExpr* expr = parse_expression();
            if (expr is null) return null;
            consume(CTokenType.RIGHT_PAREN, "Expected ')' after expression");
            if (ERROR_OCCURRED) return null;
            return CGrouping.make(allocator, expr, tok);
        }

        error("Expected expression");
        return null;
    }

    // =========================================================================
    // Utility Methods
    // =========================================================================

    CToken consume(CTokenType type, str message) {
        if (check(type)) return advance();
        error(peek(), message);
        return peek();
    }

    bool match(CTokenType type) {
        if (check(type)) {
            advance();
            return true;
        }
        return false;
    }

    bool check(CTokenType type) {
        if (at_end) return false;
        return peek().type == type;
    }

    bool check_next(CTokenType type) {
        if (current + 1 >= tokens.length) return false;
        return tokens[current + 1].type == type;
    }

    CToken advance() {
        if (!at_end) current++;
        return previous();
    }

    bool at_end() {
        return peek().type == CTokenType.EOF;
    }

    // Skip balanced parentheses including nested ones
    void skip_balanced_parens() {
        if (!check(CTokenType.LEFT_PAREN)) return;
        advance();  // consume '('
        int depth = 1;
        while (depth > 0 && !at_end) {
            if (check(CTokenType.LEFT_PAREN)) depth++;
            else if (check(CTokenType.RIGHT_PAREN)) depth--;
            advance();
        }
    }

    CToken peek() {
        return tokens[current];
    }

    CToken peek_at(int offset) {
        if (current + offset >= tokens.length)
            return tokens[$ - 1];  // Return EOF
        return tokens[current + offset];
    }

    CToken previous() {
        return tokens[current - 1];
    }
}
