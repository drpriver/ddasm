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
        fprintf(stderr, "[line %d, col %d]: Parse Error at '%.*s': %.*s\n",
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

        while (!at_end) {
            // Handle #pragma
            if (check(CTokenType.PRAGMA)) {
                handle_pragma();
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
            // Parse field type
            CType* field_type = parse_type();
            if (field_type is null) return 1;

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
            // Parse field type
            CType* field_type = parse_type();
            if (field_type is null) return 1;

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
    // Supports: integer literals, enum constant references, unary minus, +/-, comparisons
    ConstExprResult parse_enum_const_expr() {
        return parse_enum_const_equality();
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
        auto result = parse_enum_const_additive();
        if (result.err) return result;

        while (check(CTokenType.LESS) || check(CTokenType.GREATER) ||
               check(CTokenType.LESS_EQUAL) || check(CTokenType.GREATER_EQUAL)) {
            CTokenType op = peek().type;
            advance();
            auto right = parse_enum_const_additive();
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

    ConstExprResult parse_enum_const_additive() {
        auto result = parse_enum_const_unary();
        if (result.err) return result;

        while (check(CTokenType.PLUS) || check(CTokenType.MINUS)) {
            bool is_plus = check(CTokenType.PLUS);
            advance();
            auto right = parse_enum_const_unary();
            if (right.err) return right;
            if (is_plus) {
                result.value = result.value + right.value;
            } else {
                result.value = result.value - right.value;
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

        if (match(CTokenType.IDENTIFIER)) {
            CToken tok = previous();
            // Look up in already-defined enum constants
            if (long* val = tok.lexeme in enum_constants) {
                result.value = *val;
                return result;
            }
            error(tok, "Unknown enum constant in expression");
            result.err = true;
            return result;
        }

        if (match(CTokenType.LEFT_PAREN)) {
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
                    CType* field_type = parse_type();
                    if (field_type is null) return 1;

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
                        field_type = make_array_type(allocator, field_type, cast(size_t) size_result.value);
                    }

                    consume(CTokenType.SEMICOLON, "Expected ';' after field");
                    if (ERROR_OCCURRED) return 1;

                    StructField field;
                    field.name = field_name.lexeme;
                    field.type = field_type;
                    field.offset = offset;
                    fields ~= field;
                    offset += field_type.size_of();
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
                    error("Unknown struct type");
                    return 1;
                }
            }

            // Now get the typedef name
            CToken typedef_name = consume(CTokenType.IDENTIFIER, "Expected typedef name");
            if (ERROR_OCCURRED) return 1;

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
                    error("Unknown union type");
                    return 1;
                }
            }

            CToken typedef_name = consume(CTokenType.IDENTIFIER, "Expected typedef name");
            if (ERROR_OCCURRED) return 1;

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

            consume(CTokenType.SEMICOLON, "Expected ';' after typedef");
            if (ERROR_OCCURRED) return 1;

            typedef_types[typedef_name.lexeme] = base_type;
            return 0;
        }
    }

    int parse_extern_decl(CExternDecl* decl) {
        advance();  // consume 'extern'

        // Parse return type
        CType* ret_type = parse_type();
        if (ret_type is null) return 1;

        // Parse function name
        CToken name = consume(CTokenType.IDENTIFIER, "Expected function name");
        if (ERROR_OCCURRED) return 1;

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

                CParam param;
                param.type = parse_type();
                if (param.type is null) return 1;

                // Parameter name is optional in declarations
                if (check(CTokenType.IDENTIFIER)) {
                    param.name = advance();
                }
                params ~= param;
            } while (match(CTokenType.COMMA));
        }

        consume(CTokenType.RIGHT_PAREN, "Expected ')' after parameters");
        if (ERROR_OCCURRED) return 1;

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

                CParam param;
                param.type = parse_type();
                if (param.type is null) return 1;

                param.name = consume(CTokenType.IDENTIFIER, "Expected parameter name");
                if (ERROR_OCCURRED) return 1;

                params ~= param;
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
            // Allocate new type for short
            auto data = allocator.alloc(CType.sizeof);
            result = cast(CType*)data.ptr;
            result.kind = CTypeKind.SHORT;
            result.is_unsigned = is_unsigned;
        } else if (match(CTokenType.INT)) {
            result = is_unsigned ? &TYPE_UINT : &TYPE_INT;
        } else if (match(CTokenType.LONG)) {
            result = is_unsigned ? &TYPE_ULONG : &TYPE_LONG;
        } else if (match(CTokenType.FLOAT)) {
            result = &TYPE_FLOAT;
        } else if (match(CTokenType.DOUBLE)) {
            result = &TYPE_DOUBLE;
        } else if (match(CTokenType.STRUCT)) {
            // struct Name
            CToken name = consume(CTokenType.IDENTIFIER, "Expected struct name");
            // Look up struct in defined structs
            if (CType** found = name.lexeme in struct_types) {
                result = *found;
            } else {
                error("Unknown struct type");
                return null;
            }
        } else if (match(CTokenType.UNION)) {
            // union Name
            CToken name = consume(CTokenType.IDENTIFIER, "Expected union name");
            // Look up union in defined unions
            if (CType** found = name.lexeme in union_types) {
                result = *found;
            } else {
                error("Unknown union type");
                return null;
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
