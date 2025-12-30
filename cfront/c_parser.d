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

        // Initialize struct/union types tables
        struct_types.data.allocator = allocator;
        union_types.data.allocator = allocator;

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
                // Check if this is a struct definition
                // Look ahead: struct Name { ... means definition
                // struct Name var... means variable of struct type
                if (peek_at(1).type == CTokenType.IDENTIFIER &&
                    peek_at(2).type == CTokenType.LEFT_BRACE) {
                    CStructDef sdef;
                    int err = parse_struct_def(&sdef);
                    if (err) return err;
                    structs ~= sdef;
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
                // Check if this is a union definition
                // Look ahead: union Name { ... means definition
                // union Name var... means variable of union type
                if (peek_at(1).type == CTokenType.IDENTIFIER &&
                    peek_at(2).type == CTokenType.LEFT_BRACE) {
                    CUnionDef udef;
                    int err = parse_union_def(&udef);
                    if (err) return err;
                    unions ~= udef;
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
                CToken size_tok = consume(CTokenType.NUMBER, "Expected array size");
                if (ERROR_OCCURRED) return 1;
                size_t arr_size = 0;
                foreach (c; size_tok.lexeme) {
                    arr_size = arr_size * 10 + (c - '0');
                }
                consume(CTokenType.RIGHT_BRACKET, "Expected ']' after array size");
                if (ERROR_OCCURRED) return 1;
                field_type = make_array_type(allocator, field_type, arr_size);
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
                CToken size_tok = consume(CTokenType.NUMBER, "Expected array size");
                if (ERROR_OCCURRED) return 1;
                size_t arr_size = 0;
                foreach (c; size_tok.lexeme) {
                    arr_size = arr_size * 10 + (c - '0');
                }
                consume(CTokenType.RIGHT_BRACKET, "Expected ']' after array size");
                if (ERROR_OCCURRED) return 1;
                field_type = make_array_type(allocator, field_type, arr_size);
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
        if (is_type_specifier(peek().type)) {
            return parse_var_decl();
        }

        // Expression statement
        return parse_expr_stmt();
    }

    bool is_type_specifier(CTokenType type) {
        with (CTokenType) {
            return type == VOID || type == CHAR || type == SHORT ||
                   type == INT || type == LONG || type == UNSIGNED ||
                   type == SIGNED || type == CONST || type == STRUCT ||
                   type == UNION;
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
            if (is_type_specifier(peek().type)) {
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

        // Check for array declaration: int arr[10]
        if (match(CTokenType.LEFT_BRACKET)) {
            CToken size_tok = consume(CTokenType.NUMBER, "Expected array size");
            if (ERROR_OCCURRED) return null;

            // Parse the size as an integer
            import dlib.parse_numbers : parse_unsigned_human;
            auto parsed = parse_unsigned_human(size_tok.lexeme);
            if (parsed.errored || parsed.value == 0) {
                error("Invalid array size");
                return null;
            }

            consume(CTokenType.RIGHT_BRACKET, "Expected ']' after array size");
            if (ERROR_OCCURRED) return null;

            // Create array type
            var_type = make_array_type(allocator, var_type, parsed.value);
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
