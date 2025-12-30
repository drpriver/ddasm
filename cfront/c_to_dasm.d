/*
 * C to DASM Code Generator for ddasm
 * Copyright 2025, David Priver
 */
module cfront.c_to_dasm;

import core.stdc.stdio : fprintf, stderr;
import dlib.aliases;
import dlib.allocator : Allocator;
import dlib.barray : Barray, make_barray;
import dlib.stringbuilder : StringBuilder, P;
import dlib.table : Table;
import dlib.parse_numbers : parse_unsigned_human;

import cfront.c_tokenizer : CToken, CTokenType;
import cfront.c_ast;

struct RegisterAllocator {
    int alloced;
    int local_max = 0;

    int allocate() {
        int result = alloced++;
        if (result > local_max) local_max = result;
        return result;
    }

    void reset() { alloced = 0; }
    void reset_to(int r) { alloced = r; }
}

struct LabelAllocator {
    int nalloced;
    int allocate() { return nalloced++; }
    void reset() { nalloced = 0; }
}

// Analysis pass to detect which variables need stack allocation
struct CAnalyzer {
    Table!(str, bool) addr_taken;  // Variables whose address is taken
    Table!(str, bool) arrays;      // Array variables (always need stack)
    Barray!(str) all_vars;         // All local variable names
    Allocator allocator;

    void analyze_function(CFunction* func) {
        addr_taken.data.allocator = allocator;
        arrays.data.allocator = allocator;
        all_vars.bdata.allocator = allocator;
        addr_taken.cleanup();
        arrays.cleanup();
        all_vars.clear();

        foreach (stmt; func.body) {
            analyze_stmt(stmt);
        }
    }

    bool should_use_stack() {
        // Use stack if any address is taken, any arrays, or more than 4 variables
        return addr_taken.count > 0 || arrays.count > 0 || all_vars[].length > 4;
    }

    void analyze_stmt(CStmt* stmt) {
        if (stmt is null) return;
        final switch (stmt.kind) with (CStmtKind) {
            case EXPR:
                analyze_expr((cast(CExprStmt*)stmt).expression);
                break;
            case RETURN:
                if ((cast(CReturnStmt*)stmt).value)
                    analyze_expr((cast(CReturnStmt*)stmt).value);
                break;
            case IF:
                auto s = cast(CIfStmt*)stmt;
                analyze_expr(s.condition);
                analyze_stmt(s.then_branch);
                if (s.else_branch) analyze_stmt(s.else_branch);
                break;
            case WHILE:
                auto s = cast(CWhileStmt*)stmt;
                analyze_expr(s.condition);
                analyze_stmt(s.body);
                break;
            case FOR:
                auto s = cast(CForStmt*)stmt;
                if (s.init_stmt) analyze_stmt(s.init_stmt);
                if (s.condition) analyze_expr(s.condition);
                if (s.increment) analyze_expr(s.increment);
                analyze_stmt(s.body_);
                break;
            case BLOCK:
                foreach (s; (cast(CBlock*)stmt).statements)
                    analyze_stmt(s);
                break;
            case VAR_DECL:
                auto decl = cast(CVarDecl*)stmt;
                all_vars.push(decl.name.lexeme);  // Track variable name
                // Track array variables
                if (decl.var_type && decl.var_type.is_array()) {
                    arrays[decl.name.lexeme] = true;
                }
                if (decl.initializer)
                    analyze_expr(decl.initializer);
                break;
            case BREAK:
            case CONTINUE:
            case EMPTY:
                break;
        }
    }

    void analyze_expr(CExpr* expr) {
        if (expr is null) return;
        expr = expr.ungroup();

        final switch (expr.kind) with (CExprKind) {
            case LITERAL:
            case IDENTIFIER:
                break;
            case BINARY:
                auto e = cast(CBinary*)expr;
                analyze_expr(e.left);
                analyze_expr(e.right);
                break;
            case UNARY:
                auto e = cast(CUnary*)expr;
                // Check for address-of operator
                if (e.op == CTokenType.AMP) {
                    if (auto id = e.operand.as_identifier()) {
                        addr_taken[id.name.lexeme] = true;
                    }
                }
                analyze_expr(e.operand);
                break;
            case CALL:
                auto e = cast(CCall*)expr;
                analyze_expr(e.callee);
                foreach (arg; e.args)
                    analyze_expr(arg);
                break;
            case ASSIGN:
                auto e = cast(CAssign*)expr;
                analyze_expr(e.target);
                analyze_expr(e.value);
                break;
            case SUBSCRIPT:
                auto e = cast(CSubscript*)expr;
                analyze_expr(e.array);
                analyze_expr(e.index);
                break;
            case GROUPING:
                break;  // Already ungrouped
            case CAST:
            case MEMBER_ACCESS:
            case SIZEOF:
                break;
        }
    }

    void cleanup() {
        addr_taken.cleanup();
        arrays.cleanup();
        all_vars.cleanup();
    }
}

struct CDasmWriter {
    Allocator allocator;
    StringBuilder* sb;
    RegisterAllocator regallocator;
    LabelAllocator labelallocator;

    // Variable tracking
    Table!(str, int) reglocals;      // Variables in registers
    Table!(str, int) stacklocals;    // Variables on stack (offset from rbp)
    Table!(str, CType*) var_types;   // Variable types (for pointer arithmetic)
    Table!(str, str) extern_funcs;   // Map: function name -> module alias
    Table!(str, bool) addr_taken;    // Variables whose address is taken
    Table!(str, bool) arrays;        // Array variables (need special handling)
    Table!(str, CType*) global_types; // Global variable types

    // Loop control
    int current_continue_target = -1;
    int current_break_target = -1;

    bool ERROR_OCCURRED = false;
    bool use_stack = false;  // Set true when we need stack-based locals
    int funcdepth = 0;
    int stack_offset = 1;    // Current stack offset for locals (start at 1, slot 0 is saved RBP)

    enum { TARGET_IS_NOTHING = -1 }
    enum { RARG1 = 10 }  // First argument register (rarg1 = r10)
    // Note: Return value uses named register 'rout1', not a numeric register

    // Get the type of an expression (for pointer arithmetic scaling)
    CType* get_expr_type(CExpr* e) {
        if (e is null) return null;
        e = e.ungroup();

        // If type is already set, return it
        if (e.type !is null) return e.type;

        final switch (e.kind) with (CExprKind) {
            case LITERAL:
                auto lit = e.as_literal;
                if (lit.value.type == CTokenType.STRING)
                    return &TYPE_CHAR_PTR;
                if (lit.value.type == CTokenType.CHAR_LITERAL)
                    return &TYPE_CHAR;
                return &TYPE_INT;  // Integer literals are int
            case IDENTIFIER:
                auto id = e.as_identifier;
                if (auto t = id.name.lexeme in var_types) {
                    CType* typ = *t;
                    // Arrays decay to pointers
                    if (typ.is_array()) {
                        // Return pointer to element type
                        // For type checking purposes, we can use the element_type
                        // but treat it as a pointer for arithmetic
                        return typ;  // Array type has element_size() for arithmetic
                    }
                    return typ;
                }
                if (auto t = id.name.lexeme in global_types)
                    return *t;
                return null;
            case BINARY:
                auto bin = e.as_binary;
                // For arithmetic, result type follows the pointer if present
                auto lt = get_expr_type(bin.left);
                auto rt = get_expr_type(bin.right);
                if (lt && lt.is_pointer()) return lt;
                if (rt && rt.is_pointer()) return rt;
                return lt ? lt : rt;
            case UNARY:
                auto un = e.as_unary;
                if (un.op == CTokenType.STAR) {
                    // Dereference: *ptr -> pointed-to type
                    auto pt = get_expr_type(un.operand);
                    if (pt && pt.is_pointer()) return pt.pointed_to;
                }
                if (un.op == CTokenType.AMP) {
                    // Address-of: &x -> pointer to x's type
                    return null;  // Would need to construct pointer type
                }
                return get_expr_type(un.operand);
            case CALL:
                // Would need function return type tracking
                return null;
            case ASSIGN:
                return get_expr_type((cast(CAssign*)e).target);
            case SUBSCRIPT:
                auto sub = e.as_subscript;
                auto at = get_expr_type(sub.array);
                if (at && (at.is_pointer() || at.is_array())) return at.pointed_to;
                return null;
            case CAST:
            case MEMBER_ACCESS:
            case SIZEOF:
            case GROUPING:
                return null;
        }
    }

    // Get the read instruction for a given type size
    static str read_instr_for_size(size_t size) {
        switch (size) {
            case 1: return "read1";
            case 2: return "read2";
            case 4: return "read4";
            default: return "read";
        }
    }

    // Get the write instruction for a given type size
    static str write_instr_for_size(size_t size) {
        switch (size) {
            case 1: return "write1";
            case 2: return "write2";
            case 4: return "write4";
            default: return "write";
        }
    }

    @disable this();

    this(StringBuilder* s, Allocator a) {
        allocator = a;
        sb = s;
        reglocals.data.allocator = a;
        stacklocals.data.allocator = a;
        var_types.data.allocator = a;
        extern_funcs.data.allocator = a;
        addr_taken.data.allocator = a;
        arrays.data.allocator = a;
        global_types.data.allocator = a;
    }

    void cleanup() {
        reglocals.cleanup();
        stacklocals.cleanup();
        var_types.cleanup();
        extern_funcs.cleanup();
        addr_taken.cleanup();
        arrays.cleanup();
        global_types.cleanup();
    }

    void error(CToken token, str message) {
        ERROR_OCCURRED = true;
        fprintf(stderr, "[line %d]: Code gen error at '%.*s': %.*s\n",
                token.line,
                cast(int)token.lexeme.length, token.lexeme.ptr,
                cast(int)message.length, message.ptr);
    }

    // Generate a module alias from library name
    // "libc.so.6" -> "Libc", "python3.8" -> "Python3", "libfoo.so" -> "Foo"
    static str make_alias(str lib, int counter) {
        // Simple approach: use Lib0, Lib1, etc. for uniqueness
        // Could be smarter and extract name from library path
        __gshared char[16][8] alias_buffers;
        if (counter >= 8) counter = counter % 8;

        import core.stdc.stdio : snprintf;
        int len = snprintf(alias_buffers[counter].ptr, 16, "Lib%d", counter);
        return cast(str)alias_buffers[counter][0 .. len];
    }

    // =========================================================================
    // Main Entry Point
    // =========================================================================

    int generate(CTranslationUnit* unit) {
        // Generate dlimport blocks for extern declarations, grouped by library
        if (unit.externs.length > 0) {
            // First pass: collect unique libraries and assign aliases
            Table!(str, str) lib_to_alias;
            lib_to_alias.data.allocator = allocator;
            scope(exit) lib_to_alias.cleanup();

            int lib_counter = 0;
            foreach (ref ext; unit.externs) {
                str lib = ext.library.length ? ext.library : "libc.so.6";
                if (lib !in lib_to_alias) {
                    // Generate alias from library name
                    str alias_name = make_alias(lib, lib_counter++);
                    lib_to_alias[lib] = alias_name;
                }
                // Track function -> module alias mapping
                extern_funcs[ext.name.lexeme] = lib_to_alias[lib];
            }

            // Second pass: generate dlimport blocks per library
            foreach (ref ext; unit.externs) {
                str lib = ext.library.length ? ext.library : "libc.so.6";
                str alias_name = lib_to_alias[lib];

                // Check if we already started this library's block
                // We'll use a simple approach: emit header only for first function of each lib
                bool first_for_lib = true;
                foreach (ref prev; unit.externs) {
                    if (&prev is &ext) break;
                    str prev_lib = prev.library.length ? prev.library : "libc.so.6";
                    if (prev_lib == lib) {
                        first_for_lib = false;
                        break;
                    }
                }

                if (first_for_lib) {
                    sb.writef("dlimport %\n", alias_name);
                    sb.writef("  \"%\"\n", lib);

                    // Emit all functions for this library
                    foreach (ref func_ext; unit.externs) {
                        str func_lib = func_ext.library.length ? func_ext.library : "libc.so.6";
                        if (func_lib == lib) {
                            ubyte n_ret = func_ext.return_type.is_void() ? 0 : 1;
                            sb.writef("  % % %", func_ext.name.lexeme, func_ext.params.length, n_ret);
                            if (func_ext.is_varargs) sb.write(" varargs");
                            sb.write("\n");
                        }
                    }
                    sb.write("end\n\n");
                }
            }
        }

        // Generate global variables
        foreach (ref gvar; unit.globals) {
            // Track global type
            global_types[gvar.name.lexeme] = gvar.var_type;

            // Generate var declaration
            sb.writef("var % ", gvar.name.lexeme);
            if (gvar.initializer !is null) {
                // Only support constant initializers for now
                if (CLiteral* lit = gvar.initializer.as_literal()) {
                    sb.writef("%\n", lit.value.lexeme);
                } else {
                    // Non-constant initializer - initialize to 0, will set in start
                    sb.write("0\n");
                }
            } else {
                sb.write("0\n");
            }
        }
        if (unit.globals.length > 0) sb.write("\n");

        // Generate functions and track if we have main/start
        bool has_main = false;
        bool has_start = false;

        foreach (ref func; unit.functions) {
            if (func.is_definition) {
                if (func.name.lexeme == "main") has_main = true;
                if (func.name.lexeme == "start") has_start = true;
                int err = gen_function(&func);
                if (err) return err;
            }
        }

        // If there's a main() but no start(), generate start wrapper
        if (has_main && !has_start) {
            sb.write("function start 0\n");
            sb.write("    call function main 0\n");
            sb.write("    ret\n");
            sb.write("end\n");
        }

        return 0;
    }

    // =========================================================================
    // Helper Functions
    // =========================================================================

    // Parse a character literal lexeme (e.g., "'q'" or "'\n'") and return its integer value
    static int parse_char_literal(str lex) {
        if (lex.length < 3) return 0;
        char c = lex[1];  // Skip opening quote
        if (c == '\\' && lex.length >= 4) {
            switch (lex[2]) {
                case 'n': c = '\n'; break;
                case 't': c = '\t'; break;
                case 'r': c = '\r'; break;
                case '0': c = '\0'; break;
                case '\\': c = '\\'; break;
                case '\'': c = '\''; break;
                default: c = lex[2]; break;
            }
        }
        return cast(int)c;
    }

    // Count total stack slots needed for variable declarations in statements
    static int count_var_slots(CStmt*[] stmts) {
        int count = 0;
        foreach (stmt; stmts) {
            if (stmt.kind == CStmtKind.VAR_DECL) {
                auto decl = cast(CVarDecl*)stmt;
                count += cast(int)decl.var_type.stack_slots();
            } else if (stmt.kind == CStmtKind.BLOCK) {
                count += count_var_slots((cast(CBlock*)stmt).statements);
            } else if (stmt.kind == CStmtKind.IF) {
                auto s = cast(CIfStmt*)stmt;
                if (s.then_branch) count += count_var_slots((&s.then_branch)[0..1]);
                if (s.else_branch) count += count_var_slots((&s.else_branch)[0..1]);
            } else if (stmt.kind == CStmtKind.WHILE) {
                auto s = cast(CWhileStmt*)stmt;
                if (s.body) count += count_var_slots((&s.body)[0..1]);
            } else if (stmt.kind == CStmtKind.FOR) {
                auto s = cast(CForStmt*)stmt;
                if (s.init_stmt) count += count_var_slots((&s.init_stmt)[0..1]);
                if (s.body_) count += count_var_slots((&s.body_)[0..1]);
            }
        }
        return count;
    }

    // =========================================================================
    // Function Generation
    // =========================================================================

    int gen_function(CFunction* func) {
        funcdepth++;
        scope(exit) {
            funcdepth--;
            reglocals.cleanup();
            stacklocals.cleanup();
            var_types.cleanup();
            addr_taken.cleanup();
            arrays.cleanup();
            regallocator.reset();
            labelallocator.reset();
            use_stack = false;
            stack_offset = 1;  // Reset to 1 (slot 0 is saved RBP)
        }

        // Run analysis to detect address-taken variables
        CAnalyzer analyzer;
        analyzer.allocator = allocator;
        analyzer.analyze_function(func);
        scope(exit) analyzer.cleanup();

        // Copy address-taken and array info
        foreach (ref item; analyzer.addr_taken.items()) {
            addr_taken[item.key] = true;
        }
        foreach (ref item; analyzer.arrays.items()) {
            arrays[item.key] = true;
        }
        // Use stack if any address is taken OR if we have many variables (> 4)
        use_stack = analyzer.should_use_stack();

        // First, count how many stack slots we need
        int num_stack_slots = 0;
        if (use_stack) {
            // Count parameters (1 slot each)
            num_stack_slots = cast(int)func.params.length;
            // Count local variables, accounting for array sizes
            num_stack_slots += count_var_slots(func.body);
        }

        // Emit function header
        sb.writef("function % %\n", func.name.lexeme, func.params.length);

        // Set up stack frame if we have address-taken variables
        if (use_stack) {
            sb.write("    push rbp\n");
            sb.write("    move rbp rsp\n");
            // Allocate all stack slots upfront (slots 1..n, so need n+1 slots total for push safety)
            sb.writef("    add rsp rsp %\n", P(num_stack_slots + 1));
        }

        // Move arguments from rarg registers to locals
        foreach (i, ref param; func.params) {
            str pname = param.name.lexeme;
            var_types[pname] = param.type;

            if (use_stack) {
                // All parameters go on the stack when use_stack is true
                int slot = stack_offset++;
                stacklocals[pname] = slot;
                sb.writef("    local_write % r%\n", P(slot), RARG1 + cast(int)i);
            } else {
                int r = regallocator.allocate();
                reglocals[pname] = r;
                sb.writef("    move r% r%\n", r, RARG1 + cast(int)i);
            }
        }

        // Generate body
        foreach (stmt; func.body) {
            int err = gen_statement(stmt);
            if (err) return err;
        }

        // Add implicit return if needed
        if (func.body.length == 0 || func.body[$ - 1].kind != CStmtKind.RETURN) {
            if (use_stack) {
                sb.write("    move rsp rbp\n");
                sb.write("    pop rbp\n");
            }
            sb.write("    ret\n");
        }

        sb.write("end\n\n");
        return 0;
    }

    // =========================================================================
    // Statement Generation
    // =========================================================================

    int gen_statement(CStmt* stmt) {
        final switch (stmt.kind) with (CStmtKind) {
            case EXPR:     return gen_expr_stmt(cast(CExprStmt*)stmt);
            case RETURN:   return gen_return(cast(CReturnStmt*)stmt);
            case IF:       return gen_if(cast(CIfStmt*)stmt);
            case WHILE:    return gen_while(cast(CWhileStmt*)stmt);
            case FOR:      return gen_for(cast(CForStmt*)stmt);
            case BLOCK:    return gen_block(cast(CBlock*)stmt);
            case VAR_DECL: return gen_var_decl(cast(CVarDecl*)stmt);
            case BREAK:    return gen_break(cast(CBreakStmt*)stmt);
            case CONTINUE: return gen_continue(cast(CContinueStmt*)stmt);
            case EMPTY:    return 0;
        }
    }

    int gen_expr_stmt(CExprStmt* stmt) {
        int before = regallocator.alloced;
        int err = gen_expression(stmt.expression, TARGET_IS_NOTHING);
        regallocator.reset_to(before);
        return err;
    }

    int gen_return(CReturnStmt* stmt) {
        if (stmt.value !is null) {
            int before = regallocator.alloced;
            int temp = regallocator.allocate();
            int err = gen_expression(stmt.value, temp);
            if (err) return err;
            regallocator.reset_to(before);
            sb.writef("    move rout1 r%\n", temp);
        }
        if (use_stack) {
            sb.write("    move rsp rbp\n");
            sb.write("    pop rbp\n");
        }
        sb.write("    ret\n");
        return 0;
    }

    int gen_if(CIfStmt* stmt) {
        int after_label = labelallocator.allocate();

        // Generate condition
        int before = regallocator.alloced;
        int cond = regallocator.allocate();
        int err = gen_expression(stmt.condition, cond);
        if (err) return err;
        regallocator.reset_to(before);

        sb.writef("    cmp r% 0\n", cond);

        if (stmt.else_branch !is null) {
            int else_label = labelallocator.allocate();
            sb.writef("    jump eq label L%\n", else_label);

            err = gen_statement(stmt.then_branch);
            if (err) return err;

            sb.writef("    move rip label L%\n", after_label);
            sb.writef("  label L%\n", else_label);

            err = gen_statement(stmt.else_branch);
            if (err) return err;
        } else {
            sb.writef("    jump eq label L%\n", after_label);
            err = gen_statement(stmt.then_branch);
            if (err) return err;
        }

        sb.writef("  label L%\n", after_label);
        return 0;
    }

    int gen_while(CWhileStmt* stmt) {
        int prev_continue = current_continue_target;
        int prev_break = current_break_target;
        scope(exit) {
            current_continue_target = prev_continue;
            current_break_target = prev_break;
        }

        int top_label = labelallocator.allocate();
        int after_label = labelallocator.allocate();
        current_continue_target = top_label;
        current_break_target = after_label;

        sb.writef("  label L%\n", top_label);

        // Generate condition
        int before = regallocator.alloced;
        int cond = regallocator.allocate();
        int err = gen_expression(stmt.condition, cond);
        if (err) return err;
        regallocator.reset_to(before);

        sb.writef("    cmp r% 0\n", cond);
        sb.writef("    jump eq label L%\n", after_label);

        err = gen_statement(stmt.body);
        if (err) return err;

        sb.writef("    move rip label L%\n", top_label);
        sb.writef("  label L%\n", after_label);
        return 0;
    }

    int gen_for(CForStmt* stmt) {
        int prev_continue = current_continue_target;
        int prev_break = current_break_target;
        scope(exit) {
            current_continue_target = prev_continue;
            current_break_target = prev_break;
        }

        // Initializer
        if (stmt.init_stmt !is null) {
            int err = gen_statement(stmt.init_stmt);
            if (err) return err;
        }

        int top_label = labelallocator.allocate();
        int incr_label = labelallocator.allocate();
        int after_label = labelallocator.allocate();
        current_continue_target = incr_label;
        current_break_target = after_label;

        sb.writef("  label L%\n", top_label);

        // Condition
        if (stmt.condition !is null) {
            int before = regallocator.alloced;
            int cond = regallocator.allocate();
            int err = gen_expression(stmt.condition, cond);
            if (err) return err;
            regallocator.reset_to(before);

            sb.writef("    cmp r% 0\n", cond);
            sb.writef("    jump eq label L%\n", after_label);
        }

        // Body
        int err = gen_statement(stmt.body_);
        if (err) return err;

        // Increment
        sb.writef("  label L%\n", incr_label);
        if (stmt.increment !is null) {
            int before = regallocator.alloced;
            err = gen_expression(stmt.increment, TARGET_IS_NOTHING);
            if (err) return err;
            regallocator.reset_to(before);
        }

        sb.writef("    move rip label L%\n", top_label);
        sb.writef("  label L%\n", after_label);
        return 0;
    }

    int gen_block(CBlock* stmt) {
        foreach (s; stmt.statements) {
            int err = gen_statement(s);
            if (err) return err;
        }
        return 0;
    }

    int gen_var_decl(CVarDecl* stmt) {
        str name = stmt.name.lexeme;
        var_types[name] = stmt.var_type;

        // Check if this is an array
        bool is_array = stmt.var_type.is_array();

        if (use_stack) {
            // All variables go on stack when use_stack is true
            int slot = stack_offset;

            if (is_array) {
                // Arrays need multiple slots
                size_t arr_size = stmt.var_type.array_size;
                stack_offset += cast(int)arr_size;

                // Handle array initializer (string literal for char arrays)
                if (stmt.initializer !is null) {
                    if (CLiteral* lit = stmt.initializer.as_literal()) {
                        if (lit.value.type == CTokenType.STRING) {
                            // String literal initializer for char array
                            // Compute string length (excluding quotes, accounting for escapes)
                            str lex = lit.value.lexeme;
                            size_t str_len = 0;
                            for (size_t i = 1; i < lex.length - 1; i++) {
                                if (lex[i] == '\\' && i + 1 < lex.length - 1) i++;  // Skip escape
                                str_len++;
                            }
                            str_len++;  // Include null terminator

                            size_t copy_size = str_len < arr_size ? str_len : arr_size;

                            int before = regallocator.alloced;
                            int dst_reg = regallocator.allocate();
                            int src_reg = regallocator.allocate();

                            // dst = address of array on stack
                            sb.writef("    add r% rbp %\n", dst_reg, P(slot));
                            // Zero-initialize the array first (for when string is shorter than array)
                            sb.writef("    memzero r% %\n", dst_reg, arr_size);
                            // src = string literal address
                            sb.writef("    move r% %\n", src_reg, lex);
                            // memcpy dst src size
                            sb.writef("    memcpy r% r% %\n", dst_reg, src_reg, copy_size);

                            regallocator.reset_to(before);
                        } else {
                            error(stmt.stmt.token, "Array initializers must be string literals");
                            return 1;
                        }
                    } else {
                        error(stmt.stmt.token, "Array initializers must be string literals");
                        return 1;
                    }
                }
            } else {
                // Regular variable needs one slot
                stack_offset++;

                // Initialize on stack using local_write
                if (stmt.initializer !is null) {
                    int before = regallocator.alloced;
                    int temp = regallocator.allocate();
                    int err = gen_expression(stmt.initializer, temp);
                    if (err) return err;
                    sb.writef("    local_write % r%\n", P(slot), temp);
                    regallocator.reset_to(before);
                } else {
                    // Default initialize to 0
                    sb.writef("    local_write % 0\n", P(slot));
                }
            }
            stacklocals[name] = slot;
        } else {
            // Allocate register for variable (arrays can't go here)
            int r = regallocator.allocate();
            reglocals[name] = r;

            // Initialize if needed
            if (stmt.initializer !is null) {
                int err = gen_expression(stmt.initializer, r);
                if (err) return err;
            } else {
                // Default initialize to 0
                sb.writef("    move r% 0\n", r);
            }
        }

        return 0;
    }

    int gen_break(CBreakStmt* stmt) {
        if (current_break_target == -1) {
            error(stmt.stmt.token, "'break' outside of loop");
            return 1;
        }
        sb.writef("    move rip label L%\n", current_break_target);
        return 0;
    }

    int gen_continue(CContinueStmt* stmt) {
        if (current_continue_target == -1) {
            error(stmt.stmt.token, "'continue' outside of loop");
            return 1;
        }
        sb.writef("    move rip label L%\n", current_continue_target);
        return 0;
    }

    // =========================================================================
    // Expression Generation
    // =========================================================================

    int gen_expression(CExpr* e, int target) {
        e = e.ungroup();

        final switch (e.kind) with (CExprKind) {
            case LITERAL:    return gen_literal(cast(CLiteral*)e, target);
            case IDENTIFIER: return gen_identifier(cast(CIdentifier*)e, target);
            case BINARY:     return gen_binary(cast(CBinary*)e, target);
            case UNARY:      return gen_unary(cast(CUnary*)e, target);
            case CALL:       return gen_call(cast(CCall*)e, target);
            case ASSIGN:     return gen_assign(cast(CAssign*)e, target);
            case SUBSCRIPT:  return gen_subscript(cast(CSubscript*)e, target);
            case GROUPING:   assert(0);  // Should be ungrouped
            case CAST:       return 0;   // TODO
            case MEMBER_ACCESS: return 0; // TODO
            case SIZEOF:     return 0;   // TODO
        }
    }

    int gen_literal(CLiteral* expr, int target) {
        if (target == TARGET_IS_NOTHING) return 0;

        str lex = expr.value.lexeme;

        if (expr.value.type == CTokenType.STRING) {
            // String literal - emit as-is (DASM handles strings)
            sb.writef("    move r% %\n", target, lex);
        } else if (expr.value.type == CTokenType.CHAR_LITERAL) {
            // Character literal - parse the value
            sb.writef("    move r% %\n", target, parse_char_literal(lex));
        } else if (expr.value.type == CTokenType.HEX) {
            // Hex literal - DASM should handle 0x prefix
            sb.writef("    move r% %\n", target, lex);
        } else {
            // Integer literal
            sb.writef("    move r% %\n", target, lex);
        }

        return 0;
    }

    int gen_identifier(CIdentifier* expr, int target) {
        if (target == TARGET_IS_NOTHING) return 0;

        str name = expr.name.lexeme;

        // Check if this is an array - arrays decay to pointers (address of first element)
        if (CType** vt = name in var_types) {
            if ((*vt).is_array()) {
                if (int* offset = name in stacklocals) {
                    // Array-to-pointer decay: compute address of first element
                    sb.writef("    add r% rbp %\n", target, P(*offset));
                    return 0;
                }
            }
        }

        if (int* r = name in reglocals) {
            if (target == *r) return 0;  // Already in target register
            sb.writef("    move r% r%\n", target, *r);
            return 0;
        }

        if (int* offset = name in stacklocals) {
            // Read from stack
            sb.writef("    local_read r% %\n", target, P(*offset));
            return 0;
        }

        // Must be a global or function
        // Get the global's type for sized read
        if (CType** gtype = name in global_types) {
            size_t var_size = (*gtype) ? (*gtype).size_of() : 8;
            // Get address into a temp register, then do sized read
            int before = regallocator.alloced;
            int addr_reg = regallocator.allocate();
            sb.writef("    move r% var %\n", addr_reg, name);
            sb.writef("    % r% r%\n", read_instr_for_size(var_size), target, addr_reg);
            regallocator.reset_to(before);
        } else {
            // Unknown global (function pointer?) - use regular read
            sb.writef("    read r% var %\n", target, name);
        }
        return 0;
    }

    int gen_binary(CBinary* expr, int target) {
        if (target == TARGET_IS_NOTHING) {
            // Still need to evaluate for side effects
            int before = regallocator.alloced;
            int tmp = regallocator.allocate();
            int err = gen_expression(expr.left, tmp);
            if (err) return err;
            err = gen_expression(expr.right, tmp);
            regallocator.reset_to(before);
            return err;
        }

        int before = regallocator.alloced;
        int lhs = target;
        int err = gen_expression(expr.left, lhs);
        if (err) return err;

        // Check for pointer/array arithmetic scaling
        CType* left_type = get_expr_type(expr.left);
        CType* right_type = get_expr_type(expr.right);
        bool left_is_ptr = left_type && (left_type.is_pointer() || left_type.is_array());
        bool right_is_ptr = right_type && (right_type.is_pointer() || right_type.is_array());
        size_t left_elem_size = left_is_ptr ? left_type.element_size() : 0;
        size_t right_elem_size = right_is_ptr ? right_type.element_size() : 0;

        // Check for literal RHS optimization
        CExpr* right = expr.right;
        if (CLiteral* lit = right.as_literal()) {
            str rhs = lit.value.lexeme;

            // Convert char literals to numeric value
            __gshared char[16] char_lit_buf;
            if (lit.value.type == CTokenType.CHAR_LITERAL) {
                import core.stdc.stdio : snprintf;
                int len = snprintf(char_lit_buf.ptr, 16, "%d", parse_char_literal(rhs));
                rhs = cast(str)char_lit_buf[0 .. len];
            }

            switch (expr.op) with (CTokenType) {
                case PLUS:
                    if (left_elem_size > 1) {
                        // Pointer + integer: scale integer by element size at compile time
                        import dlib.parse_numbers : parse_unsigned_human;
                        auto parsed = parse_unsigned_human(rhs);
                        if (!parsed.errored) {
                            ulong scaled = parsed.value * left_elem_size;
                            sb.writef("    add r% r% %\n", target, lhs, scaled);
                            break;
                        }
                    }
                    sb.writef("    add r% r% %\n", target, lhs, rhs);
                    break;
                case MINUS:
                    // Literals can't be pointers, so this is ptr - int
                    if (left_elem_size > 1) {
                        // Pointer - integer: scale integer by element size at compile time
                        import dlib.parse_numbers : parse_unsigned_human;
                        auto parsed = parse_unsigned_human(rhs);
                        if (!parsed.errored) {
                            ulong scaled = parsed.value * left_elem_size;
                            sb.writef("    sub r% r% %\n", target, lhs, scaled);
                            break;
                        }
                    }
                    sb.writef("    sub r% r% %\n", target, lhs, rhs);
                    break;
                case STAR:
                    sb.writef("    mul r% r% %\n", target, lhs, rhs);
                    break;
                case SLASH:
                    sb.writef("    div r% rjunk r% %\n", target, lhs, rhs);
                    break;
                case PERCENT:
                    sb.writef("    div rjunk r% r% %\n", target, lhs, rhs);
                    break;
                case AMP:
                    sb.writef("    and r% r% %\n", target, lhs, rhs);
                    break;
                case PIPE:
                    sb.writef("    or r% r% %\n", target, lhs, rhs);
                    break;
                case CARET:
                    sb.writef("    xor r% r% %\n", target, lhs, rhs);
                    break;
                case LESS_LESS:
                    sb.writef("    shl r% r% %\n", target, lhs, rhs);
                    break;
                case GREATER_GREATER:
                    sb.writef("    shr r% r% %\n", target, lhs, rhs);
                    break;
                case EQUAL_EQUAL:
                    sb.writef("    scmp r% %\n", lhs, rhs);
                    sb.writef("    move r% 0\n", target);
                    sb.writef("    cmov eq r% 1\n", target);
                    break;
                case BANG_EQUAL:
                    sb.writef("    scmp r% %\n", lhs, rhs);
                    sb.writef("    move r% 0\n", target);
                    sb.writef("    cmov ne r% 1\n", target);
                    break;
                case LESS:
                    sb.writef("    scmp r% %\n", lhs, rhs);
                    sb.writef("    move r% 0\n", target);
                    sb.writef("    cmov lt r% 1\n", target);
                    break;
                case LESS_EQUAL:
                    sb.writef("    scmp r% %\n", lhs, rhs);
                    sb.writef("    move r% 0\n", target);
                    sb.writef("    cmov le r% 1\n", target);
                    break;
                case GREATER:
                    sb.writef("    scmp r% %\n", lhs, rhs);
                    sb.writef("    move r% 0\n", target);
                    sb.writef("    cmov gt r% 1\n", target);
                    break;
                case GREATER_EQUAL:
                    sb.writef("    scmp r% %\n", lhs, rhs);
                    sb.writef("    move r% 0\n", target);
                    sb.writef("    cmov ge r% 1\n", target);
                    break;
                default:
                    error(expr.expr.token, "Unhandled binary operator");
                    return 1;
            }
            return 0;
        }

        // General case: evaluate RHS to register
        int rhs = regallocator.allocate();
        err = gen_expression(right, rhs);
        if (err) return err;

        switch (expr.op) with (CTokenType) {
            case PLUS:
                if (left_elem_size > 1 && !right_is_ptr) {
                    // Pointer + integer: scale integer by element size
                    sb.writef("    mul r% r% %\n", rhs, rhs, left_elem_size);
                } else if (right_elem_size > 1 && !left_is_ptr) {
                    // Integer + pointer: scale integer by element size
                    sb.writef("    mul r% r% %\n", lhs, lhs, right_elem_size);
                }
                sb.writef("    add r% r% r%\n", target, lhs, rhs);
                break;
            case MINUS:
                if (left_is_ptr && right_is_ptr) {
                    // Pointer - pointer: check element sizes match, subtract, divide by size
                    if (left_elem_size != right_elem_size) {
                        error(expr.expr.token, "Subtraction of pointers to different types");
                        return 1;
                    }
                    sb.writef("    sub r% r% r%\n", target, lhs, rhs);
                    if (left_elem_size > 1) {
                        sb.writef("    div r% rjunk r% %\n", target, target, left_elem_size);
                    }
                } else if (left_is_ptr) {
                    // Pointer - integer: scale integer by element size
                    if (left_elem_size > 1) {
                        sb.writef("    mul r% r% %\n", rhs, rhs, left_elem_size);
                    }
                    sb.writef("    sub r% r% r%\n", target, lhs, rhs);
                } else {
                    // Regular integer subtraction
                    sb.writef("    sub r% r% r%\n", target, lhs, rhs);
                }
                break;
            case STAR:
                sb.writef("    mul r% r% r%\n", target, lhs, rhs);
                break;
            case SLASH:
                sb.writef("    div r% rjunk r% r%\n", target, lhs, rhs);
                break;
            case PERCENT:
                sb.writef("    div rjunk r% r% r%\n", target, lhs, rhs);
                break;
            case AMP:
                sb.writef("    and r% r% r%\n", target, lhs, rhs);
                break;
            case PIPE:
                sb.writef("    or r% r% r%\n", target, lhs, rhs);
                break;
            case CARET:
                sb.writef("    xor r% r% r%\n", target, lhs, rhs);
                break;
            case LESS_LESS:
                sb.writef("    shl r% r% r%\n", target, lhs, rhs);
                break;
            case GREATER_GREATER:
                sb.writef("    shr r% r% r%\n", target, lhs, rhs);
                break;
            case EQUAL_EQUAL:
                sb.writef("    scmp r% r%\n", lhs, rhs);
                sb.writef("    move r% 0\n", target);
                sb.writef("    cmov eq r% 1\n", target);
                break;
            case BANG_EQUAL:
                sb.writef("    scmp r% r%\n", lhs, rhs);
                sb.writef("    move r% 0\n", target);
                sb.writef("    cmov ne r% 1\n", target);
                break;
            case LESS:
                sb.writef("    scmp r% r%\n", lhs, rhs);
                sb.writef("    move r% 0\n", target);
                sb.writef("    cmov lt r% 1\n", target);
                break;
            case LESS_EQUAL:
                sb.writef("    scmp r% r%\n", lhs, rhs);
                sb.writef("    move r% 0\n", target);
                sb.writef("    cmov le r% 1\n", target);
                break;
            case GREATER:
                sb.writef("    scmp r% r%\n", lhs, rhs);
                sb.writef("    move r% 0\n", target);
                sb.writef("    cmov gt r% 1\n", target);
                break;
            case GREATER_EQUAL:
                sb.writef("    scmp r% r%\n", lhs, rhs);
                sb.writef("    move r% 0\n", target);
                sb.writef("    cmov ge r% 1\n", target);
                break;
            case AMP_AMP:
                // Short-circuit AND
                int after = labelallocator.allocate();
                sb.writef("    move r% 0\n", target);
                sb.writef("    cmp r% 0\n", lhs);
                sb.writef("    jump eq label L%\n", after);
                sb.writef("    cmp r% 0\n", rhs);
                sb.writef("    cmov ne r% 1\n", target);
                sb.writef("  label L%\n", after);
                break;
            case PIPE_PIPE:
                // Short-circuit OR
                int after2 = labelallocator.allocate();
                sb.writef("    move r% 1\n", target);
                sb.writef("    cmp r% 0\n", lhs);
                sb.writef("    jump ne label L%\n", after2);
                sb.writef("    cmp r% 0\n", rhs);
                sb.writef("    cmov eq r% 0\n", target);
                sb.writef("  label L%\n", after2);
                break;
            default:
                error(expr.expr.token, "Unhandled binary operator");
                return 1;
        }

        regallocator.reset_to(before);
        return 0;
    }

    int gen_unary(CUnary* expr, int target) {
        if (expr.op == CTokenType.AMP) {
            // Address-of operator
            if (CIdentifier* id = expr.operand.as_identifier()) {
                str name = id.name.lexeme;
                if (target != TARGET_IS_NOTHING) {
                    if (int* offset = name in stacklocals) {
                        // Compute stack address: rbp + offset * wordsize
                        // Stack grows up, so locals are at positive offsets from rbp
                        sb.writef("    add r% rbp %\n", target, P(*offset));
                        return 0;
                    }
                    if (name in reglocals) {
                        error(expr.expr.token, "Cannot take address of register variable");
                        return 1;
                    }
                    // Global variable
                    sb.writef("    move r% var %\n", target, name);
                }
                return 0;
            }
            error(expr.expr.token, "Invalid operand for address-of");
            return 1;
        }

        if (expr.op == CTokenType.STAR) {
            // Dereference operator
            int before = regallocator.alloced;
            int ptr_reg = target == TARGET_IS_NOTHING ? regallocator.allocate() : target;
            int err = gen_expression(expr.operand, ptr_reg);
            if (err) return err;
            if (target != TARGET_IS_NOTHING) {
                // Get the pointed-to type's size for proper sized read
                CType* ptr_type = get_expr_type(expr.operand);
                size_t elem_size = (ptr_type && ptr_type.is_pointer()) ? ptr_type.element_size() : 8;
                sb.writef("    % r% r%\n", read_instr_for_size(elem_size), target, ptr_reg);
            }
            regallocator.reset_to(before);
            return 0;
        }

        // Other unary operators
        int before = regallocator.alloced;
        int operand_reg = target == TARGET_IS_NOTHING ? regallocator.allocate() : target;
        int err = gen_expression(expr.operand, operand_reg);
        if (err) return err;

        if (target != TARGET_IS_NOTHING) {
            switch (expr.op) with (CTokenType) {
                case MINUS:
                    sb.writef("    neg r% r%\n", target, operand_reg);
                    break;
                case BANG:
                    sb.writef("    not r% r%\n", target, operand_reg);
                    break;
                case TILDE:
                    // Bitwise NOT - XOR with -1
                    sb.writef("    xor r% r% -1\n", target, operand_reg);
                    break;
                case PLUS_PLUS:
                    if (expr.is_prefix) {
                        // ++x: increment then return
                        if (CIdentifier* id = expr.operand.as_identifier()) {
                            if (int* r = id.name.lexeme in reglocals) {
                                sb.writef("    add r% r% 1\n", *r, *r);
                                if (target != *r)
                                    sb.writef("    move r% r%\n", target, *r);
                            }
                        }
                    } else {
                        // x++: return then increment
                        if (CIdentifier* id = expr.operand.as_identifier()) {
                            if (int* r = id.name.lexeme in reglocals) {
                                if (target != *r)
                                    sb.writef("    move r% r%\n", target, *r);
                                sb.writef("    add r% r% 1\n", *r, *r);
                            }
                        }
                    }
                    break;
                case MINUS_MINUS:
                    if (expr.is_prefix) {
                        if (CIdentifier* id = expr.operand.as_identifier()) {
                            if (int* r = id.name.lexeme in reglocals) {
                                sb.writef("    sub r% r% 1\n", *r, *r);
                                if (target != *r)
                                    sb.writef("    move r% r%\n", target, *r);
                            }
                        }
                    } else {
                        if (CIdentifier* id = expr.operand.as_identifier()) {
                            if (int* r = id.name.lexeme in reglocals) {
                                if (target != *r)
                                    sb.writef("    move r% r%\n", target, *r);
                                sb.writef("    sub r% r% 1\n", *r, *r);
                            }
                        }
                    }
                    break;
                default:
                    error(expr.expr.token, "Unhandled unary operator");
                    return 1;
            }
        }

        regallocator.reset_to(before);
        return 0;
    }

    int gen_call(CCall* expr, int target) {
        int before = regallocator.alloced;

        // Evaluate arguments and push to stack (for preserving across call)
        foreach (i, arg; expr.args) {
            int err = gen_expression(arg, RARG1 + cast(int)i);
            if (err) return err;
            if (i != expr.args.length - 1) {
                sb.writef("    push rarg%\n", 1 + i);
            }
        }

        // Pop arguments back
        for (int i = 1; i < expr.args.length; i++) {
            sb.writef("    pop rarg%\n", 1 + cast(int)expr.args.length - 1 - i);
        }

        // Generate call
        if (CIdentifier* id = expr.callee.as_identifier()) {
            // Save registers before call
            for (int i = 0; i < before; i++) {
                sb.writef("    push r%\n", i);
            }

            // Direct call - use qualified name for extern functions
            if (str* mod_alias = id.name.lexeme in extern_funcs) {
                sb.writef("    call function %.% %\n", *mod_alias, id.name.lexeme, expr.args.length);
            } else {
                sb.writef("    call function % %\n", id.name.lexeme, expr.args.length);
            }

            // Restore registers
            for (int i = before - 1; i >= 0; i--) {
                sb.writef("    pop r%\n", i);
            }
        } else {
            // Indirect call through register
            int func_reg = regallocator.allocate();
            int err = gen_expression(expr.callee, func_reg);
            if (err) return err;

            for (int i = 0; i < before; i++) {
                sb.writef("    push r%\n", i);
            }

            sb.writef("    call r% %\n", func_reg, expr.args.length);

            for (int i = before - 1; i >= 0; i--) {
                sb.writef("    pop r%\n", i);
            }
        }

        if (target != TARGET_IS_NOTHING) {
            sb.writef("    move r% rout1\n", target);
        }

        regallocator.reset_to(before);
        return 0;
    }

    int gen_assign(CAssign* expr, int target) {
        CExpr* lhs = expr.target.ungroup();

        // Simple variable assignment
        if (CIdentifier* id = lhs.as_identifier()) {
            str name = id.name.lexeme;

            // Check for register variable
            if (int* r = name in reglocals) {
                if (expr.op == CTokenType.EQUAL) {
                    // Simple assignment
                    int err = gen_expression(expr.value, *r);
                    if (err) return err;
                } else {
                    // Compound assignment (+=, -=, etc.)
                    int before = regallocator.alloced;
                    int rhs_reg = regallocator.allocate();
                    int err = gen_expression(expr.value, rhs_reg);
                    if (err) return err;

                    switch (expr.op) with (CTokenType) {
                        case PLUS_EQUAL:
                            sb.writef("    add r% r% r%\n", *r, *r, rhs_reg);
                            break;
                        case MINUS_EQUAL:
                            sb.writef("    sub r% r% r%\n", *r, *r, rhs_reg);
                            break;
                        case STAR_EQUAL:
                            sb.writef("    mul r% r% r%\n", *r, *r, rhs_reg);
                            break;
                        case SLASH_EQUAL:
                            sb.writef("    div r% rjunk r% r%\n", *r, *r, rhs_reg);
                            break;
                        default:
                            error(expr.expr.token, "Unhandled compound assignment");
                            return 1;
                    }
                    regallocator.reset_to(before);
                }

                if (target != TARGET_IS_NOTHING && target != *r) {
                    sb.writef("    move r% r%\n", target, *r);
                }
                return 0;
            }

            // Check for stack variable
            if (int* offset = name in stacklocals) {
                int before = regallocator.alloced;
                int val_reg = regallocator.allocate();

                if (expr.op == CTokenType.EQUAL) {
                    // Simple assignment
                    int err = gen_expression(expr.value, val_reg);
                    if (err) return err;
                } else {
                    // Compound assignment - read current value first
                    int cur_reg = regallocator.allocate();
                    sb.writef("    local_read r% %\n", cur_reg, P(*offset));

                    int err = gen_expression(expr.value, val_reg);
                    if (err) return err;

                    switch (expr.op) with (CTokenType) {
                        case PLUS_EQUAL:
                            sb.writef("    add r% r% r%\n", val_reg, cur_reg, val_reg);
                            break;
                        case MINUS_EQUAL:
                            sb.writef("    sub r% r% r%\n", val_reg, cur_reg, val_reg);
                            break;
                        case STAR_EQUAL:
                            sb.writef("    mul r% r% r%\n", val_reg, cur_reg, val_reg);
                            break;
                        case SLASH_EQUAL:
                            sb.writef("    div r% rjunk r% r%\n", val_reg, cur_reg, val_reg);
                            break;
                        default:
                            error(expr.expr.token, "Unhandled compound assignment");
                            return 1;
                    }
                }

                sb.writef("    local_write % r%\n", P(*offset), val_reg);

                if (target != TARGET_IS_NOTHING) {
                    sb.writef("    move r% r%\n", target, val_reg);
                }
                regallocator.reset_to(before);
                return 0;
            }

            // Check for global variable
            if (CType** gtype = name in global_types) {
                int before = regallocator.alloced;
                int addr_reg = regallocator.allocate();
                int val_reg = regallocator.allocate();

                // Get address of global and its size
                sb.writef("    move r% var %\n", addr_reg, name);
                size_t var_size = (*gtype) ? (*gtype).size_of() : 8;

                if (expr.op == CTokenType.EQUAL) {
                    // Simple assignment
                    int err = gen_expression(expr.value, val_reg);
                    if (err) return err;
                } else {
                    // Compound assignment - read current value first
                    int cur_reg = regallocator.allocate();
                    sb.writef("    % r% r%\n", read_instr_for_size(var_size), cur_reg, addr_reg);

                    int err = gen_expression(expr.value, val_reg);
                    if (err) return err;

                    switch (expr.op) with (CTokenType) {
                        case PLUS_EQUAL:
                            sb.writef("    add r% r% r%\n", val_reg, cur_reg, val_reg);
                            break;
                        case MINUS_EQUAL:
                            sb.writef("    sub r% r% r%\n", val_reg, cur_reg, val_reg);
                            break;
                        case STAR_EQUAL:
                            sb.writef("    mul r% r% r%\n", val_reg, cur_reg, val_reg);
                            break;
                        case SLASH_EQUAL:
                            sb.writef("    div r% rjunk r% r%\n", val_reg, cur_reg, val_reg);
                            break;
                        default:
                            error(expr.expr.token, "Unhandled compound assignment");
                            return 1;
                    }
                }

                sb.writef("    % r% r%\n", write_instr_for_size(var_size), addr_reg, val_reg);

                if (target != TARGET_IS_NOTHING) {
                    sb.writef("    move r% r%\n", target, val_reg);
                }
                regallocator.reset_to(before);
                return 0;
            }
        }

        // Pointer dereference assignment: *ptr = value
        if (CUnary* deref = lhs.as_unary()) {
            if (deref.op == CTokenType.STAR) {
                int before = regallocator.alloced;
                int ptr_reg = regallocator.allocate();
                int val_reg = regallocator.allocate();

                int err = gen_expression(deref.operand, ptr_reg);
                if (err) return err;

                err = gen_expression(expr.value, val_reg);
                if (err) return err;

                // Get pointed-to type's size for proper sized write
                CType* ptr_type = get_expr_type(deref.operand);
                size_t elem_size = (ptr_type && ptr_type.is_pointer()) ? ptr_type.element_size() : 8;
                sb.writef("    % r% r%\n", write_instr_for_size(elem_size), ptr_reg, val_reg);

                if (target != TARGET_IS_NOTHING) {
                    sb.writef("    move r% r%\n", target, val_reg);
                }

                regallocator.reset_to(before);
                return 0;
            }
        }

        // Subscript assignment: arr[i] = value (equivalent to *(arr + i) = value)
        if (CSubscript* sub = lhs.as_subscript()) {
            int before = regallocator.alloced;
            int ptr_reg = regallocator.allocate();
            int idx_reg = regallocator.allocate();
            int val_reg = regallocator.allocate();

            int err = gen_expression(sub.array, ptr_reg);
            if (err) return err;

            err = gen_expression(sub.index, idx_reg);
            if (err) return err;

            // Scale index by element size
            CType* arr_type = get_expr_type(sub.array);
            size_t elem_size = (arr_type && (arr_type.is_pointer() || arr_type.is_array())) ? arr_type.element_size() : 1;
            if (elem_size > 1) {
                sb.writef("    mul r% r% %\n", idx_reg, idx_reg, elem_size);
            }
            sb.writef("    add r% r% r%\n", ptr_reg, ptr_reg, idx_reg);

            err = gen_expression(expr.value, val_reg);
            if (err) return err;

            sb.writef("    % r% r%\n", write_instr_for_size(elem_size), ptr_reg, val_reg);

            if (target != TARGET_IS_NOTHING) {
                sb.writef("    move r% r%\n", target, val_reg);
            }

            regallocator.reset_to(before);
            return 0;
        }

        error(expr.expr.token, "Invalid assignment target");
        return 1;
    }

    int gen_subscript(CSubscript* expr, int target) {
        // array[index] is equivalent to *(array + index)
        int before = regallocator.alloced;
        int arr_reg = regallocator.allocate();
        int idx_reg = regallocator.allocate();

        int err = gen_expression(expr.array, arr_reg);
        if (err) return err;

        err = gen_expression(expr.index, idx_reg);
        if (err) return err;

        // Scale index by element size for proper pointer/array arithmetic
        CType* arr_type = get_expr_type(expr.array);
        size_t elem_size = (arr_type && (arr_type.is_pointer() || arr_type.is_array())) ? arr_type.element_size() : 1;
        if (elem_size > 1) {
            sb.writef("    mul r% r% %\n", idx_reg, idx_reg, elem_size);
        }
        sb.writef("    add r% r% r%\n", arr_reg, arr_reg, idx_reg);

        if (target != TARGET_IS_NOTHING) {
            sb.writef("    % r% r%\n", read_instr_for_size(elem_size), target, arr_reg);
        }

        regallocator.reset_to(before);
        return 0;
    }
}
