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
    Table!(str, bool) structs;     // Struct variables (always need stack)
    Barray!(str) all_vars;         // All local variable names
    Allocator allocator;

    void analyze_function(CFunction* func) {
        addr_taken.data.allocator = allocator;
        arrays.data.allocator = allocator;
        structs.data.allocator = allocator;
        all_vars.bdata.allocator = allocator;
        addr_taken.cleanup();
        arrays.cleanup();
        structs.cleanup();
        all_vars.clear();

        foreach (stmt; func.body) {
            analyze_stmt(stmt);
        }
    }

    bool should_use_stack() {
        // Use stack if any address is taken, any arrays, structs, or more than 4 variables
        return addr_taken.count > 0 || arrays.count > 0 || structs.count > 0 || all_vars[].length > 4;
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
                // Track struct/union variables
                if (decl.var_type && decl.var_type.is_struct_or_union()) {
                    structs[decl.name.lexeme] = true;
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
    Table!(str, CType*) func_return_types; // Function return types (for struct returns)
    Table!(str, long) enum_constants; // Enum constant values (name -> value)

    // Loop control
    int current_continue_target = -1;
    int current_break_target = -1;

    // Current function info
    CType* current_return_type = null;  // Return type of current function
    bool returns_struct = false;         // True if current function returns a struct
    bool uses_hidden_return_ptr = false; // True if struct is too big for registers (> 16 bytes)
    int return_ptr_slot = -1;            // Stack slot for hidden return pointer (large struct returns)

    bool ERROR_OCCURRED = false;
    bool use_stack = false;  // Set true when we need stack-based locals
    int funcdepth = 0;
    int stack_offset = 1;    // Current stack offset for locals (start at 1, slot 0 is saved RBP)

    enum { TARGET_IS_NOTHING = -1 }
    enum { RARG1 = 10 }  // First argument register (rarg1 = r10)
    // Note: Return value uses named register 'rout1', not a numeric register

    // Check if a struct/union type can be returned in registers (1-2 registers for <= 16 bytes)
    static bool struct_fits_in_registers(CType* t) {
        if (t is null || !t.is_struct_or_union()) return false;
        return t.size_of() <= 16;
    }

    // Get number of registers needed to return a struct/union (0, 1, or 2)
    static int struct_return_regs(CType* t) {
        if (t is null || !t.is_struct_or_union()) return 0;
        size_t size = t.size_of();
        if (size == 0) return 0;
        if (size <= 8) return 1;
        if (size <= 16) return 2;
        return 0;  // Too big, use hidden pointer
    }

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
                // Look up function return type
                auto call = cast(CCall*)e;
                if (CIdentifier* id = call.callee.as_identifier()) {
                    if (CType** rt = id.name.lexeme in func_return_types) {
                        return *rt;
                    }
                }
                return null;
            case ASSIGN:
                return get_expr_type((cast(CAssign*)e).target);
            case SUBSCRIPT:
                auto sub = e.as_subscript;
                auto at = get_expr_type(sub.array);
                if (at && (at.is_pointer() || at.is_array())) return at.pointed_to;
                return null;
            case MEMBER_ACCESS:
                auto ma = e.as_member_access;
                auto obj_type = get_expr_type(ma.object);
                if (obj_type is null) return null;
                // For ->, dereference pointer first
                if (ma.is_arrow && obj_type.is_pointer()) {
                    obj_type = obj_type.pointed_to;
                }
                if (obj_type && obj_type.is_struct_or_union()) {
                    auto field = obj_type.get_field(ma.member.lexeme);
                    if (field) return field.type;
                }
                return null;
            case CAST:
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

        // First pass: collect function return types for struct return handling
        func_return_types.data.allocator = allocator;
        foreach (ref func; unit.functions) {
            func_return_types[func.name.lexeme] = func.return_type;
        }

        // Load enum constants
        enum_constants.data.allocator = allocator;
        foreach (ref edef; unit.enums) {
            foreach (ref ec; edef.constants) {
                enum_constants[ec.name] = ec.value;
            }
        }

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
            current_return_type = null;
            returns_struct = false;
            uses_hidden_return_ptr = false;
            return_ptr_slot = -1;
        }

        // Track return type for struct/union returns
        current_return_type = func.return_type;
        returns_struct = func.return_type !is null && func.return_type.is_struct_or_union();
        // Only use hidden pointer for structs/unions > 16 bytes (can't fit in 2 registers)
        uses_hidden_return_ptr = returns_struct && !struct_fits_in_registers(func.return_type);

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

        // Also force stack if any parameters are structs/unions or we use hidden return pointer
        if (uses_hidden_return_ptr) use_stack = true;
        foreach (ref param; func.params) {
            if (param.type.is_struct_or_union()) {
                use_stack = true;
                break;
            }
        }

        // First, count how many stack slots we need
        int num_stack_slots = 0;
        if (use_stack) {
            // Reserve slot for hidden return pointer if returning large struct
            if (uses_hidden_return_ptr) {
                num_stack_slots += 1;
            }
            // Count parameters (struct params need multiple slots)
            foreach (ref param; func.params) {
                num_stack_slots += cast(int)param.type.stack_slots();
            }
            // Count local variables, accounting for array sizes
            num_stack_slots += count_var_slots(func.body);
        }

        // For large struct returns, caller passes hidden pointer as first arg
        int arg_offset = uses_hidden_return_ptr ? 1 : 0;

        // Calculate total register slots needed for parameters
        int total_param_slots = arg_offset;
        foreach (ref param; func.params) {
            if (param.type.is_struct_or_union() && struct_fits_in_registers(param.type)) {
                total_param_slots += struct_return_regs(param.type);
            } else {
                total_param_slots += 1;
            }
        }

        // Emit function header with correct number of register slots
        sb.writef("function % %\n", func.name.lexeme, total_param_slots);

        // Set up stack frame if we have address-taken variables
        if (use_stack) {
            sb.write("    push rbp\n");
            sb.write("    move rbp rsp\n");
            // Allocate all stack slots upfront (slots 1..n, so need n+1 slots total for push safety)
            sb.writef("    add rsp rsp %\n", P(num_stack_slots + 1));
        }

        // Save hidden return pointer if returning large struct
        if (uses_hidden_return_ptr && use_stack) {
            return_ptr_slot = stack_offset++;
            sb.writef("    local_write % rarg1\n", P(return_ptr_slot));
        }

        // Calculate register slots for parameters (structs may use 1-2 slots)
        int current_reg_slot = arg_offset;

        // Move arguments from rarg registers to locals
        foreach (i, ref param; func.params) {
            str pname = param.name.lexeme;
            var_types[pname] = param.type;

            int reg_slot = current_reg_slot;
            int regs_used = 1;
            if (param.type.is_struct_or_union() && struct_fits_in_registers(param.type)) {
                regs_used = struct_return_regs(param.type);
            }
            current_reg_slot += regs_used;

            if (use_stack) {
                int slot = stack_offset;
                int num_slots = cast(int)param.type.stack_slots();
                stack_offset += num_slots;
                stacklocals[pname] = slot;

                if (param.type.is_struct_or_union()) {
                    if (struct_fits_in_registers(param.type)) {
                        // Small struct: received in 1-2 registers, store to stack
                        sb.writef("    add r0 rbp %\n", P(slot));
                        sb.writef("    write r0 rarg%\n", 1 + reg_slot);
                        if (regs_used > 1) {
                            sb.write("    add r0 r0 8\n");
                            sb.writef("    write r0 rarg%\n", 2 + reg_slot);
                        }
                    } else {
                        // Large struct: caller passed a pointer, we copy to our stack
                        sb.writef("    add r0 rbp %\n", P(slot));
                        sb.writef("    memcpy r0 rarg% %\n", 1 + reg_slot, param.type.size_of());
                    }
                } else {
                    // Non-struct: just store the value
                    sb.writef("    local_write % rarg%\n", P(slot), 1 + reg_slot);
                }
            } else {
                int r = regallocator.allocate();
                reglocals[pname] = r;
                sb.writef("    move r% rarg%\n", r, 1 + reg_slot);
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

            if (uses_hidden_return_ptr) {
                // Large struct return (> 16 bytes): copy to hidden return pointer
                int src_reg = regallocator.allocate();
                int err = gen_struct_address(stmt.value, src_reg);
                if (err) return err;

                int dst_reg = regallocator.allocate();
                sb.writef("    local_read r% %\n", dst_reg, P(return_ptr_slot));

                size_t struct_size = current_return_type.size_of();
                sb.writef("    memcpy r% r% %\n", dst_reg, src_reg, struct_size);

                // Return the pointer in rout1
                sb.writef("    move rout1 r%\n", dst_reg);
            } else if (returns_struct) {
                // Small struct return (<= 16 bytes): return in registers
                int src_reg = regallocator.allocate();
                int err = gen_struct_address(stmt.value, src_reg);
                if (err) return err;

                size_t struct_size = current_return_type.size_of();
                int num_regs = struct_return_regs(current_return_type);

                // Load struct data into return registers
                if (struct_size <= 8) {
                    // Read 8 bytes into rout1
                    sb.writef("    read rout1 r%\n", src_reg);
                } else {
                    // 9-16 bytes: read 8 bytes into rout1, next 8 into rout2
                    sb.writef("    read rout1 r%\n", src_reg);
                    sb.writef("    add r% r% 8\n", src_reg, src_reg);
                    sb.writef("    read rout2 r%\n", src_reg);
                }
            } else {
                // Non-struct return: just move value to rout1
                int temp = regallocator.allocate();
                int err = gen_expression(stmt.value, temp);
                if (err) return err;
                sb.writef("    move rout1 r%\n", temp);
            }
            regallocator.reset_to(before);
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

        // Check if this is an array or struct/union
        bool is_array = stmt.var_type.is_array();
        bool is_struct = stmt.var_type.is_struct_or_union();

        if (use_stack) {
            // All variables go on stack when use_stack is true
            int slot = stack_offset;

            if (is_struct) {
                // Structs/unions need multiple slots based on size
                size_t num_slots = stmt.var_type.stack_slots();
                stack_offset += cast(int)num_slots;

                // Zero-initialize struct
                int before = regallocator.alloced;
                int addr_reg = regallocator.allocate();
                sb.writef("    add r% rbp %\n", addr_reg, P(slot));
                sb.writef("    memzero r% %\n", addr_reg, stmt.var_type.size_of());
                regallocator.reset_to(before);

                // TODO: Handle struct initializers if needed
            } else if (is_array) {
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
            case MEMBER_ACCESS: return gen_member_access(cast(CMemberAccess*)e, target);
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

        // Check for enum constant
        if (long* val = name in enum_constants) {
            sb.writef("    move r% %\n", target, *val);
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

        // Check if callee returns a struct
        bool callee_returns_struct = false;
        bool callee_uses_hidden_ptr = false;
        CType* callee_return_type = null;
        if (CIdentifier* id = expr.callee.as_identifier()) {
            if (CType** rt = id.name.lexeme in func_return_types) {
                callee_return_type = *rt;
                callee_returns_struct = callee_return_type !is null && callee_return_type.is_struct_or_union();
                callee_uses_hidden_ptr = callee_returns_struct && !struct_fits_in_registers(callee_return_type);
            }
        }

        // For large struct/union returns, arg registers shift by 1 (rarg1 = hidden return ptr)
        int arg_offset = callee_uses_hidden_ptr ? 1 : 0;

        // Helper to get number of register slots for an argument
        int arg_slots(CExpr* arg) {
            CType* t = get_expr_type(arg);
            if (t && t.is_struct_or_union() && struct_fits_in_registers(t)) {
                return struct_return_regs(t);
            }
            return 1;
        }

        // Calculate starting slot for argument i
        int get_arg_slot(size_t idx) {
            int slot = arg_offset;
            foreach (j, arg; expr.args) {
                if (j == idx) return slot;
                slot += arg_slots(arg);
            }
            return slot;
        }

        // Calculate total register slots
        int total_reg_slots = arg_offset;
        foreach (arg; expr.args) {
            total_reg_slots += arg_slots(arg);
        }

        // Evaluate arguments
        foreach (i, arg; expr.args) {
            CType* arg_type = get_expr_type(arg);
            int slot = get_arg_slot(i);
            int num_slots = arg_slots(arg);

            if (arg_type && arg_type.is_struct_or_union()) {
                if (struct_fits_in_registers(arg_type)) {
                    // Small struct: load data into register(s)
                    int addr_reg = regallocator.allocate();
                    int err = gen_struct_address(arg, addr_reg);
                    if (err) return err;

                    size_t struct_size = arg_type.size_of();
                    sb.writef("    read rarg% r%\n", 1 + slot, addr_reg);
                    if (struct_size > 8) {
                        sb.writef("    add r% r% 8\n", addr_reg, addr_reg);
                        sb.writef("    read rarg% r%\n", 2 + slot, addr_reg);
                    }
                    regallocator.reset_to(regallocator.alloced - 1);
                } else {
                    // Large struct: pass address (callee copies)
                    int err = gen_struct_address(arg, RARG1 + slot);
                    if (err) return err;
                }
            } else {
                int err = gen_expression(arg, RARG1 + slot);
                if (err) return err;
            }

            // Push to preserve across subsequent arg evaluation
            if (i != expr.args.length - 1) {
                for (int s = 0; s < num_slots; s++) {
                    sb.writef("    push rarg%\n", 1 + slot + s);
                }
            }
        }

        // Pop arguments back in reverse order
        for (int i = cast(int)expr.args.length - 2; i >= 0; i--) {
            int slot = get_arg_slot(i);
            int num_slots = arg_slots(expr.args[i]);
            for (int s = num_slots - 1; s >= 0; s--) {
                sb.writef("    pop rarg%\n", 1 + slot + s);
            }
        }

        // For large struct returns, pass destination address in rarg1
        // We use rsp as the temp location (it points past the allocated frame)
        int struct_slots_needed = 0;
        if (callee_uses_hidden_ptr) {
            struct_slots_needed = cast(int)callee_return_type.stack_slots();
            // Allocate temp space by advancing rsp
            sb.writef("    add rsp rsp %\n", P(struct_slots_needed));
            // Pass address of temp space as rarg1 (rsp - slots = start of temp area)
            sb.writef("    sub rarg1 rsp %\n", P(struct_slots_needed));
        }

        int total_args = total_reg_slots;  // Accounts for multi-register struct params

        // Generate call
        if (CIdentifier* id = expr.callee.as_identifier()) {
            // Save registers before call
            for (int i = 0; i < before; i++) {
                sb.writef("    push r%\n", i);
            }

            // Direct call - use qualified name for extern functions
            if (str* mod_alias = id.name.lexeme in extern_funcs) {
                sb.writef("    call function %.% %\n", *mod_alias, id.name.lexeme, total_args);
            } else {
                sb.writef("    call function % %\n", id.name.lexeme, total_args);
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

            sb.writef("    call r% %\n", func_reg, total_args);

            for (int i = before - 1; i >= 0; i--) {
                sb.writef("    pop r%\n", i);
            }
        }

        if (target != TARGET_IS_NOTHING) {
            if (callee_uses_hidden_ptr) {
                // Large struct returns: temp space already allocated, address is at rsp - slots
                sb.writef("    sub r% rsp %\n", target, P(struct_slots_needed));
            } else if (callee_returns_struct) {
                // Small struct returns: allocate temp space and store rout1/rout2 there
                size_t struct_size = callee_return_type.size_of();
                int slots = cast(int)callee_return_type.stack_slots();

                // Allocate temp space
                sb.writef("    add rsp rsp %\n", P(slots));
                // Get address of temp space
                sb.writef("    sub r% rsp %\n", target, P(slots));

                // Store rout1 (first 8 bytes)
                sb.writef("    write r% rout1\n", target);

                // Store rout2 if needed (9-16 bytes)
                if (struct_size > 8) {
                    sb.writef("    add r% r% 8\n", target, target);
                    sb.writef("    write r% rout2\n", target);
                    // Reset target to start of struct
                    sb.writef("    sub r% r% 8\n", target, target);
                }
            } else {
                sb.writef("    move r% rout1\n", target);
            }
        }

        // Note: We don't free the temp struct return space here because:
        // 1. The caller may still need to read from it (e.g., for nested calls)
        // 2. It will be cleaned up when the function returns (rsp = rbp)

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
                // Check if this is a struct/union assignment
                CType** var_type_ptr = name in var_types;
                if (var_type_ptr && (*var_type_ptr).is_struct_or_union()) {
                    // Struct/union assignment - use memcpy
                    if (expr.op != CTokenType.EQUAL) {
                        error(expr.expr.token, "Compound assignment not supported for structs");
                        return 1;
                    }

                    int before = regallocator.alloced;
                    int dst_reg = regallocator.allocate();
                    int src_reg = regallocator.allocate();

                    // dst = address of target struct
                    sb.writef("    add r% rbp %\n", dst_reg, P(*offset));

                    // src = address of source struct
                    CExpr* val = expr.value.ungroup();
                    if (CIdentifier* src_id = val.as_identifier()) {
                        // Source is a variable
                        str src_name = src_id.name.lexeme;
                        if (int* src_offset = src_name in stacklocals) {
                            sb.writef("    add r% rbp %\n", src_reg, P(*src_offset));
                        } else {
                            error(expr.expr.token, "Source struct must be a local variable");
                            return 1;
                        }
                    } else if (val.kind == CExprKind.CALL) {
                        // Source is a function call that returns a struct
                        // Generate the call - it will return pointer to struct in src_reg
                        int err = gen_expression(val, src_reg);
                        if (err) return err;
                        // src_reg now contains pointer to the returned struct
                    } else {
                        error(expr.expr.token, "Struct assignment source must be a variable or function call");
                        return 1;
                    }

                    // memcpy dst src size
                    size_t struct_size = (*var_type_ptr).size_of();
                    sb.writef("    memcpy r% r% %\n", dst_reg, src_reg, struct_size);

                    regallocator.reset_to(before);
                    return 0;
                }

                // Regular (non-struct) assignment
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

        // Member access assignment: p.x = value or p->x = value
        if (CMemberAccess* ma = lhs.as_member_access()) {
            int before = regallocator.alloced;
            int addr_reg = regallocator.allocate();
            int val_reg = regallocator.allocate();

            // Get the object type
            CType* obj_type = get_expr_type(ma.object);
            if (obj_type is null) {
                error(expr.expr.token, "Cannot determine type of struct expression");
                return 1;
            }

            // For ->, obj_type is a pointer to struct
            CType* struct_type = obj_type;
            if (ma.is_arrow) {
                if (!obj_type.is_pointer()) {
                    error(expr.expr.token, "'->' requires pointer to struct");
                    return 1;
                }
                struct_type = obj_type.pointed_to;
            }

            if (struct_type is null || !struct_type.is_struct_or_union()) {
                error(expr.expr.token, "Member access requires struct/union type");
                return 1;
            }

            // Find the field
            StructField* field = struct_type.get_field(ma.member.lexeme);
            if (field is null) {
                error(expr.expr.token, "Unknown struct/union field");
                return 1;
            }

            // Get address of struct/object
            if (ma.is_arrow) {
                int err = gen_expression(ma.object, addr_reg);
                if (err) return err;
            } else {
                // For ., we need the address of the struct
                if (CIdentifier* id = ma.object.as_identifier()) {
                    str fname = id.name.lexeme;
                    if (int* offset = fname in stacklocals) {
                        sb.writef("    add r% rbp %\n", addr_reg, P(*offset));
                    } else {
                        error(expr.expr.token, "Cannot take address for '.' assignment");
                        return 1;
                    }
                } else {
                    error(expr.expr.token, "'.' assignment on complex expressions not supported");
                    return 1;
                }
            }

            // Add field offset
            if (field.offset > 0) {
                sb.writef("    add r% r% %\n", addr_reg, addr_reg, field.offset);
            }

            // Generate value
            int err = gen_expression(expr.value, val_reg);
            if (err) return err;

            // Write to field
            size_t field_size = field.type.size_of();
            sb.writef("    % r% r%\n", write_instr_for_size(field_size), addr_reg, val_reg);

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

    int gen_member_access(CMemberAccess* expr, int target) {
        int before = regallocator.alloced;
        int obj_reg = regallocator.allocate();

        // Get the object type
        CType* obj_type = get_expr_type(expr.object);
        if (obj_type is null) {
            error(expr.expr.token, "Cannot determine type of struct expression");
            return 1;
        }

        // For ->, obj_type is a pointer to struct
        // For ., obj_type is the struct itself
        CType* struct_type = obj_type;
        if (expr.is_arrow) {
            if (!obj_type.is_pointer()) {
                error(expr.expr.token, "'->' requires pointer to struct");
                return 1;
            }
            struct_type = obj_type.pointed_to;
        }

        if (struct_type is null || !struct_type.is_struct_or_union()) {
            error(expr.expr.token, "Member access requires struct/union type");
            return 1;
        }

        // Find the field
        StructField* field = struct_type.get_field(expr.member.lexeme);
        if (field is null) {
            error(expr.expr.token, "Unknown struct/union field");
            return 1;
        }

        // Generate code to get address of object
        if (expr.is_arrow) {
            // For ->, the expression gives us the pointer directly
            int err = gen_expression(expr.object, obj_reg);
            if (err) return err;
        } else {
            // For ., we need the address of the struct
            // If it's an identifier, get its address
            if (CIdentifier* id = expr.object.as_identifier()) {
                str name = id.name.lexeme;
                if (int* offset = name in stacklocals) {
                    sb.writef("    add r% rbp %\n", obj_reg, P(*offset));
                } else {
                    error(expr.expr.token, "Cannot take address of expression for '.' access");
                    return 1;
                }
            } else {
                error(expr.expr.token, "'.' access on complex expressions not supported");
                return 1;
            }
        }

        // Add field offset
        if (field.offset > 0) {
            sb.writef("    add r% r% %\n", obj_reg, obj_reg, field.offset);
        }

        // For array and struct/union fields, return the address rather than reading
        // (they decay to pointers in most contexts)
        if (target != TARGET_IS_NOTHING) {
            if (field.type.is_array() || field.type.is_struct_or_union()) {
                // Return address of the array/struct field
                if (target != obj_reg) {
                    sb.writef("    move r% r%\n", target, obj_reg);
                }
            } else {
                // Read scalar field value
                size_t field_size = field.type.size_of();
                sb.writef("    % r% r%\n", read_instr_for_size(field_size), target, obj_reg);
            }
        }

        regallocator.reset_to(before);
        return 0;
    }

    // Generate code to get the address of a struct expression (for pass-by-value)
    int gen_struct_address(CExpr* e, int target) {
        e = e.ungroup();

        // Identifier - get address of local struct variable
        if (CIdentifier* id = e.as_identifier()) {
            str name = id.name.lexeme;
            if (int* offset = name in stacklocals) {
                sb.writef("    add r% rbp %\n", target, P(*offset));
                return 0;
            }
            error(id.name, "Cannot take address of non-local variable for struct pass-by-value");
            return 1;
        }

        // Member access - get address of struct member
        if (CMemberAccess* ma = e.as_member_access()) {
            CType* obj_type = get_expr_type(ma.object);
            if (obj_type is null) {
                error(ma.expr.token, "Cannot determine type for member access");
                return 1;
            }

            CType* struct_type = obj_type;
            if (ma.is_arrow) {
                if (!obj_type.is_pointer()) {
                    error(ma.expr.token, "'->' requires pointer to struct");
                    return 1;
                }
                struct_type = obj_type.pointed_to;
            }

            StructField* field = struct_type.get_field(ma.member.lexeme);
            if (field is null) {
                error(ma.expr.token, "Unknown struct field");
                return 1;
            }

            // Get base address
            if (ma.is_arrow) {
                int err = gen_expression(ma.object, target);
                if (err) return err;
            } else {
                if (CIdentifier* id = ma.object.as_identifier()) {
                    str name = id.name.lexeme;
                    if (int* offset = name in stacklocals) {
                        sb.writef("    add r% rbp %\n", target, P(*offset));
                    } else {
                        error(ma.expr.token, "Cannot take address of non-local struct");
                        return 1;
                    }
                } else {
                    error(ma.expr.token, "Cannot take address of complex expression");
                    return 1;
                }
            }

            // Add field offset
            if (field.offset > 0) {
                sb.writef("    add r% r% %\n", target, target, field.offset);
            }
            return 0;
        }

        // Function call returning struct/union - gen_expression returns the address
        if (e.kind == CExprKind.CALL) {
            CType* call_type = get_expr_type(e);
            if (call_type && call_type.is_struct_or_union()) {
                return gen_expression(e, target);
            }
        }

        error(e.token, "Cannot pass this expression as struct/union by value");
        return 1;
    }
}
