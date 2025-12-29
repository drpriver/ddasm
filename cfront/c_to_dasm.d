/*
 * C to DASM Code Generator for ddasm
 * Copyright 2025, David Priver
 */
module cfront.c_to_dasm;

import core.stdc.stdio : fprintf, stderr;
import dlib.aliases;
import dlib.allocator : Allocator;
import dlib.barray : Barray, make_barray;
import dlib.stringbuilder : StringBuilder;
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

struct CDasmWriter {
    Allocator allocator;
    StringBuilder* sb;
    RegisterAllocator regallocator;
    LabelAllocator labelallocator;

    // Variable tracking
    Table!(str, int) reglocals;      // Variables in registers
    Table!(str, int) stacklocals;    // Variables on stack
    Table!(str, CType*) var_types;   // Variable types (for pointer arithmetic)
    Table!(str, bool) extern_funcs;  // Set of extern function names

    // Loop control
    int current_continue_target = -1;
    int current_break_target = -1;

    bool ERROR_OCCURRED = false;
    bool use_stack = false;  // Set true when we need stack-based locals
    int funcdepth = 0;

    enum { TARGET_IS_NOTHING = -1 }
    enum { RARG1 = 10 }  // First argument register (rarg1 = r10)
    // Note: Return value uses named register 'rout1', not a numeric register

    @disable this();

    this(StringBuilder* s, Allocator a) {
        allocator = a;
        sb = s;
        reglocals.data.allocator = a;
        stacklocals.data.allocator = a;
        var_types.data.allocator = a;
        extern_funcs.data.allocator = a;
    }

    void cleanup() {
        reglocals.cleanup();
        stacklocals.cleanup();
        var_types.cleanup();
        extern_funcs.cleanup();
    }

    void error(CToken token, str message) {
        ERROR_OCCURRED = true;
        fprintf(stderr, "[line %d]: Code gen error at '%.*s': %.*s\n",
                token.line,
                cast(int)token.lexeme.length, token.lexeme.ptr,
                cast(int)message.length, message.ptr);
    }

    // =========================================================================
    // Main Entry Point
    // =========================================================================

    int generate(CTranslationUnit* unit) {
        // Generate dlimport blocks for extern declarations
        if (unit.externs.length > 0) {
            // Group externs by library
            str lib = unit.current_library.length ? unit.current_library : "libc.so.6";
            sb.write("dlimport C\n");
            sb.writef("  \"%\"\n", lib);

            foreach (ref ext; unit.externs) {
                // Track extern function for qualified call generation
                extern_funcs[ext.name.lexeme] = true;

                ubyte n_ret = ext.return_type.is_void() ? 0 : 1;
                sb.writef("  % % %", ext.name.lexeme, ext.params.length, n_ret);
                if (ext.is_varargs) sb.write(" varargs");
                sb.write("\n");
            }
            sb.write("end\n\n");
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
    // Function Generation
    // =========================================================================

    int gen_function(CFunction* func) {
        funcdepth++;
        scope(exit) {
            funcdepth--;
            reglocals.cleanup();
            stacklocals.cleanup();
            var_types.cleanup();
            regallocator.reset();
            labelallocator.reset();
            use_stack = false;
        }

        // Emit function header
        sb.writef("function % %\n", func.name.lexeme, func.params.length);

        // Move arguments from rarg registers to local registers
        foreach (i, ref param; func.params) {
            int r = regallocator.allocate();
            reglocals[param.name.lexeme] = r;
            var_types[param.name.lexeme] = param.type;
            sb.writef("    move r% r%\n", r, RARG1 + cast(int)i);
        }

        // Generate body
        foreach (stmt; func.body) {
            int err = gen_statement(stmt);
            if (err) return err;
        }

        // Add implicit return if needed
        if (func.body.length == 0 || func.body[$ - 1].kind != CStmtKind.RETURN) {
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
        // Allocate register for variable
        int r = regallocator.allocate();
        reglocals[stmt.name.lexeme] = r;
        var_types[stmt.name.lexeme] = stmt.var_type;

        // Initialize if needed
        if (stmt.initializer !is null) {
            int err = gen_expression(stmt.initializer, r);
            if (err) return err;
        } else {
            // Default initialize to 0
            sb.writef("    move r% 0\n", r);
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
            if (lex.length >= 3) {
                char c = lex[1];  // Skip opening quote
                if (c == '\\' && lex.length >= 4) {
                    // Escape sequence
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
                sb.writef("    move r% %\n", target, cast(int)c);
            }
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

        if (int* r = name in reglocals) {
            if (target == *r) return 0;  // Already in target register
            sb.writef("    move r% r%\n", target, *r);
            return 0;
        }

        // Must be a global or function
        sb.writef("    read r% var %\n", target, name);
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

        // Check for literal RHS optimization
        CExpr* right = expr.right;
        if (CLiteral* lit = right.as_literal()) {
            str rhs = lit.value.lexeme;
            switch (expr.op) with (CTokenType) {
                case PLUS:
                    sb.writef("    add r% r% %\n", target, lhs, rhs);
                    break;
                case MINUS:
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
                sb.writef("    add r% r% r%\n", target, lhs, rhs);
                break;
            case MINUS:
                sb.writef("    sub r% r% r%\n", target, lhs, rhs);
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
                if (target != TARGET_IS_NOTHING) {
                    if (int* r = id.name.lexeme in reglocals) {
                        error(expr.expr.token, "Cannot take address of register variable");
                        return 1;
                    }
                    sb.writef("    move r% var %\n", target, id.name.lexeme);
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
                sb.writef("    read r% r%\n", target, ptr_reg);
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
            if (id.name.lexeme in extern_funcs) {
                sb.writef("    call function C.% %\n", id.name.lexeme, expr.args.length);
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
        CExpr* lhs = expr.target;

        // Simple variable assignment
        if (CIdentifier* id = lhs.as_identifier()) {
            if (int* r = id.name.lexeme in reglocals) {
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

                sb.writef("    write r% r%\n", ptr_reg, val_reg);

                if (target != TARGET_IS_NOTHING) {
                    sb.writef("    move r% r%\n", target, val_reg);
                }

                regallocator.reset_to(before);
                return 0;
            }
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

        // TODO: Scale index by element size for proper pointer arithmetic
        // For now, assume byte-sized elements
        sb.writef("    add r% r% r%\n", arr_reg, arr_reg, idx_reg);

        if (target != TARGET_IS_NOTHING) {
            sb.writef("    read r% r%\n", target, arr_reg);
        }

        regallocator.reset_to(before);
        return 0;
    }
}
