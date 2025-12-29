/*
 * Copyright © 2021-2025, David Priver
 */
module dscript_to_dasm;

import core.stdc.stdio: fprintf, stderr;

import dlib.aliases;
import dlib.allocator: MALLOCATOR, ArenaAllocator, Allocator, Mallocator;
import dlib.box: Box;
import dlib.stringbuilder: StringBuilder, P;
import dlib.barray: Barray, make_barray;
import dlib.table: Table;
import dlib.bettercobject: BCObject;
import dlib.str_util: stripped;

import dscript.dscript;

import dvm.dvm_regs;

int
compile_to_dasm(const ubyte[] source, Box!(char[])* progtext){
    ArenaAllocator arena = ArenaAllocator(MALLOCATOR);
    scope(exit) arena.free_all;
    Barray!Token tokens = make_barray!Token(arena.allocator());
    Tokenizer tokenizer = Tokenizer(source, &tokens);
    int err = tokenizer.tokenize_tokens();
    if(err) return 1;
    tokens.bdata.resize(tokens.count);
    Parser parser = Parser(arena.allocator(), tokens[]);
    Barray!(Statement*) statements = make_barray!(Statement*)(arena.allocator());
    err = parser.parse(&statements);
    if(err) return 1;
    StringBuilder sb = {allocator:progtext.allocator};
    DasmWriter writer = DasmWriter(&sb, arena.allocator());
    err = writer.do_it(statements[]);
    writer.cleanup;
    if(err){
        sb.cleanup;
        return err;
    }
    *progtext = sb.detach;
    return 0;
}

struct RegisterAllocator {
    int alloced;
    int local_max = 0;
    enum {LOCAL_MAX = 100};
    int allocate(){
        int result = alloced++;
        // fprintf(stderr, "%d: alloced: %d\n", __LINE__, alloced);
        if(result > local_max){
            local_max = result;
            // fprintf(stderr, "New high water mark: %d\n", local_max);
        }
        return result;
    }
    void reset(){
        alloced = 0;
        // fprintf(stderr, "%d: reset: alloced: %d\n", __LINE__, alloced);
    }
    void reset_to(int r){
        alloced = r;
        // fprintf(stderr, "%d: reset_to: alloced: %d\n", __LINE__, alloced);
    }
}
struct StackAllocator {
    int nalloced;
    size_t allocate(){
        int depth = nalloced++;
        return depth;
    }
    void reset(){
        nalloced = 0;
    }
}

struct LabelAllocator {
    int nalloced;
    int allocate(){
        return nalloced++;
    }
    void reset(){
        nalloced = 0;
    }
}

struct Analysis {
    Barray!(Token) vars;
    bool vars_on_stack;
    bool has_goto;
}
struct DasmAnalyzer{
    @disable this();
    Analysis analysis;
    this(Allocator allocator){
        analysis.vars.bdata.allocator = allocator;
    }

    void analyze(Expr* e){
        final switch(e.type)with(ExprType){
            case ASSIGN:   return visit_assign(cast(Assign*)e);
            case BINARY:   return visit_binary(cast(Binary*)e);
            case GROUPING: return visit_grouping(cast(Grouping*)e);
            case LITERAL:  return visit_literal(cast(Literal*)e);
            case UNARY:    return visit_unary(cast(Unary*)e);
            case VARIABLE: return visit_var(cast(VarExpr*)e);
            case LOGICAL:  return visit_logical(cast(Logical*)e);
            case CALL:     return visit_call(cast(Call*)e);
        }
    }
    void analyze(Statement* s){
        final switch(s.type)with(StatementType){
            case BLOCK:      return visit_block(cast(Block*)s);
            case EXPRESSION: return visit_expression(cast(ExpressionStmt*)s);
            case LET:        return visit_let(cast(LetStmt*)s);
            case IF:         return visit_if(cast(IfStmt*)s);
            case WHILE:      return visit_while(cast(WhileStatement*)s);
            case FUNCTION:   return visit_function(cast(FuncStatement*)s);
            case IMPORT:     return visit_import(cast(ImportStatement*)s);
            case RETURN:     return visit_return(cast(ReturnStatement*)s);
            case CONTINUE:   return visit_continue(cast(ContinueStatement*)s);
            case BREAK:      return visit_break(cast(BreakStatement*)s);
            case GOTO:       return visit_goto(cast(GotoStatement*)s);
            case LABEL:      return visit_label(cast(LabelStatement*)s);
            case HALT:       return visit_halt(cast(HaltStatement*)s);
            case ABORT:      return visit_abort(cast(AbortStatement*)s);
            case DASM:       return visit_dasm(cast(DasmStatement*)s);
            case PAUSE:       return visit_pause(cast(DasmStatement*)s);
            case DLIMPORT:   return visit_dlimport(cast(DlimportStatement*)s);
        }
    }

    void visit_binary(Binary* expr){
        analyze(expr.left);
        analyze(expr.right);
    }
    void visit_call(Call* expr){
        analyze(expr.callee);
        foreach(a; expr.args)
            analyze(a);
    }
    void visit_grouping(Grouping* expr){
        analyze(expr.expression);
    }
    void visit_literal(Literal* expr){
    }
    void visit_unary(Unary* expr){
        if(expr.operator.type == TokenType.AMP)
            analysis.vars_on_stack = true;
        analyze(expr.right);
    }
    void visit_var(VarExpr* expr){
    }
    void visit_assign(Assign* expr){
        analyze(expr.right);
    }
    void visit_logical(Logical* expr){
        analyze(expr.left);
        analyze(expr.right);
    }
    void visit_expression(ExpressionStmt* stmt){
        analyze(stmt.expr);
    }
    void visit_let(LetStmt* stmt){
        analyze(stmt.initializer);
        analysis.vars ~= stmt.name;
    }
    void visit_if(IfStmt* stmt){
        analyze(stmt.condition);
        analyze(stmt.then_branch);
        if(stmt.else_branch !is null)
            analyze(stmt.else_branch);
    }
    void visit_while(WhileStatement* stmt){
        analyze(stmt.condition);
        analyze(stmt.statement);
    }
    void visit_return(ReturnStatement* stmt){
        analyze(stmt.value);
    }
    void visit_block(Block* stmt){
        foreach(s; stmt.statements)
            analyze(s);
    }
    void visit_function(FuncStatement* stmt){
        analysis.vars.clear();
        analysis.has_goto = false;
        foreach(s; stmt.body)
            analyze(s);
    }
    void visit_import(ImportStatement* stmt){
    }
    void visit_dasm(DasmStatement* stmt){
    }
    void visit_pause(DasmStatement* stmt){
    }
    void visit_dlimport(DlimportStatement* stmt){
    }
    void visit_halt(HaltStatement* stmt){
    }
    void visit_abort(AbortStatement* stmt){
    }
    void visit_continue(ContinueStatement* stmt){
    }
    void visit_break(BreakStatement* stmt){
    }
    void visit_goto(GotoStatement* stmt){
        analysis.has_goto = true;
    }
    void visit_label(LabelStatement* stmt){
    }

}
struct DasmWriter{
    Allocator allocator;
    StringBuilder* sb;
    RegisterAllocator regallocator;
    StackAllocator stackallocator;
    LabelAllocator labelallocator;
    int current_continue_target = -1;
    int current_break_target = -1;
    Table!(str, int) locals;
    Table!(str, int) reglocals;
    Analysis analysis;
    void cleanup(){
        locals.cleanup;
        reglocals.cleanup;
        analysis.vars.cleanup;
    }
    bool ERROR_OCCURRED = false;

    @disable this();
    this(StringBuilder* s, Allocator a){
        allocator = a;
        locals.data.allocator = a;
        reglocals.data.allocator = a;
        sb = s;
    }

    int gen_expression(Expr* e, int target){
        final switch(e.type)with(ExprType){
            case ASSIGN:   return visit_assign(cast(Assign*)e, target);
            case BINARY:   return visit_binary(cast(Binary*)e, target);
            case GROUPING: return visit_grouping(cast(Grouping*)e, target);
            case LITERAL:  return visit_literal(cast(Literal*)e, target);
            case UNARY:    return visit_unary(cast(Unary*)e, target);
            case VARIABLE: return visit_var(cast(VarExpr*)e, target);
            case LOGICAL:  return visit_logical(cast(Logical*)e, target);
            case CALL:     return visit_call(cast(Call*)e, target);
        }
    }

    int gen_statement(Statement* s){
        final switch(s.type)with(StatementType){
            case BLOCK:      return visit_block(cast(Block*)s);
            case EXPRESSION: return visit_expression(cast(ExpressionStmt*)s);
            case LET:        return visit_let(cast(LetStmt*)s);
            case IF:         return visit_if(cast(IfStmt*)s);
            case WHILE:      return visit_while(cast(WhileStatement*)s);
            case FUNCTION:   return visit_function(cast(FuncStatement*)s);
            case IMPORT:     return visit_import(cast(ImportStatement*)s);
            case RETURN:     return visit_return(cast(ReturnStatement*)s);
            case BREAK:      return visit_break(cast(BreakStatement*)s);
            case CONTINUE:   return visit_continue(cast(ContinueStatement*)s);
            case GOTO:       return visit_goto(cast(GotoStatement*)s);
            case LABEL:      return visit_label(cast(LabelStatement*)s);
            case HALT:       return visit_halt(cast(HaltStatement*)s);
            case PAUSE:       return visit_pause(cast(PauseStatement*)s);
            case ABORT:      return visit_abort(cast(AbortStatement*)s);
            case DASM:       return visit_dasm(cast(DasmStatement*)s);
            case DLIMPORT:   return visit_dlimport(cast(DlimportStatement*)s);
        }
    }

    void
    error(Token token, str message){
        ERROR_OCCURRED = true;
        int line = token.line;
        fprintf(stderr, "[line %d]: Write Error at '%.*s': %.*s\n",
                line, 
                cast(int)token.lexeme.length, token.lexeme.ptr, 
                cast(int)message.length, message.ptr);
    }

    void
    error(int line, str message){
        ERROR_OCCURRED = true;
        fprintf(stderr, "[line %d]: Write Error: %.*s\n", line, cast(int)message.length, message.ptr);
    }

    void save_reglocals(){
        for(int i = 0; i < regallocator.alloced; i++){
            sb.writef("    push r%\n", i);
        }
    }
    void restore_reglocals(){
        for(int i = regallocator.alloced-1; i >= 0; i--){
            sb.writef("    pop r%\n", i);
        }
    }

    void restore_stack(){
        sb.write("    move rsp rbp\n");
        sb.write("    pop rbp\n");
    }
    void save_stack(){
        sb.write("    push rbp\n");
        sb.write("    move rbp rsp\n");
    }
    int funcdepth;
    enum {TARGET_IS_CMP_FLAGS = -2};
    enum {TARGET_IS_NOTHING   = -1};

    int maybe_reg_from_expr(Expr* e){
        if(VarExpr* ve = e.is_variable()){
            if(int* rlocal = ve.name.lexeme in reglocals)
                return *rlocal;
        }
        return -1;
    }
    int binary_cmp(Binary* expr){
        int before = regallocator.alloced;
        scope(exit) regallocator.reset_to(before);
        int lhs = maybe_reg_from_expr(expr.left);
        if(lhs == -1){
            lhs = regallocator.allocate();
            int res = gen_expression(expr.left, lhs);
            if(res != 0) return res;
        }
        if(expr.right.is_literal() && expr.right.as_literal().value.type == TokenType.NUMBER){
            Literal* lit = expr.right.as_literal();
            str rhs = lit.value.lexeme;
            sb.writef("    scmp r% %\n", lhs, rhs);
            return 0;
        }
        else {
            int rhs = maybe_reg_from_expr(expr.right);
            if(rhs == -1){
                rhs = regallocator.allocate();
                int res2 = gen_expression(expr.right, rhs);
                if(res2 != 0) return res2;
            }
            sb.writef("    scmp r% r%\n", lhs, rhs);
            return 0;
        }
    }
    int visit_binary(Binary* expr, int target){
        // expr.dump; fprintf(stderr, "\n");
        if(target == TARGET_IS_CMP_FLAGS)
            return binary_cmp(expr);
        // std.stdio.writefln("HERE: %d", __LINE__);
        int before = regallocator.alloced;
        int lhs = target;
        int res_ = gen_expression(expr.left, lhs);
        if(res_ != 0) return res_;
        if(Literal* lit = expr.right.is_literal()){
            if(target == TARGET_IS_NOTHING)
                return 0;
            const vt = lit.value.type;
            if(vt == TokenType.STRING || vt == TokenType.NUMBER|| vt == TokenType.HEX || vt == TokenType.PHEX || vt == TokenType.SNUM || vt == TokenType.BNUM){
                str rhs = lit.value.lexeme;
                switch(expr.operator.type)with(TokenType){
                    case MINUS:
                        sb.writef("    sub r% r% %\n", target, lhs, rhs);
                        break;
                    case PLUS:
                        sb.writef("    add r% r% %\n", target, lhs, rhs);
                        break;
                    case STAR:
                        sb.writef("    mul r% r% %\n", target, lhs, rhs);
                        break;
                    case SLASH:
                        sb.writef("    div r% rjunk r% %\n", target, lhs, rhs);
                        break;
                    case MOD:
                        sb.writef("    div rjunk r% r% %\n", target, lhs, rhs);
                        break;
                    case BAR:
                        sb.writef("    or r% r% %\n", target, lhs, rhs);
                        break;
                    case AMP:
                        sb.writef("    and r% r% %\n", target, lhs, rhs);
                        break;
                    case HAT:
                        sb.writef("    xor r% r% %\n", target, lhs, rhs);
                        break;
                    case LESS_LESS:
                        sb.writef("    shl r% r% %\n", target, lhs, rhs);
                        break;
                    case GREATER_GREATER:
                        sb.writef("    shr r% r% %\n", target, lhs, rhs);
                        break;
                    case BANG_EQUAL:
                        sb.writef("    scmp r% %\n", lhs, rhs);
                        sb.writef("    move r% 0\n", target);
                        sb.writef("    cmov ne r% 1\n", target);
                        break;
                    case EQUAL_EQUAL:
                        sb.writef("    scmp r% %\n", lhs, rhs);
                        sb.writef("    move r% 0\n", target);
                        sb.writef("    cmov eq r% 1\n", target);
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
                    default:
                        error(expr.operator, "Unhandled binary op");
                        return -1;
                }
                return 0;
            }
            error(lit.value, "Unhandled literal type");
            return -1;
        }
        else {
            if(target == TARGET_IS_NOTHING){
                return gen_expression(expr.right, target);
            }
            int rhs;
            if(expr.right.is_variable() && expr.right.as_variable().name.lexeme in reglocals){
                rhs = reglocals[expr.right.as_variable().name.lexeme];
            }
            else {
                rhs = regallocator.allocate();
                int res = gen_expression(expr.right, rhs);
                if(res != 0) return res;
            }
            switch(expr.operator.type)with(TokenType){
                case MINUS:
                    sb.writef("    sub r% r% r%\n", target, lhs, rhs);
                    break;
                case PLUS:
                    sb.writef("    add r% r% r%\n", target, lhs, rhs);
                    break;
                case STAR:
                    sb.writef("    mul r% r% r%\n", target, lhs, rhs);
                    break;
                case SLASH:
                    sb.writef("    div r% rjunk r% r%\n", target, lhs, rhs);
                    break;
                case MOD:
                    sb.writef("    div rjunk r% r% r%\n", target, lhs, rhs);
                    break;
                case BAR:
                    sb.writef("    or r% r% r%\n", target, lhs, rhs);
                    break;
                case AMP:
                    sb.writef("    and r% r% r%\n", target, lhs, rhs);
                    break;
                case HAT:
                    sb.writef("    xor r% r% r%\n", target, lhs, rhs);
                    break;
                case LESS_LESS:
                    sb.writef("    shl r% r% r%\n", target, lhs, rhs);
                    break;
                case GREATER_GREATER:
                    sb.writef("    shr r% r% r%\n", target, lhs, rhs);
                    break;
                case BANG_EQUAL:
                    sb.writef("    scmp r% r%\n", lhs, rhs);
                    sb.writef("    move r% 0\n", target);
                    sb.writef("    cmov ne r% 1\n", target);
                    break;
                case EQUAL_EQUAL:
                    sb.writef("    scmp r% r%\n", lhs, rhs);
                    sb.writef("    move r% 0\n", target);
                    sb.writef("    cmov eq r% 1\n", target);
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
                default:
                    error(expr.operator, "Unhandled binary op");
                    return -1;
            }
        }
        regallocator.reset_to(before);
        return 0;
    }
    int visit_literal(Literal* expr, int target){
        if(target == TARGET_IS_NOTHING) return 0;
        if(expr.value.type == TokenType.NUMBER || expr.value.type == TokenType.HEX || expr.value.type == TokenType.PHEX || expr.value.type == TokenType.SNUM || expr.value.type == TokenType.BNUM){
            sb.writef("    move r% %\n", target, expr.value.lexeme);
            return 0;
        }
        if(expr.value.type == TokenType.NIL){
            sb.writef("    move r% 0\n", target);
            return 0;
        }
        if(expr.value.type == TokenType.TRUE){
            sb.writef("    move r% 1\n", target);
            return 0;
        }
        if(expr.value.type == TokenType.FALSE){
            sb.writef("    move r% 0\n", target);
            return 0;
        }
        if(expr.value.type == TokenType.STRING){
            sb.writef("    move r% %\n", target, expr.value.lexeme);
            return 0;
        }
        fprintf(stderr, "expr.value.type: %d\n", cast(int)expr.value.type);
        fprintf(stderr, "Label: %d\n", cast(int)TokenType.LABEL);
        fprintf(stderr, "%s:%d\n", __FILE__.ptr, __LINE__);
        return -1;
    }
    int visit_unary(Unary* expr, int target){
        if(expr.operator.type == TokenType.AMP){
            if(VarExpr* ve = expr.right.is_variable()){
                if(int* rlocal = ve.name.lexeme in reglocals){
                    error(ve.name, "RHS of & is in registers");
                    return -1;
                }
                if(target != TARGET_IS_NOTHING){
                    if(int* local = ve.name.lexeme in locals){
                        sb.writef("    add r% rbp %\n", target, P(*local));
                        return 0;
                    }
                    sb.writef("    move r% var %\n", target, ve.name.lexeme);
                    return 0;
                }
                return 0;
            }
            else {
                error(expr.operator, "Unhandled rhs of &");
                return -1;
            }
        }
        int v = gen_expression(expr.right, target);
        if(v < 0) return v;
        if(target != TARGET_IS_NOTHING){
            switch(expr.operator.type)with(TokenType){
                case MINUS:
                    sb.writef("    neg r% r%\n", target, target);
                    break;
                case BANG:
                    sb.writef("    not r% r%\n", target, target);
                    break;
                default:
                    error(expr.operator, "Unhandled unary operator");
                    return -1;
            }
        }
        return 0;
    }
    int visit_call(Call* expr, int target){
        int rarg1 = RegisterNames.RARG1;
        for(int i = 0; i < expr.args.length; i++){
            Expr* arg = expr.args[i].ungroup;
            int before = regallocator.alloced;
            int res = gen_expression(arg, rarg1+i);
            if(res != 0) return res;
            if(i != expr.args.length-1){
                //can elide the push/pop for last arg
                sb.writef("    push rarg%\n", 1+i);
            }
            regallocator.reset_to(before);
        }
        for(int i = 1; i < expr.args.length; i++){
            sb.writef("    pop rarg%\n", 1+expr.args.length-1-i);
        }
        bool called = false;
        // this is wrong but whatever
        if(VarExpr* ve = expr.callee.is_variable()){
            if(ve.name.type == TokenType.IDENTIFIER){
                // HACK
                bool hack = (target == regallocator.alloced-1);
                if(hack) regallocator.alloced--;
                save_reglocals();
                sb.writef("    call function %\n", ve.name.lexeme);
                restore_reglocals();
                if(hack) regallocator.alloced++;
                called = true;
            }
        }
        if(!called){
            int before = regallocator.alloced;
            int func = regallocator.allocate();
            int res = gen_expression(expr.callee, func);
            if(res != 0) return res;
            regallocator.reset_to(before);
            save_reglocals();
            sb.writef("    call r%\n", func);
            restore_reglocals();
        }
        if(target != TARGET_IS_NOTHING)
            sb.writef(  "    move r% rout1\n", target);
        return 0;
    }
    int do_tail_call(Call* expr){
        int rarg1 = RegisterNames.RARG1;
        for(int i = 0; i < expr.args.length; i++){
            Expr* arg = expr.args[i].ungroup;
            int before = regallocator.alloced;
            int res = gen_expression(arg, rarg1+i);
            if(res != 0) return res;
            regallocator.reset_to(before);
        }
        bool called = false;
        // this is wrong but whatever
        if(VarExpr* ve = expr.callee.is_variable()){
            if(ve.name.type == TokenType.IDENTIFIER){
                if(analysis.vars_on_stack)
                    restore_stack();
                sb.writef("    tail_call function %\n", ve.name.lexeme);
                called = true;
            }
        }
        if(!called){
            int before = regallocator.alloced;
            int func = regallocator.allocate();
            int res = gen_expression(expr.callee, func);
            if(res != 0) return res;
            regallocator.reset_to(before);
            if(analysis.vars_on_stack)
                restore_stack();
            sb.writef("    tail_call r%\n", func);
        }
        return 0;
    }
    int visit_grouping(Grouping* expr, int target){
        int res = gen_expression(expr.expression, target);
        if(res != 0) return res;
        return 0;
    }
    int visit_var(VarExpr* expr, int target){
        if(target == TARGET_IS_NOTHING) return 0;
        if(int* rlocal = expr.name.lexeme in reglocals){
            if(target == *rlocal){
                // loading into the same register
                return 0;
            }
            sb.writef("    move r% r%\n", target, *rlocal);
            return 0;
        }
        if(int* local = expr.name.lexeme in locals){
            sb.writef("    local_read r% %\n", target, P(*local));
            return 0;
        }
        // must be a global.
        sb.writef("    read r% var %\n", target, expr.name.lexeme);
        return 0;
    }
    int visit_assign(Assign* expr, int target){
        // ignore target;
        if(int* rlocal = expr.name.lexeme in reglocals){
            if(Literal* lit = expr.right.is_literal()){
                switch(lit.value.type)with(TokenType){
                    case NIL:
                    case FALSE:
                        sb.writef("    move r% 0\n", *rlocal);
                        break;
                    case TRUE:
                        sb.writef("    move r% 1\n", *rlocal);
                        break;
                    case STRING:
                        sb.writef("    move r% %\n", *rlocal, lit.value.lexeme);
                        break;
                    case NUMBER:
                        sb.writef("    move r% %\n", *rlocal, lit.value.lexeme);
                        break;
                    case BNUM:
                    case SNUM:
                    case PHEX:
                    case HEX:
                        sb.writef("    move r% %\n", *rlocal, lit.value.lexeme);
                        break;
                    default:
                        error(lit.value, "Unhandled literal type in assign");
                        return -1;
                }
            }
            else {
                int temp = - 1;
                // optimization for x = x + 1
                if(Binary* bin = expr.right.is_binary()){
                    Expr* l = bin.left;
                    Expr* r = bin.right;
                    if(l.is_variable && r.is_literal()){
                        VarExpr* ve = l.as_variable();
                        if(ve.name == expr.name){
                            if(int* vr = ve.name.lexeme in reglocals)
                                temp = *vr;
                        }
                    }
                }
                // temp = -1;

                int before = regallocator.alloced;
                scope(exit) regallocator.reset_to(before);
                if(temp == -1){
                    temp = regallocator.allocate();
                    int res = gen_expression(expr.right, temp);
                    if(res != 0) return res;
                    sb.writef("    move r% r%\n", *rlocal, temp);
                }
                else {
                    int res = gen_expression(expr.right, temp);
                    if(res != 0) return res;
                }
            }
            return 0;
        }
        if(int* local = expr.name.lexeme in locals){
            if(Literal* lit = expr.right.is_literal()){
                switch(lit.value.type)with(TokenType){
                    case NIL:
                    case FALSE:
                        sb.writef("    local_write % 0\n", P(*local));
                        break;
                    case TRUE:
                        sb.writef("    local_write % 1\n", P(*local));
                        break;
                    case STRING:
                        sb.writef("    local_write % %\n", P(*local), lit.value.lexeme);
                        break;
                    case NUMBER:
                        sb.writef("    local_write % %\n", P(*local), lit.value.lexeme);
                        break;
                    case BNUM:
                    case SNUM:
                    case PHEX:
                    case HEX:
                        sb.writef("    local_write % %\n", P(*local), lit.value.lexeme);
                        break;
                    default:
                        error(lit.value, "Unhandled literal type in assign");
                        return -1;
                }
            }
            else {
                int before = regallocator.alloced;
                int temp = regallocator.allocate();
                int res = gen_expression(expr.right, temp);
                if(res != 0) return res;
                regallocator.reset_to(before);
                sb.writef("    local_write % r%\n", P(*local), temp);
            }
            return 0;
        }
        // must be a global.
        if(Literal* lit = expr.right.is_literal()){
            int before = regallocator.alloced;
            int temp = regallocator.allocate();
            sb.writef("    move r% var %\n", temp, expr.name.lexeme);
            regallocator.reset_to(before);
            switch(lit.value.type)with(TokenType){
                case NIL:
                case FALSE:
                    sb.writef("    write r% 0\n", temp);
                    break;
                case TRUE:
                    sb.writef("    write r% 1\n", temp);
                    break;
                case STRING:
                    sb.writef("    write r% %\n", temp, lit.value.lexeme);
                    break;
                case NUMBER:
                    sb.writef("    write r% %\n", temp, lit.value.lexeme);
                    break;
                case BNUM:
                case SNUM:
                case PHEX:
                case HEX:
                    sb.writef("    write r% %\n", temp, lit.value.lexeme);
                    break;
                default:
                    error(lit.value, "Unhandled literal type in assign");
                    return -1;
            }
        }
        else {
            int before = regallocator.alloced;
            int lhs = regallocator.allocate();
            int rhs = regallocator.allocate();
            int res = gen_expression(expr.right, rhs);
            if(res != 0) return res;
            regallocator.reset_to(before);
            sb.writef("    move r% var %\n", lhs, expr.name.lexeme);
            sb.writef("    write r% r%\n", lhs, rhs);
        }
        return 0;
    }
    int visit_logical(Logical* expr, int target){
        if(target == TARGET_IS_NOTHING){
            int before = regallocator.alloced;
            int lhs = regallocator.allocate();
            int res_ = gen_expression(expr.left, lhs);
            int after = -1; 
            after = labelallocator.allocate();
            if(res_ != 0) return res_;
            if(expr.operator.type == TokenType.AND){
                sb.writef("    cmp r% 0\n", lhs);
                sb.writef("    jump eq label L%\n", after);
            }
            else {
                assert(expr.operator.type == TokenType.OR);
                sb.writef("    cmp r% 0\n", lhs);
                sb.writef("    jump ne label L%\n", after);
            }
            regallocator.reset_to(before);
            res_ = gen_expression(expr.right, TARGET_IS_NOTHING);
            if(res_ != 0) return res_;
            sb.writef("  label L%\n", after);
            return 0;
        }
        else {
            int before = regallocator.alloced;
            int lhs = regallocator.allocate();
            int res_ = gen_expression(expr.left, lhs);
            int after = -1; 
            after = labelallocator.allocate();
            if(res_ != 0) return res_;
            if(expr.operator.type == TokenType.AND){
                sb.writef("    move r% 0\n", target);
                sb.writef("    cmp r% 0\n", lhs);
                sb.writef("    jump eq label L%\n", after);
            }
            else {
                assert(expr.operator.type == TokenType.OR);
                sb.writef("    move r% 1\n", target);
                sb.writef("    cmp r% 0\n", lhs);
                sb.writef("    jump ne label L%\n", after);
            }
            int rhs = regallocator.allocate();
            res_ = gen_expression(expr.right, rhs);
            if(res_ != 0) return res_;
            if(expr.operator.type == TokenType.AND){
                sb.writef("    cmp r% 0\n", rhs);
                sb.writef("    cmov ne r% 1\n", target);
            }
            else {
                sb.writef("    cmp r% 0\n", rhs);
                sb.writef("    cmov eq r% 0\n", target);
            }

            sb.writef("  label L%\n", after);
            regallocator.reset_to(before);
            return 0;
        }
        error(expr.operator, "Unhandled logical operator");
        return -1;
    }
    int visit_expression(ExpressionStmt* stmt){
        if(!funcdepth){
            error(0, "Expression outside of function");
            return -1;
        }
        int before = regallocator.alloced;
        int result = gen_expression(stmt.expr, TARGET_IS_NOTHING);
        regallocator.reset_to(before);
        return result;
    }
    int visit_let(LetStmt* stmt){
        if(!funcdepth) {
            // global variable;
            if(stmt.initializer.type != ExprType.LITERAL){
                error(stmt.name, "Non constant initializer for global variable");
                return -1;
            }
            Literal* lit = cast(Literal*)stmt.initializer;
            switch(lit.value.type) with(TokenType){
                case FALSE:
                case NIL:
                    sb.writef("var % 0\n", stmt.name.lexeme);
                    return 0;
                case TRUE:
                    sb.writef("var % 1\n", stmt.name.lexeme);
                    return 0;
                case STRING:
                    sb.writef("var % %\n", stmt.name.lexeme, lit.value.lexeme);
                    return 0;
                case NUMBER:
                    sb.writef("var % %\n", stmt.name.lexeme, lit.value.lexeme);
                    return 0;
                default:
                    error(lit.value, "Unhandled literal type");
                    return -1;
            }
        }
        // initializer is guaranteed to be the NIl Expr or a real expr.
        // if(!stmt.initializer) return 0;
        // Turn the initializer into an assignment statement.
        Assign assign = Assign.make(stmt.name, stmt.initializer);
        int res = visit_assign(&assign, 0);
        return res;
        static if(0){
        if(int* rlocal = stmt.name.lexeme in reglocals){
            int res = gen_statement(stmt.initializer, *rlocal);
            if(res != 0) return res;
            return 0;
        }
        if(int* local = stmt.name.lexeme in locals){
            int before = regallocator.alloced;
            int temp = regallocator.allocate();
            int res = gen_statement(stmt.initializer, temp);
            if(res != 0) return res;
            regallocator.reset_to(before);
            sb.writef("    local_write % r%\n", P(*local), temp);
            return 0;
        }
        error(stmt.name, "Unhandled var stmt");
        return -1;
        }
    }
    int visit_dasm(DasmStatement* stmt){
        sb.writef("%\n", stmt.dasm.lexeme[1..$-1]);
        return 0;
    }
    int visit_block(Block* stmt){
        if(!funcdepth) {
            error(0, "Block outside of function");
            return -1;
        }
        foreach(s; stmt.statements){
            int res = gen_statement(s);
            if(res != 0) return res;
        }
        return 0;
    }
    int visit_if(IfStmt* stmt){
        if(!funcdepth) {
            error(0, "If outside of function");
            return -1;
        }
        int label = labelallocator.allocate();
        if(Binary* b = stmt.condition.is_binary()){
            switch(b.operator.type)with(TokenType){
                case GREATER:
                case EQUAL_EQUAL:
                case GREATER_EQUAL:
                case LESS_EQUAL:
                case LESS:
                case BANG_EQUAL:
                    int res = gen_expression(stmt.condition, TARGET_IS_CMP_FLAGS);
                    if(res != 0) return res;
                    break;
                default:
                    goto Lgeneric;
            }
            string jmpmode;
            // jump modes need to be inverted
            // as we only jump if condition is false
            switch(b.operator.type)with(TokenType){
                case GREATER:
                    jmpmode = "le";
                    break;
                case EQUAL_EQUAL:
                    jmpmode = "ne";
                    break;
                case GREATER_EQUAL:
                    jmpmode = "lt";
                    break;
                case LESS_EQUAL:
                    jmpmode = "gt";
                    break;
                case LESS:
                    jmpmode = "ge";
                    break;
                case BANG_EQUAL:
                    jmpmode = "eq";
                    break;
                default:
                    assert(0);
            }
            sb.writef("    jump % label L%\n", jmpmode, label);
        }
        else if(Unary*u = stmt.condition.is_unary()){
            switch(u.operator.type)with(TokenType){
                case BANG:{
                    int before = regallocator.alloced;
                    int cond = regallocator.allocate();
                    scope(exit) regallocator.reset_to(before);
                    int res = gen_expression(u.right, cond);
                    if(res != 0) return res;
                    sb.writef("    cmp r% 0\n", cond);
                    sb.writef("    jump ne label L%\n", label);
                }break;
                default:
                    goto Lgeneric;
            }
        }
        else {
            Lgeneric:
            int before = regallocator.alloced;
            int cond = regallocator.allocate();
            scope(exit) regallocator.reset_to(before);
            int res = gen_expression(stmt.condition, cond);
            if(res != 0) return res;
            sb.writef("    cmp r% 0\n", cond);
            sb.writef("    jump eq label L%\n", label);
        }
        int res = gen_statement(stmt.then_branch);
        if(res != 0) return res;
        if(stmt.else_branch){
            int after_else_label = labelallocator.allocate();
            sb.writef("    move rip label L%\n", after_else_label);
            sb.writef("  label L%\n", label);
            int r = gen_statement(stmt.else_branch);
            if(r != 0) return r;
            sb.writef("  label L%\n", after_else_label);
        }
        else {
            sb.writef("  label L%\n", label);
        }
        return 0;
    }
    int visit_function(FuncStatement* stmt){
        if(funcdepth){
            error(stmt.name, "Nested function");
            return -1;
        }
        DasmAnalyzer analyzer = DasmAnalyzer(allocator);
        analyzer.visit_function(stmt);
        analysis = analyzer.analysis;
        funcdepth++;
        scope(exit){
            funcdepth--;
            locals.cleanup();
            reglocals.cleanup();
            regallocator.reset();
            stackallocator.reset();
            labelallocator.reset();
            analysis.vars.cleanup();
        }
        sb.writef("function % %\n", stmt.name.lexeme, stmt.params.length);
        int rarg = 10;
        foreach(p; stmt.params){
            int r = regallocator.allocate();
            reglocals[p.lexeme] = r;
            sb.writef("    move r% r%\n", r, rarg++);
        }
        if(analysis.vars[].length > 4)
            analysis.vars_on_stack = true;
        if(analysis.vars_on_stack)
            save_stack();
        if(!analysis.vars_on_stack){
            foreach(v; analysis.vars){
                if(v.lexeme in reglocals){
                    error(v, "duplicate var");
                    return 1;
                }
                int r = regallocator.allocate();
                reglocals[v.lexeme] = r;
            }
        }
        else {
            // our stack grows upwards
            uint nvars = 0;
            foreach(v; analysis.vars){
                if(v.lexeme in locals){
                    error(v, "duplicate var");
                    return 1;
                }
                size_t s = stackallocator.allocate();
                locals[v.lexeme] = cast(int)s;
                nvars++;
            }
            sb.writef("    add rsp rsp %\n", P(nvars));
        }
        foreach(Statement* s; stmt.body){
            int res = gen_statement(s);
            if(res != 0) return res;
        }
        if(!stmt.body.length || stmt.body[$-1].type != StatementType.RETURN){
            if(analysis.vars_on_stack)
                restore_stack();
            sb.write("    ret\n");
        }
        sb.write("end\n");
        return 0;
    }
    int visit_import(ImportStatement* stmt){
        sb.writef("import %\n", stmt.name.lexeme);
        return 0;
    }
    int visit_dlimport(DlimportStatement* stmt){
        // Strip quotes from library path
        str lib = stmt.library_path.lexeme;
        if(lib.length >= 2 && lib[0] == '"' && lib[$-1] == '"')
            lib = lib[1..$-1];
        sb.writef("dlimport \"%\" %\n", lib, stmt.alias_name.lexeme);
        foreach(func; stmt.funcs){
            ubyte n_ret = func.return_type.lexeme == "void" ? 0 : 1;
            sb.writef("  % % %\n", func.name.lexeme, func.n_args, n_ret);
        }
        sb.write("end\n");
        return 0;
    }
    int visit_halt(HaltStatement* stmt){
        sb.writef("    halt\n");
        return 0;
    }
    int visit_pause(PauseStatement* stmt){
        sb.writef("    pause\n");
        return 0;
    }
    int visit_abort(AbortStatement* stmt){
        sb.writef("    abort\n");
        return 0;
    }
    int visit_return(ReturnStatement* stmt){
        if(!funcdepth) {
            error(stmt.keyword, "Return outside of function");
            return -1;
        }
        if(Call* c = stmt.value.is_call())
            return do_tail_call(c);
        if(stmt.value !is &NilExpr_.exp){
            int before = regallocator.alloced;
            int temp = regallocator.allocate();
            int rout = RegisterNames.ROUT1;
            int res = gen_expression(stmt.value, temp);
            if(res != 0) return res;
            regallocator.reset_to(before);
            sb.writef("    move r% r%\n", rout, temp);
        }
        if(analysis.vars_on_stack)
            restore_stack();
        sb.write("    ret\n");
        return 0;
    }
    int visit_while(WhileStatement* stmt){
        if(!funcdepth) {
            error(0, "`while` outside of function");
            return -1;
        }
        int prev_continue_target = current_continue_target;
        int prev_break_target = current_continue_target;
        scope(exit){
            current_continue_target = prev_continue_target;
            current_break_target = prev_break_target;
        }
        int top = labelallocator.allocate();
        // after the loop
        int after = labelallocator.allocate();
        current_continue_target = top;
        current_break_target = after;
        // TODO: abstract over this instead of this
        // copy paste silliness?
        if(Literal* lit = stmt.condition.is_literal()){
            switch(lit.value.type)with(TokenType){
                case TRUE:
                    sb.writef("  label L%\n", top);
                    break;
                case FALSE:
                    sb.writef("  label L%\n", top);
                    sb.writef("  move rip label L%\n", after);
                    // labelallocator.nalloced = top-1;
                    break;
                case NIL:
                    goto case FALSE;
                    labelallocator.nalloced = top-1;
                    return 0;
                case NUMBER:
                case BNUM:
                case SNUM:
                case PHEX:
                case HEX:
                    if(lit.as_number()){
                        goto case TRUE;
                        sb.writef("  label L%\n", top);
                        break;
                    }
                    goto case FALSE;
                    labelallocator.nalloced = top-1;
                    return 0;
                default:
                    error(lit.value, "Unhandled literal type for while condition");
                    return -1;
            }
        }
        else if(Binary* b = stmt.condition.is_binary()){
            sb.writef("  label L%\n", top);
            switch(b.operator.type)with(TokenType){
                case GREATER:
                case EQUAL_EQUAL:
                case GREATER_EQUAL:
                case LESS_EQUAL:
                case LESS:
                case BANG_EQUAL:
                    int res = gen_expression(stmt.condition, TARGET_IS_CMP_FLAGS);
                    if(res != 0) return res;
                    break;
                default:
                    goto Lgeneric;
            }
            string jmpmode;
            // jump modes need to be inverted
            // as we only jump if condition is false
            switch(b.operator.type)with(TokenType){
                case GREATER:
                    jmpmode = "le";
                    break;
                case EQUAL_EQUAL:
                    jmpmode = "ne";
                    break;
                case GREATER_EQUAL:
                    jmpmode = "lt";
                    break;
                case LESS_EQUAL:
                    jmpmode = "gt";
                    break;
                case LESS:
                    jmpmode = "ge";
                    break;
                case BANG_EQUAL:
                    jmpmode = "eq";
                    break;
                default:
                    assert(0);
            }
            sb.writef("    jump % label L%\n", jmpmode, after);
        }
        else {
            sb.writef("  label L%\n", top);
            Lgeneric:
            int before = regallocator.alloced;
            scope(exit) regallocator.reset_to(before);
            int cond = maybe_reg_from_expr(stmt.condition);
            if(cond == -1) cond = regallocator.allocate();
            int res = gen_expression(stmt.condition, cond);
            if(res != 0) return res;
            sb.writef("    cmp r% 0\n", cond);
            sb.writef("    jump eq label L%\n", after);
        }
        int res = gen_statement(stmt.statement);
        if(res != 0) return res;
        sb.writef("    move rip label L%\n", top);
        sb.writef("  label L%\n", after);
        return 0;
    }
    int do_it(Statement*[] stmts){
        foreach(Statement* s; stmts){
            int res = gen_statement(s);
            if(res != 0) return res;
        }
        return 0;
    }
    int visit_goto(GotoStatement* stmt){
        sb.writef("    move rip label L%\n", stmt.label.lexeme);
        return 0;
    }
    int visit_continue(ContinueStatement* stmt){
        if(current_continue_target == -1){
            error(-1, "No continue target");
            return 1;
        }
        sb.writef("    move rip label L%\n", current_continue_target);
        return 0;
    }
    int visit_break(BreakStatement* stmt){
        if(current_break_target == -1){
            error(-1, "No break target");
            return 1;
        }
        sb.writef("    move rip label L%\n", current_break_target);
        return 0;
    }
    int visit_label(LabelStatement* stmt){
        sb.writef("  label L%\n", stmt.label.lexeme);
        return 0;
    }
}

