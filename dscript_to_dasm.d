/*
 * Copyright © 2021-2023, David Priver
 */
module dscript_to_dasm;

import core.stdc.stdio: fprintf, stderr;

import dlib.aliases;
import dlib.allocator: Mallocator, ArenaAllocator;
import dlib.box: Box;
import dlib.stringbuilder: StringBuilder, P;
import dlib.barray: Barray, make_barray;
import dlib.table: Table;
import dlib.bettercobject: BCObject;
import dlib.str_util: stripped;

import dscript.dscript;

import dvm.dvm_regs;

int
compile_to_dasm(const ubyte[] source, Box!(char[], Mallocator)* progtext){
    ArenaAllocator!(Mallocator) arena;
    scope(exit) arena.free_all;
    auto tokens = make_barray!Token(&arena);
    auto tokenizer = Tokenizer!(typeof(tokens))(source, &tokens);
    int err = tokenizer.tokenizeTokens();
    if(err) return 1;
    tokens.bdata.resize(tokens.count);
    auto parser = Parser!(typeof(arena)*)(&arena, tokens[]);
    auto statements = make_barray!(Statement*)(&arena);
    err = parser.parse(&statements);
    if(err) return 1;
    StringBuilder!Mallocator sb;
    scope writer = new DasmWriter!(typeof(sb), typeof(arena)*)(&sb, &arena);
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

struct Analysis(A) {
    Barray!(Token, A) vars;
    bool vars_on_stack;
    bool has_goto;
}
class DasmAnalyzer(A): BCObject, Visitor!void, StatementVisitor!void {
    @disable this();
    Analysis!A analysis;
    this(A allocator){
        analysis.vars.bdata.allocator = allocator;
    }

    void visit(Binary* expr){
        expr.left.accept(this);
        expr.right.accept(this);
    }
    void visit(Call* expr){
        expr.callee.accept(this);
        foreach(a; expr.args)
            a.accept(this);
    }
    void visit(Grouping* expr){
        expr.expression.accept(this);
    }
    void visit(Literal* expr){
    }
    void visit(Unary* expr){
        expr.right.accept(this);
    }
    void visit(VarExpr* expr){
    }
    void visit(Assign* expr){
        expr.right.accept(this);
    }
    void visit(Logical* expr){
        expr.left.accept(this);
        expr.right.accept(this);
    }
    void visit(ExpressionStmt* stmt){
        stmt.expr.accept(this);
    }
    void visit(LetStmt* stmt){
        stmt.initializer.accept(this);
        analysis.vars ~= stmt.name;
    }
    void visit(IfStmt* stmt){
        stmt.condition.accept(this);
        stmt.thenBranch.accept(this);
        if(stmt.elseBranch !is null)
            stmt.elseBranch.accept(this);
    }
    void visit(WhileStatement* stmt){
        stmt.condition.accept(this);
        stmt.statement.accept(this);
    }
    void visit(ReturnStatement* stmt){
        stmt.value.accept(this);
    }
    void visit(Block* stmt){
        foreach(s; stmt.statements)
            s.accept(this);
    }
    void visit(FuncStatement* stmt){
        analysis.vars.clear();
        analysis.has_goto = false;
        foreach(s; stmt.body)
            s.accept(this);
    }
    void visit(ImportStatement* stmt){
    }
    void visit(HaltStatement* stmt){
    }
    void visit(AbortStatement* stmt){
    }
    void visit(GotoStatement* stmt){
        analysis.has_goto = true;
    }
    void visit(LabelStatement* stmt){
    }

}
class DasmWriter(SB, A): BCObject, RegVisitor!int, StatementVisitor!int {
    A allocator;
    SB* sb;
    RegisterAllocator regallocator;
    StackAllocator stackallocator;
    LabelAllocator labelallocator;
    Table!(str, int) locals;
    Table!(str, int) reglocals;
    Analysis!A analysis;
    void cleanup(){
        locals.cleanup;
        reglocals.cleanup;
        analysis.vars.cleanup;
    }
    bool ERROR_OCCURRED = false;

    @disable this();
    this(SB* s, A a){
        allocator = a;
        sb = s;
    }

    void
    error(Token token, str message){
        ERROR_OCCURRED = true;
        int line = token.line;
        fprintf(stderr, "[line %d]: Write Error at '%.*s': %.*s\n", line, cast(int)token.lexeme.length, token.lexeme.ptr, cast(int)message.length, message.ptr);
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
    int binary_cmp(Binary* expr){
        int before = regallocator.alloced;
        scope(exit) regallocator.reset_to(before);
        int lhs;
        if(expr.left.type == ExprType.VARIABLE && (cast(VarExpr*)expr.left).name.lexeme in reglocals){
            lhs = *((cast(VarExpr*)expr.left).name.lexeme in reglocals);
        }
        else {
            lhs = regallocator.allocate();
            int res = expr.left.accept(this, lhs);
            if(res != 0) return res;
        }
        if(expr.right.type == ExprType.LITERAL && (cast(Literal*)expr.right).value.type == TokenType.NUMBER){
            auto lit = cast(Literal*)expr.right;
            auto rhs = cast(size_t)lit.value.number;
            sb.writef("    scmp r% %\n", lhs, rhs);
            return 0;
        }
        else {
            int rhs;
            if(expr.right.type == ExprType.VARIABLE && (cast(VarExpr*)expr.right).name.lexeme in reglocals){
                rhs = *((cast(VarExpr*)expr.right).name.lexeme in reglocals);
            }
            else {
                rhs = regallocator.allocate();
                int res2 = expr.right.accept(this, rhs);
                if(res2 != 0) return res2;
            }
            sb.writef("    scmp r% r%\n", lhs, rhs);
            return 0;
        }
    }
    int visit(Binary* expr, int target){
        // expr.dump; fprintf(stderr, "\n");
        if(target == TARGET_IS_CMP_FLAGS)
            return binary_cmp(expr);
        // std.stdio.writefln("HERE: %d", __LINE__);
        int before = regallocator.alloced;
        int lhs = target;
        int res_ = expr.left.accept(this, lhs);
        if(res_ != 0) return res_;
        if(expr.right.type == ExprType.LITERAL){
            if(target == TARGET_IS_NOTHING)
                return 0;
            auto lit = cast(Literal*)expr.right;
            if(lit.value.type == TokenType.NUMBER){
                auto rhs = cast(size_t) lit.value.number;
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
            }
            if(lit.value.type == TokenType.STRING){
                auto rhs = lit.value.string_;
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
                    default:
                        error(expr.operator, "Unhandled binary op");
                        return -1;
                }
            }
        }
        else {
            if(target == TARGET_IS_NOTHING){
                return expr.right.accept(this, target);
            }
            int rhs;
            if(expr.right.type == ExprType.VARIABLE && (cast(VarExpr*)expr.right).name.lexeme in reglocals){
                rhs = reglocals[(cast(VarExpr*)expr.right).name.lexeme];
            }
            else {
                rhs = regallocator.allocate();
                int res = expr.right.accept(this, rhs);
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
    int visit(Literal* expr, int target){
        if(target == TARGET_IS_NOTHING) return 0;
        if(expr.value.type == TokenType.NUMBER){
            sb.writef("    move r% %\n", target, cast(size_t)expr.value.number);
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
            sb.writef("    move r% \"%\"\n", target, expr.value.string_);
            return 0;
        }
        fprintf(stderr, "expr.value.type: %d\n", cast(int)expr.value.type);
        fprintf(stderr, "Label: %d\n", cast(int)TokenType.LABEL);
        fprintf(stderr, "%d\n", __LINE__);
        return -1;
    }
    int visit(Unary* expr, int target){
        int v = expr.right.accept(this, target);
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
    int visit(Call* expr, int target){
        int rarg1 = RegisterNames.RARG1;
        for(int i = 0; i < expr.args.length; i++){
            auto arg = expr.args[i].ungroup;
            int before = regallocator.alloced;
            int res = arg.accept(this, rarg1+i);
            if(res != 0) return res;
            if(i != expr.args.length-1){
                //can elide the push/pop for last arg
                sb.writef("    push r%\n", rarg1+i);
            }
            regallocator.reset_to(before);
        }
        for(int i = 1; i < expr.args.length; i++){
            sb.writef("    pop r%\n", rarg1+expr.args.length-1-i);
        }
        bool called = false;
        // this is wrong but whatever
        if(expr.callee.type == ExprType.VARIABLE){
            auto l = cast(Literal*)expr.callee;
            if(l.value.type == TokenType.IDENTIFIER){
                // HACK
                bool hack = (target == regallocator.alloced-1);
                if(hack) regallocator.alloced--;
                save_reglocals();
                sb.writef("    call function %\n", l.value.lexeme);
                restore_reglocals();
                if(hack) regallocator.alloced++;
                called = true;
            }
        }
        if(!called){
            int before = regallocator.alloced;
            auto func = regallocator.allocate();
            int res = expr.callee.accept(this, func);
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
            auto arg = expr.args[i].ungroup;
            int before = regallocator.alloced;
            int res = arg.accept(this, rarg1+i);
            if(res != 0) return res;
            regallocator.reset_to(before);
        }
        bool called = false;
        // this is wrong but whatever
        if(expr.callee.type == ExprType.VARIABLE){
            auto l = cast(Literal*)expr.callee;
            if(l.value.type == TokenType.IDENTIFIER){
                if(analysis.vars_on_stack)
                    restore_stack();
                sb.writef("    tail_call function %\n", l.value.lexeme);
                called = true;
            }
        }
        if(!called){
            int before = regallocator.alloced;
            auto func = regallocator.allocate();
            int res = expr.callee.accept(this, func);
            if(res != 0) return res;
            regallocator.reset_to(before);
            if(analysis.vars_on_stack)
                restore_stack();
            sb.writef("    tail_call r%\n", func);
        }
        return 0;
    }
    int visit(Grouping* expr, int target){
        int res = expr.expression.accept(this, target);
        if(res != 0) return res;
        return 0;
    }
    int visit(VarExpr* expr, int target){
        if(target == TARGET_IS_NOTHING) return 0;
        if(auto rlocal = expr.name.lexeme in reglocals){
            if(target == *rlocal){
                // loading into the same register
                return 0;
            }
            sb.writef("    move r% r%\n", target, *rlocal);
            return 0;
        }
        if(auto local = expr.name.lexeme in locals){
            sb.writef("    local_read r% %\n", target, P(*local));
            return 0;
        }
        // must be a global.
        sb.writef("    read r% var %\n", target, expr.name.lexeme);
        return 0;
    }
    int visit(Assign* expr, int target){
        // ignore target;
        if(auto rlocal = expr.name.lexeme in reglocals){
            if(expr.right.type == ExprType.LITERAL){
                auto lit = cast(Literal*)expr.right;
                switch(lit.value.type)with(TokenType){
                    case NIL:
                    case FALSE:
                        sb.writef("    move r% 0\n", *rlocal);
                        break;
                    case TRUE:
                        sb.writef("    move r% 1\n", *rlocal);
                        break;
                    case STRING:
                        sb.writef("    move r% \"%\"\n", *rlocal, lit.value.string_);
                        break;
                    case NUMBER:
                        sb.writef("    move r% %\n", *rlocal, cast(size_t)lit.value.number);
                        break;
                    case BNUM:
                    case SNUM:
                    case PHEX:
                    case HEX:
                        sb.writef("    move r% %\n", *rlocal, lit.value.string_);
                        break;
                    default:
                        error(lit.value, "Unhandled literal type in assign");
                        return -1;
                }
            }
            else {

                int before = regallocator.alloced;
                int temp = regallocator.allocate();
                int res = expr.right.accept(this, temp);
                if(res != 0) return res;
                regallocator.reset_to(before);
                sb.writef("    move r% r%\n", *rlocal, temp);
            }
            return 0;
        }
        if(auto local  = expr.name.lexeme in locals){
            if(expr.right.type == ExprType.LITERAL){
                auto lit = cast(Literal*)expr.right;
                switch(lit.value.type)with(TokenType){
                    case NIL:
                    case FALSE:
                        sb.writef("    local_write % 0\n", P(*local));
                        break;
                    case TRUE:
                        sb.writef("    local_write % 1\n", P(*local));
                        break;
                    case STRING:
                        sb.writef("    local_write % \"%\"\n", P(*local), lit.value.string_);
                        break;
                    case NUMBER:
                        sb.writef("    local_write % %\n", P(*local), cast(size_t)lit.value.number);
                        break;
                    default:
                        error(lit.value, "Unhandled literal type in assign");
                        return -1;
                }
            }
            else {
                int before = regallocator.alloced;
                int temp = regallocator.allocate();
                int res = expr.right.accept(this, temp);
                if(res != 0) return res;
                regallocator.reset_to(before);
                sb.writef("    local_write % r%\n", P(*local), temp);
            }
            return 0;
        }
        // must be a global.
        if(expr.right.type == ExprType.LITERAL){
            auto lit = cast(Literal*)expr.right;
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
                    sb.writef("    write r% \"%\"\n", temp, lit.value.string_);
                    break;
                case NUMBER:
                    sb.writef("    write r% %\n", temp, cast(size_t)lit.value.number);
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
            int res = expr.right.accept(this, rhs);
            if(res != 0) return res;
            regallocator.reset_to(before);
            sb.writef("    move r% var %\n", lhs, expr.name.lexeme);
            sb.writef("    write r% r%\n", lhs, rhs);
        }
        return 0;
    }
    int visit(Logical* expr, int target){
        if(target == TARGET_IS_NOTHING){
        }
        error(expr.operator, "Unhandled logical operator");
        return -1;
    }
    int visit(ExpressionStmt* stmt){
        if(!funcdepth){
            error(0, "Expression outside of function");
            return -1;
        }
        int before = regallocator.alloced;
        int result = stmt.expr.accept(this, TARGET_IS_NOTHING);
        regallocator.reset_to(before);
        return result;
    }
    int visit(LetStmt* stmt){
        if(!funcdepth) {
            // global variable;
            if(stmt.initializer.type != ExprType.LITERAL){
                error(stmt.name, "Non constant initializer for global variable");
                return -1;
            }
            auto lit = cast(Literal*)stmt.initializer;
            switch(lit.value.type) with(TokenType){
                case FALSE:
                case NIL:
                    sb.writef("var % 0\n", stmt.name.lexeme);
                    return 0;
                case TRUE:
                    sb.writef("var % 1\n", stmt.name.lexeme);
                    return 0;
                case STRING:
                    sb.writef("var % \"%\"\n", stmt.name.lexeme, lit.value.string_);
                    return 0;
                case NUMBER:
                    sb.writef("var % %\n", stmt.name.lexeme, lit.value.number);
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
        int res = (&assign.exp).accept(this, 0);
        return res;
        static if(0){
        if(auto rlocal = stmt.name.lexeme in reglocals){
            int res = stmt.initializer.accept(this, *rlocal);
            if(res != 0) return res;
            return 0;
        }
        if(auto local = stmt.name.lexeme in locals){
            int before = regallocator.alloced;
            int temp = regallocator.allocate();
            int res = stmt.initializer.accept(this, temp);
            if(res != 0) return res;
            regallocator.reset_to(before);
            sb.writef("    local_write % r%\n", P(*local), temp);
            return 0;
        }
        error(stmt.name, "Unhandled var stmt");
        return -1;
        }
    }
    int visit(Block* stmt){
        if(!funcdepth) {
            error(0, "Block outside of function");
            return -1;
        }
        foreach(s; stmt.statements){
            int res = s.accept(this);
            if(res != 0) return res;
        }
        return 0;
    }
    int visit(IfStmt* stmt){
        if(!funcdepth) {
            error(0, "If outside of function");
            return -1;
        }
        int label = labelallocator.allocate();
        if(stmt.condition.type == ExprType.BINARY){
            Binary* b = cast(Binary*)stmt.condition;
            switch(b.operator.type)with(TokenType){
                case GREATER:
                case EQUAL_EQUAL:
                case GREATER_EQUAL:
                case LESS_EQUAL:
                case LESS:
                case BANG_EQUAL:
                    int res = stmt.condition.accept(this, TARGET_IS_CMP_FLAGS);
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
        else {
            Lgeneric:
            int before = regallocator.alloced;
            int cond = regallocator.allocate();
            scope(exit) regallocator.reset_to(before);
            int res = stmt.condition.accept(this, cond);
            if(res != 0) return res;
            sb.writef("    cmp r% 0\n", cond);
            sb.writef("    jump eq label L%\n", label);
        }
        int res = stmt.thenBranch.accept(this);
        if(res != 0) return res;
        if(stmt.elseBranch){
            int after_else_label = labelallocator.allocate();
            sb.writef("    move rip label L%\n", after_else_label);
            sb.writef("  label L%\n", label);
            int r = stmt.elseBranch.accept(this);
            if(r != 0) return r;
            sb.writef("  label L%\n", after_else_label);
        }
        else {
            sb.writef("  label L%\n", label);
        }
        return 0;
    }
    int visit(FuncStatement* stmt){
        if(funcdepth){
            error(stmt.name, "Nested function");
            return -1;
        }
        scope analyzer = new DasmAnalyzer!(typeof(allocator))(allocator);
        analyzer.visit(stmt);
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
            auto r = regallocator.allocate();
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
                auto r = regallocator.allocate();
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
                auto s = stackallocator.allocate();
                locals[v.lexeme] = cast(int)s;
                nvars++;
            }
            sb.writef("    add rsp rsp %\n", P(nvars));
        }
        foreach(s; stmt.body){
            int res = s.accept(this);
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
    int visit(ImportStatement* stmt){
        sb.writef("import %\n", stmt.name.lexeme);
        return 0;
    }
    int visit(HaltStatement* stmt){
        sb.writef("    halt\n");
        return 0;
    }
    int visit(AbortStatement* stmt){
        sb.writef("    abort\n");
        return 0;
    }
    int visit(ReturnStatement* stmt){
        if(!funcdepth) {
            error(stmt.keyword, "Return outside of function");
            return -1;
        }
        if(stmt.value.type == ExprType.CALL){
            return do_tail_call(cast(Call*)stmt.value);
        }
        if(stmt.value !is &NilExpr_.exp){
            int before = regallocator.alloced;
            int temp = regallocator.allocate();
            int rout = RegisterNames.ROUT1;
            int res = stmt.value.accept(this, temp);
            if(res != 0) return res;
            regallocator.reset_to(before);
            sb.writef("    move r% r%\n", rout, temp);
        }
        if(analysis.vars_on_stack)
            restore_stack();
        sb.write("    ret\n");
        return 0;
    }
    int visit(WhileStatement* stmt){
        if(!funcdepth) {
            error(0, "`while` outside of function");
            return -1;
        }
        int top = labelallocator.allocate();
        // after the loop
        int after = labelallocator.allocate();
        // TODO: abstract over this instead of this
        // copy paste silliness?
        if(stmt.condition.type == ExprType.LITERAL){
            auto lit = cast(Literal*)stmt.condition;
            switch(lit.value.type)with(TokenType){
                case TRUE:
                    sb.writef("  label L%\n", top);
                    break;
                case FALSE:
                    labelallocator.nalloced = top-1;
                    return 0;
                case NIL:
                    labelallocator.nalloced = top-1;
                    return 0;
                case NUMBER:
                    if(lit.value.number){
                        sb.writef("  label L%\n", top);
                        break;
                    }
                    labelallocator.nalloced = top-1;
                    return 0;
                default:
                    error(lit.value, "Unhandled literal type for while condition");
                    return -1;
            }
        }
        else if(stmt.condition.type == ExprType.BINARY){
            sb.writef("  label L%\n", top);
            Binary* b = cast(Binary*)stmt.condition;
            switch(b.operator.type)with(TokenType){
                case GREATER:
                case EQUAL_EQUAL:
                case GREATER_EQUAL:
                case LESS_EQUAL:
                case LESS:
                case BANG_EQUAL:
                    int res = stmt.condition.accept(this, TARGET_IS_CMP_FLAGS);
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
            sb.writef("    label L%\n", top);
            Lgeneric:
            int before = regallocator.alloced;
            int cond = regallocator.allocate();
            scope(exit) regallocator.reset_to(before);
            int res = stmt.condition.accept(this, cond);
            if(res != 0) return res;
            sb.writef("    cmp r% 0\n", cond);
            sb.writef("    jump eq label L%\n", after);
        }
        int res = stmt.statement.accept(this);
        if(res != 0) return res;
        sb.writef("    move rip label L%\n", top);
        sb.writef("  label L%\n", after);
        return 0;
    }
    int do_it(Statement*[] stmts){
        foreach(s; stmts){
            int res = s.accept(this);
            if(res != 0) return res;
        }
        return 0;
    }
    int visit(GotoStatement* stmt){
        sb.writef("    move rip label L%\n", stmt.label.lexeme);
        return 0;
    }
    int visit(LabelStatement* stmt){
        sb.writef("  label L%\n", stmt.label.lexeme);
        return 0;
    }
}

