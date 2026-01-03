/*
 * AST Printer for C Front-End
 * Copyright 2025, David Priver
 */
module cfront.c_ast_printer;

import core.stdc.stdio : fprintf, stderr;
import dlib.aliases : str;
import cfront.c_ast;
import cfront.c_pp_to_c : CToken, CTokenType;

void print_ast(CTranslationUnit* unit) {
    fprintf(stderr, "=== AST Dump ===\n");

    if (unit.structs.length > 0) {
        fprintf(stderr, "\nStructs (%zu):\n", unit.structs.length);
        foreach (ref s; unit.structs) {
            fprintf(stderr, "  struct %.*s\n", cast(int)s.name.lexeme.length, s.name.lexeme.ptr);
        }
    }

    if (unit.unions.length > 0) {
        fprintf(stderr, "\nUnions (%zu):\n", unit.unions.length);
        foreach (ref u; unit.unions) {
            fprintf(stderr, "  union %.*s\n", cast(int)u.name.lexeme.length, u.name.lexeme.ptr);
        }
    }

    if (unit.enums.length > 0) {
        fprintf(stderr, "\nEnums (%zu):\n", unit.enums.length);
        foreach (ref e; unit.enums) {
            fprintf(stderr, "  enum %.*s\n", cast(int)e.name.lexeme.length, e.name.lexeme.ptr);
        }
    }

    if (unit.globals.length > 0) {
        fprintf(stderr, "\nGlobals (%zu):\n", unit.globals.length);
        foreach (ref g; unit.globals) {
            fprintf(stderr, "  ");
            print_type(g.var_type);
            fprintf(stderr, " %.*s", cast(int)g.name.lexeme.length, g.name.lexeme.ptr);
            if (g.initializer !is null) {
                fprintf(stderr, " = ");
                print_expr(g.initializer, 0);
            }
            fprintf(stderr, "\n");
        }
    }

    if (unit.functions.length > 0) {
        fprintf(stderr, "\nFunctions (%zu):\n", unit.functions.length);
        foreach (ref f; unit.functions) {
            print_function(&f);
        }
    }
}

void print_function(CFunction* f) {
    fprintf(stderr, "\n");
    if (f.is_static) fprintf(stderr, "static ");
    if (f.is_inline) fprintf(stderr, "inline ");
    print_type(f.return_type);
    fprintf(stderr, " %.*s(", cast(int)f.name.lexeme.length, f.name.lexeme.ptr);

    foreach (i, ref p; f.params) {
        if (i > 0) fprintf(stderr, ", ");
        print_type(p.type);
        if (p.name.lexeme.length > 0) {
            fprintf(stderr, " %.*s", cast(int)p.name.lexeme.length, p.name.lexeme.ptr);
        }
    }
    if (f.is_varargs) {
        if (f.params.length > 0) fprintf(stderr, ", ");
        fprintf(stderr, "...");
    }
    fprintf(stderr, ")");

    if (f.is_definition) {
        fprintf(stderr, " {\n");
        foreach (s; f.body) {
            print_stmt(s, 1);
        }
        fprintf(stderr, "}\n");
    } else {
        fprintf(stderr, ";\n");
    }
}

void print_indent(int level) {
    foreach (_; 0 .. level) {
        fprintf(stderr, "  ");
    }
}

void print_stmt(CStmt* s, int indent) {
    if (s is null) return;

    print_indent(indent);

    final switch (s.kind) with (CStmtKind) {
        case EXPR:
            auto es = cast(CExprStmt*)s;
            print_expr(es.expression, indent);
            fprintf(stderr, ";\n");
            break;

        case RETURN:
            auto rs = cast(CReturnStmt*)s;
            fprintf(stderr, "return");
            if (rs.value !is null) {
                fprintf(stderr, " ");
                print_expr(rs.value, indent);
            }
            fprintf(stderr, ";\n");
            break;

        case IF:
            auto is_ = cast(CIfStmt*)s;
            fprintf(stderr, "if (");
            print_expr(is_.condition, indent);
            fprintf(stderr, ")\n");
            print_stmt(is_.then_branch, indent + 1);
            if (is_.else_branch !is null) {
                print_indent(indent);
                fprintf(stderr, "else\n");
                print_stmt(is_.else_branch, indent + 1);
            }
            break;

        case WHILE:
            auto ws = cast(CWhileStmt*)s;
            fprintf(stderr, "while (");
            print_expr(ws.condition, indent);
            fprintf(stderr, ")\n");
            print_stmt(ws.body, indent + 1);
            break;

        case DO_WHILE:
            auto dws = cast(CDoWhileStmt*)s;
            fprintf(stderr, "do\n");
            print_stmt(dws.body, indent + 1);
            print_indent(indent);
            fprintf(stderr, "while (");
            print_expr(dws.condition, indent);
            fprintf(stderr, ");\n");
            break;

        case FOR:
            auto fs = cast(CForStmt*)s;
            fprintf(stderr, "for (");
            if (fs.init_stmt !is null) {
                print_stmt_inline(fs.init_stmt);
            }
            fprintf(stderr, "; ");
            if (fs.condition !is null) {
                print_expr(fs.condition, indent);
            }
            fprintf(stderr, "; ");
            if (fs.increment !is null) {
                print_expr(fs.increment, indent);
            }
            fprintf(stderr, ")\n");
            print_stmt(fs.body_, indent + 1);
            break;

        case BLOCK:
            auto bs = cast(CBlock*)s;
            fprintf(stderr, "{\n");
            foreach (stmt; bs.statements) {
                print_stmt(stmt, indent + 1);
            }
            print_indent(indent);
            fprintf(stderr, "}\n");
            break;

        case VAR_DECL:
            auto vd = cast(CVarDecl*)s;
            print_type(vd.var_type);
            fprintf(stderr, " %.*s", cast(int)vd.name.lexeme.length, vd.name.lexeme.ptr);
            if (vd.initializer !is null) {
                fprintf(stderr, " = ");
                print_expr(vd.initializer, indent);
            }
            fprintf(stderr, ";\n");
            break;

        case BREAK:
            fprintf(stderr, "break;\n");
            break;

        case CONTINUE:
            fprintf(stderr, "continue;\n");
            break;

        case EMPTY:
            fprintf(stderr, ";\n");
            break;

        case SWITCH:
            auto sw = cast(CSwitchStmt*)s;
            fprintf(stderr, "switch (");
            print_expr(sw.condition, indent);
            fprintf(stderr, ")\n");
            print_stmt(sw.body_, indent + 1);
            break;

        case GOTO:
            auto gs = cast(CGotoStmt*)s;
            fprintf(stderr, "goto %.*s;\n", cast(int)gs.label.lexeme.length, gs.label.lexeme.ptr);
            break;

        case LABEL:
            auto ls = cast(CLabelStmt*)s;
            fprintf(stderr, "%.*s:\n", cast(int)ls.label.lexeme.length, ls.label.lexeme.ptr);
            if (ls.statement !is null) {
                print_stmt(ls.statement, indent);
            }
            break;

        case CASE_LABEL:
            auto cs = cast(CCaseLabelStmt*)s;
            if (cs.is_default) {
                fprintf(stderr, "default:\n");
            } else {
                fprintf(stderr, "case ");
                print_expr(cs.case_value, indent);
                fprintf(stderr, ":\n");
            }
            if (cs.statement !is null) {
                print_stmt(cs.statement, indent + 1);
            }
            break;

        case DASM:
            auto ds = cast(CDasmStmt*)s;
            fprintf(stderr, "__dasm__ { %.*s }\n", cast(int)ds.code.length, ds.code.ptr);
            break;

        case ASM:
            fprintf(stderr, "__asm__(...);\n");
            break;
    }
}

void print_stmt_inline(CStmt* s) {
    if (s is null) return;

    final switch (s.kind) with (CStmtKind) {
        case EXPR:
            auto es = cast(CExprStmt*)s;
            print_expr(es.expression, 0);
            break;

        case VAR_DECL:
            auto vd = cast(CVarDecl*)s;
            print_type(vd.var_type);
            fprintf(stderr, " %.*s", cast(int)vd.name.lexeme.length, vd.name.lexeme.ptr);
            if (vd.initializer !is null) {
                fprintf(stderr, " = ");
                print_expr(vd.initializer, 0);
            }
            break;

        case RETURN, IF, WHILE, DO_WHILE, FOR, BLOCK, BREAK, CONTINUE, EMPTY,
             SWITCH, GOTO, LABEL, CASE_LABEL, DASM, ASM:
            fprintf(stderr, "...");
            break;
    }
}

void print_op(CTokenType op) {
    // Print common operators
    switch (op) {
        case CTokenType.PLUS: fprintf(stderr, "+"); break;
        case CTokenType.MINUS: fprintf(stderr, "-"); break;
        case CTokenType.STAR: fprintf(stderr, "*"); break;
        case CTokenType.SLASH: fprintf(stderr, "/"); break;
        case CTokenType.PERCENT: fprintf(stderr, "%%"); break;
        case CTokenType.AMP: fprintf(stderr, "&"); break;
        case CTokenType.PIPE: fprintf(stderr, "|"); break;
        case CTokenType.CARET: fprintf(stderr, "^"); break;
        case CTokenType.TILDE: fprintf(stderr, "~"); break;
        case CTokenType.BANG: fprintf(stderr, "!"); break;
        case CTokenType.EQUAL: fprintf(stderr, "="); break;
        case CTokenType.EQUAL_EQUAL: fprintf(stderr, "=="); break;
        case CTokenType.BANG_EQUAL: fprintf(stderr, "!="); break;
        case CTokenType.LESS: fprintf(stderr, "<"); break;
        case CTokenType.LESS_EQUAL: fprintf(stderr, "<="); break;
        case CTokenType.GREATER: fprintf(stderr, ">"); break;
        case CTokenType.GREATER_EQUAL: fprintf(stderr, ">="); break;
        case CTokenType.LESS_LESS: fprintf(stderr, "<<"); break;
        case CTokenType.GREATER_GREATER: fprintf(stderr, ">>"); break;
        case CTokenType.AMP_AMP: fprintf(stderr, "&&"); break;
        case CTokenType.PIPE_PIPE: fprintf(stderr, "||"); break;
        case CTokenType.PLUS_PLUS: fprintf(stderr, "++"); break;
        case CTokenType.MINUS_MINUS: fprintf(stderr, "--"); break;
        case CTokenType.PLUS_EQUAL: fprintf(stderr, "+="); break;
        case CTokenType.MINUS_EQUAL: fprintf(stderr, "-="); break;
        case CTokenType.STAR_EQUAL: fprintf(stderr, "*="); break;
        case CTokenType.SLASH_EQUAL: fprintf(stderr, "/="); break;
        case CTokenType.PERCENT_EQUAL: fprintf(stderr, "%%="); break;
        case CTokenType.AMP_EQUAL: fprintf(stderr, "&="); break;
        case CTokenType.PIPE_EQUAL: fprintf(stderr, "|="); break;
        case CTokenType.CARET_EQUAL: fprintf(stderr, "^="); break;
        case CTokenType.LESS_LESS_EQUAL: fprintf(stderr, "<<="); break;
        case CTokenType.GREATER_GREATER_EQUAL: fprintf(stderr, ">>="); break;
        default: fprintf(stderr, "?op?"); break;
    }
}

void print_expr(CExpr* e, int indent) {
    if (e is null) {
        fprintf(stderr, "(null)");
        return;
    }

    final switch (e.kind) with (CExprKind) {
        case LITERAL:
            auto lit = cast(CLiteral*)e;
            fprintf(stderr, "%.*s", cast(int)lit.value.lexeme.length, lit.value.lexeme.ptr);
            break;

        case IDENTIFIER:
            auto id = cast(CIdentifier*)e;
            fprintf(stderr, "%.*s", cast(int)id.name.lexeme.length, id.name.lexeme.ptr);
            break;

        case BINARY:
            auto bin = cast(CBinary*)e;
            fprintf(stderr, "(");
            print_expr(bin.left, indent);
            fprintf(stderr, " ");
            print_op(bin.op);
            fprintf(stderr, " ");
            print_expr(bin.right, indent);
            fprintf(stderr, ")");
            break;

        case UNARY:
            auto un = cast(CUnary*)e;
            if (un.is_prefix) {
                print_op(un.op);
                print_expr(un.operand, indent);
            } else {
                print_expr(un.operand, indent);
                print_op(un.op);
            }
            break;

        case CALL:
            auto call = cast(CCall*)e;
            print_expr(call.callee, indent);
            fprintf(stderr, "(");
            foreach (i, arg; call.args) {
                if (i > 0) fprintf(stderr, ", ");
                print_expr(arg, indent);
            }
            fprintf(stderr, ")");
            break;

        case CAST:
            auto c = cast(CCast*)e;
            fprintf(stderr, "(");
            print_type(c.cast_type);
            fprintf(stderr, ")");
            print_expr(c.operand, indent);
            break;

        case SUBSCRIPT:
            auto sub = cast(CSubscript*)e;
            print_expr(sub.array, indent);
            fprintf(stderr, "[");
            print_expr(sub.index, indent);
            fprintf(stderr, "]");
            break;

        case MEMBER_ACCESS:
            auto ma = cast(CMemberAccess*)e;
            print_expr(ma.object, indent);
            if (ma.is_arrow) {
                fprintf(stderr, "->");
            } else {
                fprintf(stderr, ".");
            }
            fprintf(stderr, "%.*s", cast(int)ma.member.lexeme.length, ma.member.lexeme.ptr);
            break;

        case ASSIGN:
            auto asgn = cast(CAssign*)e;
            print_expr(asgn.target, indent);
            fprintf(stderr, " ");
            print_op(asgn.op);
            fprintf(stderr, " ");
            print_expr(asgn.value, indent);
            break;

        case SIZEOF:
            auto sz = cast(CSizeof*)e;
            fprintf(stderr, "sizeof(");
            if (sz.sizeof_type !is null) {
                print_type(sz.sizeof_type);
            } else if (sz.sizeof_expr !is null) {
                print_expr(sz.sizeof_expr, indent);
            }
            fprintf(stderr, ")");
            break;

        case ALIGNOF:
            auto al = cast(CAlignof*)e;
            fprintf(stderr, "_Alignof(");
            if (al.alignof_type !is null) {
                print_type(al.alignof_type);
            } else if (al.alignof_expr !is null) {
                print_expr(al.alignof_expr, indent);
            }
            fprintf(stderr, ")");
            break;

        case COUNTOF:
            auto cnt = cast(CCountof*)e;
            fprintf(stderr, "_Countof(");
            if (cnt.countof_type !is null) {
                print_type(cnt.countof_type);
            } else if (cnt.countof_expr !is null) {
                print_expr(cnt.countof_expr, indent);
            }
            fprintf(stderr, ")");
            break;

        case VA_ARG:
            auto va = cast(CVaArg*)e;
            fprintf(stderr, "va_arg(");
            print_expr(va.va_list_expr, indent);
            fprintf(stderr, ", ");
            print_type(va.arg_type);
            fprintf(stderr, ")");
            break;

        case GROUPING:
            auto grp = cast(CGrouping*)e;
            fprintf(stderr, "(");
            print_expr(grp.expression, indent);
            fprintf(stderr, ")");
            break;

        case TERNARY:
            auto tern = cast(CTernary*)e;
            fprintf(stderr, "(");
            print_expr(tern.condition, indent);
            fprintf(stderr, " ? ");
            print_expr(tern.if_true, indent);
            fprintf(stderr, " : ");
            print_expr(tern.if_false, indent);
            fprintf(stderr, ")");
            break;

        case INIT_LIST:
            auto il = cast(CInitList*)e;
            fprintf(stderr, "{ ");
            foreach (i, ref elem; il.elements) {
                if (i > 0) fprintf(stderr, ", ");
                print_expr(elem.value, indent);
            }
            fprintf(stderr, " }");
            break;

        case COMPOUND_LITERAL:
            auto cl = cast(CCompoundLiteral*)e;
            fprintf(stderr, "(");
            print_type(cl.literal_type);
            fprintf(stderr, ")");
            print_expr(cl.initializer, indent);
            break;

        case GENERIC:
            fprintf(stderr, "_Generic(...)");
            break;

        case EMBED:
            auto emb = cast(CEmbed*)e;
            fprintf(stderr, "__embed(\"%.*s\", %ld, %ld)",
                cast(int)emb.path.length, emb.path.ptr, emb.offset, emb.length);
            break;

        case STMT_EXPR:
            auto se = cast(CStmtExpr*)e;
            fprintf(stderr, "({ ");
            foreach (stmt; se.statements) {
                print_stmt(stmt, indent + 1);
            }
            if (se.result_expr !is null) {
                print_expr(se.result_expr, indent);
            }
            fprintf(stderr, " })");
            break;
    }
}

void print_type(CType* t) {
    if (t is null) {
        fprintf(stderr, "(null)");
        return;
    }

    if (t.is_const) fprintf(stderr, "const ");

    final switch (t.kind) with (CTypeKind) {
        case VOID: fprintf(stderr, "void"); break;
        case CHAR:
            if (t.is_unsigned) fprintf(stderr, "unsigned ");
            fprintf(stderr, "char");
            break;
        case SHORT:
            if (t.is_unsigned) fprintf(stderr, "unsigned ");
            fprintf(stderr, "short");
            break;
        case INT:
            if (t.is_unsigned) fprintf(stderr, "unsigned ");
            fprintf(stderr, "int");
            break;
        case LONG:
            if (t.is_unsigned) fprintf(stderr, "unsigned ");
            fprintf(stderr, "long");
            break;
        case INT128:
            if (t.is_unsigned) fprintf(stderr, "unsigned ");
            fprintf(stderr, "__int128");
            break;
        case FLOAT: fprintf(stderr, "float"); break;
        case DOUBLE: fprintf(stderr, "double"); break;
        case LONG_DOUBLE: fprintf(stderr, "long double"); break;
        case POINTER:
            print_type(t.pointed_to);
            fprintf(stderr, "*");
            break;
        case ARRAY:
            print_type(t.pointed_to);
            fprintf(stderr, "[%zu]", t.array_size);
            break;
        case FUNCTION:
            print_type(t.return_type);
            fprintf(stderr, "(*)()");
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
    }
}
