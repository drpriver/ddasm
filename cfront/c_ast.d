/*
 * C Front-End AST and Type System for ddasm
 * Copyright 2025, David Priver
 */
module cfront.c_ast;

import core.stdc.stdio : fprintf, stderr;
import dlib.aliases;
import dlib.allocator : Allocator;

import cfront.c_tokenizer : CToken, CTokenType;

// =============================================================================
// Type System
// =============================================================================

enum CTypeKind {
    VOID,
    CHAR,
    SHORT,
    INT,
    LONG,
    FLOAT,
    DOUBLE,
    POINTER,
    ARRAY,
    FUNCTION,
    STRUCT,
    UNION,
    ENUM,
}

// Struct field definition
struct StructField {
    str name;
    CType* type;
    size_t offset;  // Byte offset from start of struct
}

struct CType {
    CTypeKind kind;
    bool is_unsigned;
    bool is_const;
    CType* pointed_to;     // For pointers and arrays
    CType* return_type;    // For functions
    CType*[] param_types;  // For functions
    bool is_varargs;       // For functions
    size_t array_size;     // For arrays
    // Struct fields
    str struct_name;       // For named structs
    StructField[] fields;  // For structs
    size_t struct_size;    // Cached total size of struct

    // Get size in bytes (for pointer arithmetic)
    size_t size_of() {
        final switch (kind) {
            case CTypeKind.VOID:     return 0;
            case CTypeKind.CHAR:     return 1;
            case CTypeKind.SHORT:    return 2;
            case CTypeKind.INT:      return 4;
            case CTypeKind.LONG:     return 8;
            case CTypeKind.FLOAT:    return 4;
            case CTypeKind.DOUBLE:   return 8;
            case CTypeKind.POINTER:  return 8;  // 64-bit
            case CTypeKind.ARRAY:    return pointed_to ? pointed_to.size_of() * array_size : 0;
            case CTypeKind.FUNCTION: return 8;  // Function pointer size
            case CTypeKind.STRUCT:   return struct_size;
            case CTypeKind.UNION:    return struct_size;  // Union size is max of all members
            case CTypeKind.ENUM:     return 4;  // Enums are ints
        }
    }

    // Get element size for pointer arithmetic
    size_t element_size() {
        if ((kind == CTypeKind.POINTER || kind == CTypeKind.ARRAY) && pointed_to) {
            size_t s = pointed_to.size_of();
            return s ? s : 1;  // void* arithmetic uses size 1
        }
        return 1;
    }

    bool is_void() { return kind == CTypeKind.VOID; }
    bool is_pointer() { return kind == CTypeKind.POINTER; }
    bool is_array() { return kind == CTypeKind.ARRAY; }
    bool is_struct() { return kind == CTypeKind.STRUCT; }
    bool is_union() { return kind == CTypeKind.UNION; }
    bool is_struct_or_union() { return kind == CTypeKind.STRUCT || kind == CTypeKind.UNION; }
    bool is_enum() { return kind == CTypeKind.ENUM; }
    bool is_integer() {
        return kind == CTypeKind.CHAR || kind == CTypeKind.SHORT ||
               kind == CTypeKind.INT || kind == CTypeKind.LONG ||
               kind == CTypeKind.ENUM;  // Enums are integer-compatible
    }
    bool is_float() {
        return kind == CTypeKind.FLOAT || kind == CTypeKind.DOUBLE;
    }
    bool is_arithmetic() {
        return is_integer() || is_float();
    }

    // Get a struct/union field by name
    StructField* get_field(str name) {
        if (kind != CTypeKind.STRUCT && kind != CTypeKind.UNION) return null;
        foreach (ref f; fields) {
            if (f.name == name) return &f;
        }
        return null;
    }

    // Get element type for arrays/pointers
    CType* element_type() {
        if (kind == CTypeKind.ARRAY || kind == CTypeKind.POINTER)
            return pointed_to;
        return null;
    }

    // Number of stack slots needed for this type (for local allocation)
    size_t stack_slots() {
        if (kind == CTypeKind.ARRAY) {
            // Array needs one slot per element
            return array_size;
        }
        if (kind == CTypeKind.STRUCT || kind == CTypeKind.UNION) {
            // Struct/union needs enough slots for its size (8 bytes per slot)
            return (struct_size + 7) / 8;
        }
        return 1;  // Everything else is one slot (pointer-sized)
    }
}

// Pre-allocated common types
__gshared CType TYPE_VOID = { kind: CTypeKind.VOID };
__gshared CType TYPE_CHAR = { kind: CTypeKind.CHAR };
__gshared CType TYPE_INT = { kind: CTypeKind.INT };
__gshared CType TYPE_LONG = { kind: CTypeKind.LONG };
__gshared CType TYPE_FLOAT = { kind: CTypeKind.FLOAT };
__gshared CType TYPE_DOUBLE = { kind: CTypeKind.DOUBLE };
__gshared CType TYPE_UCHAR = { kind: CTypeKind.CHAR, is_unsigned: true };
__gshared CType TYPE_UINT = { kind: CTypeKind.INT, is_unsigned: true };
__gshared CType TYPE_ULONG = { kind: CTypeKind.LONG, is_unsigned: true };

// Pointer types
__gshared CType TYPE_VOID_PTR = { kind: CTypeKind.POINTER, pointed_to: &TYPE_VOID };
__gshared CType TYPE_CHAR_PTR = { kind: CTypeKind.POINTER, pointed_to: &TYPE_CHAR };
__gshared CType TYPE_INT_PTR = { kind: CTypeKind.POINTER, pointed_to: &TYPE_INT };
__gshared CType TYPE_LONG_PTR = { kind: CTypeKind.POINTER, pointed_to: &TYPE_LONG };

CType* make_pointer_type(Allocator a, CType* base) {
    auto data = a.alloc(CType.sizeof);
    auto result = cast(CType*)data.ptr;
    result.kind = CTypeKind.POINTER;
    result.pointed_to = base;
    return result;
}

CType* make_array_type(Allocator a, CType* element_type, size_t size) {
    auto data = a.alloc(CType.sizeof);
    auto result = cast(CType*)data.ptr;
    result.kind = CTypeKind.ARRAY;
    result.pointed_to = element_type;  // Element type stored in pointed_to
    result.array_size = size;
    return result;
}

CType* make_struct_type(Allocator a, str name, StructField[] fields, size_t total_size) {
    auto data = a.alloc(CType.sizeof);
    auto result = cast(CType*)data.ptr;
    result.kind = CTypeKind.STRUCT;
    result.struct_name = name;
    result.fields = fields;
    result.struct_size = total_size;
    return result;
}

CType* make_union_type(Allocator a, str name, StructField[] fields, size_t total_size) {
    auto data = a.alloc(CType.sizeof);
    auto result = cast(CType*)data.ptr;
    result.kind = CTypeKind.UNION;
    result.struct_name = name;
    result.fields = fields;  // All fields have offset 0 in a union
    result.struct_size = total_size;
    return result;
}

CType* make_enum_type(Allocator a, str name) {
    auto data = a.alloc(CType.sizeof);
    auto result = cast(CType*)data.ptr;
    result.kind = CTypeKind.ENUM;
    result.struct_name = name;  // Reuse struct_name for enum name
    return result;
}

CType* make_function_type(Allocator a, CType* ret_type, CType*[] param_types, bool is_varargs) {
    auto data = a.alloc(CType.sizeof);
    auto result = cast(CType*)data.ptr;
    result.kind = CTypeKind.FUNCTION;
    result.return_type = ret_type;
    result.param_types = param_types;
    result.is_varargs = is_varargs;
    return result;
}

// Enum constant (name -> value)
struct EnumConstant {
    str name;
    long value;
}

// =============================================================================
// Expressions
// =============================================================================

enum CExprKind {
    LITERAL,
    IDENTIFIER,
    BINARY,
    UNARY,
    CALL,
    CAST,
    SUBSCRIPT,
    MEMBER_ACCESS,
    ASSIGN,
    SIZEOF,
    GROUPING,
}

struct CExpr {
    CExprKind kind;
    CType* type;  // Resolved type (set during analysis/codegen)
    CToken token; // For error reporting

    inout(CLiteral)* as_literal() inout {
        return kind == CExprKind.LITERAL ? cast(typeof(return))&this : null;
    }
    inout(CIdentifier)* as_identifier() inout {
        return kind == CExprKind.IDENTIFIER ? cast(typeof(return))&this : null;
    }
    inout(CBinary)* as_binary() inout {
        return kind == CExprKind.BINARY ? cast(typeof(return))&this : null;
    }
    inout(CUnary)* as_unary() inout {
        return kind == CExprKind.UNARY ? cast(typeof(return))&this : null;
    }
    inout(CCall)* as_call() inout {
        return kind == CExprKind.CALL ? cast(typeof(return))&this : null;
    }
    inout(CAssign)* as_assign() inout {
        return kind == CExprKind.ASSIGN ? cast(typeof(return))&this : null;
    }
    inout(CGrouping)* as_grouping() inout {
        return kind == CExprKind.GROUPING ? cast(typeof(return))&this : null;
    }
    inout(CCast)* as_cast() inout {
        return kind == CExprKind.CAST ? cast(typeof(return))&this : null;
    }
    inout(CSubscript)* as_subscript() inout {
        return kind == CExprKind.SUBSCRIPT ? cast(typeof(return))&this : null;
    }
    inout(CMemberAccess)* as_member_access() inout {
        return kind == CExprKind.MEMBER_ACCESS ? cast(typeof(return))&this : null;
    }

    CExpr* ungroup() {
        if (kind == CExprKind.GROUPING) {
            return (cast(CGrouping*)&this).expression.ungroup();
        }
        return &this;
    }
}

struct CLiteral {
    CExpr expr;
    CToken value;

    static CExpr* make(Allocator a, CToken t, CType* type) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.LITERAL;
        result.expr.token = t;
        result.expr.type = type;
        result.value = t;
        return &result.expr;
    }
}

struct CSizeof {
    CExpr expr;
    CType* sizeof_type;   // If sizeof(type)
    CExpr* sizeof_expr;   // If sizeof expr
    size_t size;          // Precomputed size (if known)

    static CExpr* make(Allocator a, CType* t, size_t sz, CToken tok) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.SIZEOF;
        result.expr.token = tok;
        result.expr.type = &TYPE_LONG;  // sizeof returns size_t (use long)
        result.sizeof_type = t;
        result.sizeof_expr = null;
        result.size = sz;
        return &result.expr;
    }

    static CExpr* make_expr(Allocator a, CExpr* e, CToken tok) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.SIZEOF;
        result.expr.token = tok;
        result.expr.type = &TYPE_LONG;
        result.sizeof_type = null;
        result.sizeof_expr = e;
        result.size = 0;  // Will be computed during codegen
        return &result.expr;
    }
}

struct CIdentifier {
    CExpr expr;
    CToken name;

    static CExpr* make(Allocator a, CToken t) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.IDENTIFIER;
        result.expr.token = t;
        result.name = t;
        return &result.expr;
    }
}

struct CBinary {
    CExpr expr;
    CExpr* left_;
    CTokenType op;
    CExpr* right_;

    CExpr* left() { return left_.ungroup(); }
    CExpr* right() { return right_.ungroup(); }

    static CExpr* make(Allocator a, CExpr* l, CTokenType op, CExpr* r, CToken t) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.BINARY;
        result.expr.token = t;
        result.left_ = l;
        result.op = op;
        result.right_ = r;
        return &result.expr;
    }
}

struct CUnary {
    CExpr expr;
    CTokenType op;
    CExpr* operand_;
    bool is_prefix;  // true for prefix ops like *p, &x, -x; false for postfix like p++

    CExpr* operand() { return operand_.ungroup(); }

    static CExpr* make(Allocator a, CTokenType op, CExpr* operand, bool prefix, CToken t) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.UNARY;
        result.expr.token = t;
        result.op = op;
        result.operand_ = operand;
        result.is_prefix = prefix;
        return &result.expr;
    }
}

struct CCall {
    CExpr expr;
    CExpr* callee_;
    CExpr*[] args;

    CExpr* callee() { return callee_.ungroup(); }

    static CExpr* make(Allocator a, CExpr* callee, CExpr*[] args, CToken t) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.CALL;
        result.expr.token = t;
        result.callee_ = callee;
        result.args = args;
        return &result.expr;
    }
}

struct CAssign {
    CExpr expr;
    CExpr* target_;
    CTokenType op;  // EQUAL, PLUS_EQUAL, etc.
    CExpr* value_;

    CExpr* target() { return target_.ungroup(); }
    CExpr* value() { return value_.ungroup(); }

    static CExpr* make(Allocator a, CExpr* target, CTokenType op, CExpr* value, CToken t) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.ASSIGN;
        result.expr.token = t;
        result.target_ = target;
        result.op = op;
        result.value_ = value;
        return &result.expr;
    }
}

struct CGrouping {
    CExpr expr;
    CExpr* expression;

    static CExpr* make(Allocator a, CExpr* e, CToken t) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.GROUPING;
        result.expr.token = t;
        result.expression = e;
        return &result.expr;
    }
}

struct CCast {
    CExpr expr;
    CType* cast_type;
    CExpr* operand_;

    CExpr* operand() { return operand_.ungroup(); }

    static CExpr* make(Allocator a, CType* t, CExpr* e, CToken tok) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.CAST;
        result.expr.token = tok;
        result.expr.type = t;
        result.cast_type = t;
        result.operand_ = e;
        return &result.expr;
    }
}

struct CSubscript {
    CExpr expr;
    CExpr* array_;
    CExpr* index_;

    CExpr* array() { return array_.ungroup(); }
    CExpr* index() { return index_.ungroup(); }

    static CExpr* make(Allocator a, CExpr* arr, CExpr* idx, CToken t) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.SUBSCRIPT;
        result.expr.token = t;
        result.array_ = arr;
        result.index_ = idx;
        return &result.expr;
    }
}

struct CMemberAccess {
    CExpr expr;
    CExpr* object_;
    CToken member;
    bool is_arrow;  // true for ->, false for .

    CExpr* object() { return object_.ungroup(); }

    static CExpr* make(Allocator a, CExpr* obj, CToken member_, bool arrow, CToken t) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.MEMBER_ACCESS;
        result.expr.token = t;
        result.object_ = obj;
        result.member = member_;
        result.is_arrow = arrow;
        return &result.expr;
    }
}

// =============================================================================
// Statements
// =============================================================================

enum CStmtKind {
    EXPR,
    RETURN,
    IF,
    WHILE,
    FOR,
    BLOCK,
    VAR_DECL,
    BREAK,
    CONTINUE,
    EMPTY,
}

struct CStmt {
    CStmtKind kind;
    CToken token;  // For error reporting
}

struct CExprStmt {
    CStmt stmt;
    CExpr* expression;

    static CStmt* make(Allocator a, CExpr* e, CToken t) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.stmt.kind = CStmtKind.EXPR;
        result.stmt.token = t;
        result.expression = e;
        return &result.stmt;
    }
}

struct CReturnStmt {
    CStmt stmt;
    CExpr* value;  // null for bare return

    static CStmt* make(Allocator a, CExpr* v, CToken t) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.stmt.kind = CStmtKind.RETURN;
        result.stmt.token = t;
        result.value = v;
        return &result.stmt;
    }
}

struct CIfStmt {
    CStmt stmt;
    CExpr* condition;
    CStmt* then_branch;
    CStmt* else_branch;  // null if no else

    static CStmt* make(Allocator a, CExpr* cond, CStmt* then_, CStmt* else_, CToken t) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.stmt.kind = CStmtKind.IF;
        result.stmt.token = t;
        result.condition = cond;
        result.then_branch = then_;
        result.else_branch = else_;
        return &result.stmt;
    }
}

struct CWhileStmt {
    CStmt stmt;
    CExpr* condition;
    CStmt* body;

    static CStmt* make(Allocator a, CExpr* cond, CStmt* body_, CToken t) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.stmt.kind = CStmtKind.WHILE;
        result.stmt.token = t;
        result.condition = cond;
        result.body = body_;
        return &result.stmt;
    }
}

struct CForStmt {
    CStmt stmt;
    CStmt* init_stmt;   // null, VarDecl, or ExprStmt
    CExpr* condition;   // null for infinite loop
    CExpr* increment;   // null if none
    CStmt* body_;

    static CStmt* make(Allocator a, CStmt* init_, CExpr* cond, CExpr* incr, CStmt* body__, CToken t) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.stmt.kind = CStmtKind.FOR;
        result.stmt.token = t;
        result.init_stmt = init_;
        result.condition = cond;
        result.increment = incr;
        result.body_ = body__;
        return &result.stmt;
    }
}

struct CBlock {
    CStmt stmt;
    CStmt*[] statements;

    static CStmt* make(Allocator a, CStmt*[] stmts, CToken t) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.stmt.kind = CStmtKind.BLOCK;
        result.stmt.token = t;
        result.statements = stmts;
        return &result.stmt;
    }
}

struct CVarDecl {
    CStmt stmt;
    CType* var_type;
    CToken name;
    CExpr* initializer;  // null if uninitialized

    static CStmt* make(Allocator a, CType* type, CToken name_, CExpr* init, CToken t) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.stmt.kind = CStmtKind.VAR_DECL;
        result.stmt.token = t;
        result.var_type = type;
        result.name = name_;
        result.initializer = init;
        return &result.stmt;
    }
}

struct CBreakStmt {
    CStmt stmt;

    static CStmt* make(Allocator a, CToken t) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.stmt.kind = CStmtKind.BREAK;
        result.stmt.token = t;
        return &result.stmt;
    }
}

struct CContinueStmt {
    CStmt stmt;

    static CStmt* make(Allocator a, CToken t) {
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.stmt.kind = CStmtKind.CONTINUE;
        result.stmt.token = t;
        return &result.stmt;
    }
}

struct CEmptyStmt {
    CStmt stmt;

    __gshared CEmptyStmt singleton = { stmt: { kind: CStmtKind.EMPTY } };

    static CStmt* get() {
        return &singleton.stmt;
    }
}

// =============================================================================
// Top-Level Declarations
// =============================================================================

struct CParam {
    CType* type;
    CToken name;
}

struct CFunction {
    CToken name;
    CType* return_type;
    CParam[] params;
    bool is_varargs;
    CStmt*[] body;  // Empty for declarations
    bool is_definition;  // true if has body
    bool is_extern;      // extern declaration
}

struct CExternDecl {
    CToken name;
    CType* return_type;
    CParam[] params;
    bool is_varargs;
    str library;  // From #pragma library
}

struct CGlobalVar {
    CToken name;
    CType* var_type;
    CExpr* initializer;  // null if uninitialized (will be zero-initialized)
}

struct CStructDef {
    CToken name;
    CType* struct_type;  // The fully defined struct type
}

struct CUnionDef {
    CToken name;
    CType* union_type;  // The fully defined union type
}

struct CEnumDef {
    CToken name;
    CType* enum_type;          // The enum type
    EnumConstant[] constants;  // The enum constants (name -> value)
}

struct CTranslationUnit {
    CFunction[] functions;
    CExternDecl[] externs;
    CGlobalVar[] globals;
    CStructDef[] structs;
    CUnionDef[] unions;
    CEnumDef[] enums;
    str current_library;  // Set by #pragma library
}
