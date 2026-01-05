/*
 * C Front-End AST and Type System for ddasm
 * Copyright 2025, David Priver
 */
module cfront.c_ast;

import core.stdc.stdio : fprintf, stderr;
import dlib.aliases;
import dlib.allocator : Allocator;

import cfront.c_pp_to_c : CToken, CTokenType;

// Detect LP64 (Unix: long is 64 bits) vs LLP64 (Windows: long is 32 bits)
version(Windows){
    version(X86_64){
        enum IS_64BIT = true;
        enum IS_LP64 = false;  // Windows LLP64: long is 32 bits
    } else {
        enum IS_64BIT = false;
        enum IS_LP64 = false;
    }
} else {
    version(AArch64){
        enum IS_64BIT = true;
        enum IS_LP64 = true;
    } else version(X86_64){
        enum IS_64BIT = true;
        enum IS_LP64 = true;
    } else {
        enum IS_64BIT = false;
        enum IS_LP64 = false;
    }
}

// =============================================================================
// Type System
// =============================================================================

enum CTypeKind {
    UNSET,
    VOID,
    CHAR,
    SHORT,
    INT,
    LONG,
    LONG_LONG,
    INT128,
    FLOAT,
    DOUBLE,
    LONG_DOUBLE,
    POINTER,
    ARRAY,
    VECTOR,
    FUNCTION,
    STRUCT,
    UNION,
    ENUM,
    EMBED,
    INIT_LIST,
    ANY, // magically converts to any other type during type checking, but causes errors
         // at codegen. For stubbed out functionality like __builtins().
}
// FIXME: metaprogam this
str str_for(CTypeKind k){
    final switch(k)with(k){
        case UNSET       : return "UNSET";
        case VOID        : return "VOID";
        case CHAR        : return "CHAR";
        case SHORT       : return "SHORT";
        case INT         : return "INT";
        case LONG        : return "LONG";
        case LONG_LONG   : return "LONG_LONG";
        case INT128      : return "INT128";
        case FLOAT       : return "FLOAT";
        case DOUBLE      : return "DOUBLE";
        case LONG_DOUBLE : return "LONG_DOUBLE";
        case POINTER     : return "POINTER";
        case ARRAY       : return "ARRAY";
        case VECTOR      : return "VECTOR";
        case FUNCTION    : return "FUNCTION";
        case STRUCT      : return "STRUCT";
        case UNION       : return "UNION";
        case ENUM        : return "ENUM";
        case EMBED       : return "EMBED";
        case INIT_LIST   : return "INIT_LIST";
        case ANY         : return "ANY";
    }
}

// Struct field definition
struct StructField {
    str name;
    CType* type;
    size_t offset;  // Byte offset from start of struct
}

// =============================================================================
// Declarator (ISO/IEC 9899:202y Section 6.7.7)
// =============================================================================
//
// (6.7.7.1) declarator:
//     pointer_opt direct-declarator
//
// (6.7.7.1) direct-declarator:
//     identifier attribute-specifier-sequence_opt
//     ( declarator )
//     array-declarator attribute-specifier-sequence_opt
//     function-declarator attribute-specifier-sequence_opt
//
// (6.7.7.1) pointer:
//     * attribute-specifier-sequence_opt type-qualifier-list_opt
//     * attribute-specifier-sequence_opt type-qualifier-list_opt pointer
//
struct CDeclarator {
    CToken name;              // The identifier (empty for abstract declarators)
    int pointer_depth;        // Number of pointer indirections (each * adds 1)

    // For array declarators: direct-declarator [ assignment-expression_opt ]
    // Supports multi-dimensional arrays like arr[2][3][4]
    size_t[] array_dims;      // Array dimensions (empty if not an array)

    // For function declarators: direct-declarator ( parameter-type-list_opt )
    bool is_function;
    CType*[] param_types;     // Parameter types (for type building)
    CParam[] params;          // Full parameters with names (for function definitions)
    bool is_varargs;          // Has ... in parameter list

    // For nested declarators like (*ptr) or (*func)(int)
    // Used for function pointers and pointer-to-array
    CDeclarator* nested;

    bool is_array() { return array_dims.length > 0; }
}

struct CType {
    CTypeKind kind;
    bool is_signed;
    bool is_const;
    bool is_magic_builtin; // For gnu __builtin_* function-like builtins.
    CType* pointed_to;     // For pointers and arrays
    CType* return_type;    // For functions
    CType*[] param_types;  // For functions
    bool is_varargs;       // For functions
    size_t array_size;     // For arrays
    size_t vector_size;    // For vectors, size in bytes (for some reason)
    // Struct fields
    str struct_name;       // For named structs
    StructField[] fields;  // For structs
    size_t struct_size;    // Cached total size of struct

    // Get size in bytes (for pointer arithmetic)
    size_t size_of(){
        final switch(kind){
            case CTypeKind.UNSET:       return 0;
            case CTypeKind.VOID:        return 0;
            case CTypeKind.CHAR:        return 1;
            case CTypeKind.SHORT:       return 2;
            case CTypeKind.INT:         return 4;
            // long: 8 bytes on LP64 (Unix), 4 bytes on LLP64 (Windows)
            case CTypeKind.LONG:
                static if(IS_LP64) return 8;
                else return 4;
            case CTypeKind.LONG_LONG:   return 8;
            case CTypeKind.INT128:      return 16;
            case CTypeKind.FLOAT:       return 4;
            case CTypeKind.DOUBLE:      return 8;
            // long double: 8 bytes on ARM64 (same as double), 16 bytes on x86_64 (80-bit extended)
            case CTypeKind.LONG_DOUBLE:
                version(AArch64) return 8;
                else return 16;
            // Pointers: 8 bytes on 64-bit, 4 bytes on 32-bit
            case CTypeKind.POINTER:
                static if(IS_64BIT) return 8;
                else return 4;
            case CTypeKind.ARRAY:    return pointed_to.size_of() * array_size;
            case CTypeKind.FUNCTION:
                static if(IS_64BIT) return 8;
                else return 4;
            case CTypeKind.STRUCT:   return struct_size;
            case CTypeKind.UNION:    return struct_size;  // Union size is max of all members
            case CTypeKind.ENUM:     return 4;  // Enums are ints
            case CTypeKind.EMBED:    return 0; // maybe assert? idk
            case CTypeKind.INIT_LIST:return 0; // maybe assert? idk
            case CTypeKind.ANY:      return 8; // maybe assert? idk
            case CTypeKind.VECTOR:   return vector_size;
        }
    }

    // Get alignment in bytes
    size_t align_of(){
        final switch(kind){
            case CTypeKind.UNSET:       return 0;
            case CTypeKind.VOID:        return 1;
            case CTypeKind.CHAR:        return 1;
            case CTypeKind.SHORT:       return 2;
            case CTypeKind.INT:         return 4;
            // long: 8 bytes alignment on LP64 (Unix), 4 bytes on LLP64 (Windows)
            case CTypeKind.LONG:
                static if(IS_LP64) return 8;
                else return 4;
            case CTypeKind.LONG_LONG:   return 8;
            case CTypeKind.INT128:      return 16;
            case CTypeKind.FLOAT:       return 4;
            case CTypeKind.DOUBLE:      return 8;
            // long double: 8 bytes alignment on ARM64, 16 bytes on x86_64
            case CTypeKind.LONG_DOUBLE:
                version(AArch64) return 8;
                else return 16;
            // Pointers: 8 bytes alignment on 64-bit, 4 bytes on 32-bit
            case CTypeKind.POINTER:
                static if(IS_64BIT) return 8;
                else return 4;
            case CTypeKind.ARRAY:    return pointed_to ? pointed_to.align_of() : 1;
            case CTypeKind.FUNCTION:
                static if(IS_64BIT) return 8;
                else return 4;
            case CTypeKind.STRUCT:
            case CTypeKind.UNION:
                // Alignment is max alignment of all fields
                size_t max_align = 1;
                foreach(ref f; fields){
                    size_t a = f.type.align_of();
                    if(a > max_align) max_align = a;
                }
                return max_align;
            case CTypeKind.ENUM:     return 4;
            case CTypeKind.EMBED:    return 0; // maybe assert? idk
            case CTypeKind.INIT_LIST:return 0; // maybe assert? idk
            case CTypeKind.ANY:      return 8; // maybe assert? idk
            case CTypeKind.VECTOR:   return vector_size;
        }
    }

    // Get element size for pointer arithmetic
    size_t element_size(){
        if((kind == CTypeKind.POINTER || kind == CTypeKind.ARRAY) && pointed_to){
            size_t s = pointed_to.size_of();
            return s ? s : 1;  // void* arithmetic uses size 1
        }
        assert(0, "element size for non pointer/array");
        return 1;
    }

    bool is_void(){ return kind == CTypeKind.VOID; }
    bool is_pointer(){ return kind == CTypeKind.POINTER; }
    bool is_array(){ return kind == CTypeKind.ARRAY; }
    bool is_indexable(){ return kind == CTypeKind.ARRAY || kind == CTypeKind.POINTER || kind == CTypeKind.VECTOR; }
    bool is_object(){ return kind != CTypeKind.FUNCTION;}
    bool is_struct(){ return kind == CTypeKind.STRUCT; }
    bool is_union(){ return kind == CTypeKind.UNION; }
    bool is_struct_or_union(){ return kind == CTypeKind.STRUCT || kind == CTypeKind.UNION; }
    bool is_enum(){ return kind == CTypeKind.ENUM; }
    bool is_function(){ return kind == CTypeKind.FUNCTION; }
    bool is_embed(){ return kind == CTypeKind.EMBED;}
    bool is_integer(){
        return kind == CTypeKind.CHAR || kind == CTypeKind.SHORT ||
               kind == CTypeKind.INT || kind == CTypeKind.LONG ||
               kind == CTypeKind.ENUM;  // Enums are integer-compatible
    }
    bool is_float(){
        return kind == CTypeKind.FLOAT || kind == CTypeKind.DOUBLE;
    }
    bool is_float32(){
        return kind == CTypeKind.FLOAT;
    }
    bool is_arithmetic(){
        return is_integer() || is_float();
    }
    bool is_boolable(){
        final switch(kind)with(kind){
            case UNSET       : return false;
            case VOID        : return false;
            case CHAR        : return true;
            case SHORT       : return true;
            case INT         : return true;
            case LONG        : return true;
            case LONG_LONG   : return true;
            case INT128      : return true;
            case FLOAT       : return true;
            case DOUBLE      : return true;
            case LONG_DOUBLE : return true;
            case POINTER     : return true;
            case ARRAY       : return true;
            case FUNCTION    : return true; // a bit weird
            case STRUCT      : return false;
            case UNION       : return false;
            case ENUM        : return true;
            case EMBED       : return false;
            case INIT_LIST   : return false;
            case ANY         : return true;
            case VECTOR      : return false;
        }
    }

    // Get a struct/union field by name
    StructField* get_field(str name){
        if(kind != CTypeKind.STRUCT && kind != CTypeKind.UNION) return null;
        foreach(ref f; fields){
            if(f.name == name) return &f;
        }
        return null;
    }

    // Get element type for arrays/pointers
    CType* element_type(){
        if(kind == CTypeKind.ARRAY || kind == CTypeKind.POINTER)
            return pointed_to;
        if(vector_size)
            return pointed_to;
        assert(0, "element type for nonnull array/pointer");
        return null;
    }

    // Number of stack slots needed for this type (for local allocation)
    size_t stack_slots(){
        if(kind == CTypeKind.ARRAY){
            // Array needs enough slots for its total size (8 bytes per slot)
            return (size_of() + 7) / 8;
        }
        if(kind == CTypeKind.STRUCT || kind == CTypeKind.UNION){
            // Struct/union needs enough slots for its size (8 bytes per slot)
            return (struct_size + 7) / 8;
        }
        return 1;  // Everything else is one slot (pointer-sized)
    }
}

// Pre-allocated common types
__gshared CType TYPE_VOID = { kind: CTypeKind.VOID };
__gshared CType TYPE_CHAR = { kind: CTypeKind.CHAR, is_signed:true};
__gshared CType TYPE_INT = { kind: CTypeKind.INT, is_signed:true};
__gshared CType TYPE_LONG = { kind: CTypeKind.LONG, is_signed:true};
__gshared CType TYPE_LLONG = { kind: CTypeKind.LONG_LONG, is_signed:true};
__gshared CType TYPE_FLOAT = { kind: CTypeKind.FLOAT };
__gshared CType TYPE_DOUBLE = { kind: CTypeKind.DOUBLE };
__gshared CType TYPE_LONG_DOUBLE = { kind: CTypeKind.LONG_DOUBLE };
__gshared CType TYPE_UCHAR = { kind: CTypeKind.CHAR};
__gshared CType TYPE_UINT = { kind: CTypeKind.INT};
__gshared CType TYPE_ULONG = { kind: CTypeKind.LONG};
__gshared CType TYPE_ULLONG = { kind: CTypeKind.LONG_LONG};
__gshared CType TYPE_INT128 = { kind: CTypeKind.INT128, is_signed:true};
__gshared CType TYPE_UINT128 = { kind: CTypeKind.INT128};
// FIXME: cfront.compat? or something?
alias TYPE_SIZE_T = TYPE_ULLONG;
alias TYPE_PTRDIFF_T = TYPE_LLONG;

// Pointer types
__gshared CType TYPE_VOID_PTR = { kind: CTypeKind.POINTER, pointed_to: &TYPE_VOID };
__gshared CType TYPE_CHAR_PTR = { kind: CTypeKind.POINTER, pointed_to: &TYPE_CHAR };
__gshared CType TYPE_INT_PTR = { kind: CTypeKind.POINTER, pointed_to: &TYPE_INT };
__gshared CType TYPE_LONG_PTR = { kind: CTypeKind.POINTER, pointed_to: &TYPE_LONG };

// This might break things, but we'll see
__gshared CType TYPE_EMBED = {kind: CTypeKind.EMBED};
__gshared CType TYPE_INIT_LIST = {kind: CTypeKind.INIT_LIST};
__gshared CType TYPE_UNIMPLEMENTED_BUILTIN = {kind: CTypeKind.FUNCTION, is_magic_builtin:true, return_type:&TYPE_ANY};
__gshared CType TYPE_ANY = {kind: CTypeKind.ANY, is_magic_builtin:true};

CType* make_pointer_type(Allocator a, CType* base){
    void[] data = a.zalloc(CType.sizeof);
    CType* result = cast(CType*)data.ptr;
    result.kind = CTypeKind.POINTER;
    result.pointed_to = base;
    return result;
}

CType* make_array_type(Allocator a, CType* element_type, size_t size){
    void[] data = a.zalloc(CType.sizeof);
    CType* result = cast(CType*)data.ptr;
    result.kind = CTypeKind.ARRAY;
    result.pointed_to = element_type;  // Element type stored in pointed_to
    result.array_size = size;
    return result;
}
CType* make_vector_type(Allocator a, CType* element_type, size_t size){
    void[] data = a.zalloc(CType.sizeof);
    CType* result = cast(CType*)data.ptr;
    result.kind = CTypeKind.VECTOR;
    result.pointed_to = element_type;  // Element type stored in pointed_to
    result.vector_size = size;
    return result;
}

CType* make_struct_type(Allocator a, str name, StructField[] fields, size_t total_size){
    auto data = a.zalloc(CType.sizeof);
    auto result = cast(CType*)data.ptr;
    result.kind = CTypeKind.STRUCT;
    result.struct_name = name;
    result.fields = fields;
    result.struct_size = total_size;
    return result;
}

CType* make_union_type(Allocator a, str name, StructField[] fields, size_t total_size){
    auto data = a.zalloc(CType.sizeof);
    auto result = cast(CType*)data.ptr;
    result.kind = CTypeKind.UNION;
    result.struct_name = name;
    result.fields = fields;  // All fields have offset 0 in a union
    result.struct_size = total_size;
    return result;
}

CType* make_enum_type(Allocator a, str name){
    auto data = a.zalloc(CType.sizeof);
    auto result = cast(CType*)data.ptr;
    result.kind = CTypeKind.ENUM;
    result.struct_name = name;  // Reuse struct_name for enum name
    return result;
}

CType* make_function_type(Allocator a, CType* ret_type, CType*[] param_types, bool is_varargs){
    void[] data = a.zalloc(CType.sizeof);
    CType* result = cast(CType*)data.ptr;
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
    ALIGNOF,
    COUNTOF,
    VA_ARG,
    GROUPING,
    TERNARY,
    INIT_LIST,
    COMPOUND_LITERAL,
    GENERIC,
    EMBED,
    STMT_EXPR,  // GNU statement expression ({ ... })
}

struct CExpr {
    CExprKind kind;
    CType* type;  // Resolved type (set during analysis/codegen)
    CToken token; // For error reporting

    pragma(inline, true);

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
    inout(CTernary)* as_ternary() inout {
        return kind == CExprKind.TERNARY ? cast(typeof(return))&this : null;
    }
    inout(CInitList)* as_init_list() inout {
        return kind == CExprKind.INIT_LIST ? cast(typeof(return))&this : null;
    }
    inout(CCompoundLiteral)* as_compound_literal() inout {
        return kind == CExprKind.COMPOUND_LITERAL ? cast(typeof(return))&this : null;
    }
    inout(CGeneric)* as_generic() inout {
        return kind == CExprKind.GENERIC ? cast(typeof(return))&this : null;
    }
    inout(CEmbed)* as_embed() inout {
        return kind == CExprKind.EMBED ? cast(typeof(return))&this : null;
    }
    inout(CSizeof)* as_sizeof() inout {
        return kind == CExprKind.SIZEOF ? cast(typeof(return))&this : null;
    }
    inout(CAlignof)* as_alignof() inout {
        return kind == CExprKind.ALIGNOF ? cast(typeof(return))&this : null;
    }
    inout(CCountof)* as_countof() inout {
        return kind == CExprKind.COUNTOF ? cast(typeof(return))&this : null;
    }
    inout(CVaArg)* as_va_arg() inout {
        return kind == CExprKind.VA_ARG ? cast(typeof(return))&this : null;
    }
    inout(CStmtExpr)* as_stmt_expr() inout {
        return kind == CExprKind.STMT_EXPR ? cast(typeof(return))&this : null;
    }

    CExpr* ungroup(){
        if(kind == CExprKind.GROUPING){
            return this.as_grouping.expression.ungroup();
        }
        if(kind == CExprKind.GENERIC)
            return this.as_generic.picked;
        return &this;
    }
}

struct CLiteral {
    CExpr expr;
    CToken value;

    static CExpr* make(Allocator a, CToken t, CType* type){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.LITERAL;
        result.expr.token = t;
        result.expr.type = type;
        result.value = t;
        return &result.expr;
    }

    // Create a synthetic integer literal
    static CExpr* make_int(Allocator a, int val, CToken tok){
        import dlib.stringbuilder : mwritef;
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.LITERAL;
        result.expr.token = tok;
        result.expr.type = &TYPE_INT;
        result.value = tok;
        result.value.lexeme = mwritef(a, "%", val)[];
        result.value.type = CTokenType.NUMBER;
        return &result.expr;
    }
    static CExpr* make_size_t(Allocator a, size_t val, CToken tok){
        import dlib.stringbuilder : mwritef;
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.LITERAL;
        result.expr.token = tok;
        result.expr.type = &TYPE_SIZE_T;
        result.value = tok;
        result.value.lexeme = mwritef(a, "%", val)[];
        result.value.type = CTokenType.NUMBER;
        return &result.expr;
    }
}

struct CSizeof {
    CExpr expr;
    CType* sizeof_type;   // If sizeof(type)
    CExpr* sizeof_expr;   // If sizeof expr
    size_t size;          // Precomputed size (if known)

    static CExpr* make(Allocator a, CType* t, size_t sz, CToken tok){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.SIZEOF;
        result.expr.token = tok;
        result.expr.type = &TYPE_SIZE_T;
        result.sizeof_type = t;
        result.sizeof_expr = null;
        result.size = sz;
        return &result.expr;
    }

    static CExpr* make_expr(Allocator a, CExpr* e, CToken tok){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.SIZEOF;
        result.expr.token = tok;
        result.expr.type = &TYPE_SIZE_T;
        result.sizeof_type = null;
        result.sizeof_expr = e;
        result.size = 0;  // Will be computed during codegen
        return &result.expr;
    }
}

struct CAlignof {
    CExpr expr;
    CType* alignof_type;   // If _Alignof(type)
    CExpr* alignof_expr;   // If _Alignof(expr) (GNU extension)
    size_t alignment;      // Precomputed alignment (if known)

    static CExpr* make(Allocator a, CType* t, size_t align_, CToken tok){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.ALIGNOF;
        result.expr.token = tok;
        result.expr.type = &TYPE_SIZE_T;
        result.alignof_type = t;
        result.alignof_expr = null;
        result.alignment = align_;
        return &result.expr;
    }

    static CExpr* make_expr(Allocator a, CExpr* e, CToken tok){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.ALIGNOF;
        result.expr.token = tok;
        result.expr.type = &TYPE_SIZE_T;
        result.alignof_type = null;
        result.alignof_expr = e;
        result.alignment = 0;
        return &result.expr;
    }
}

struct CCountof {
    CExpr expr;
    CType* countof_type;   // If _Countof(type)
    CExpr* countof_expr;   // If _Countof(expr)
    size_t count;          // Precomputed count (if known)

    static CExpr* make(Allocator a, CType* t, size_t count_, CToken tok){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.COUNTOF;
        result.expr.token = tok;
        result.expr.type = &TYPE_SIZE_T;
        result.countof_type = t;
        result.countof_expr = null;
        result.count = count_;
        return &result.expr;
    }

    static CExpr* make_expr(Allocator a, CExpr* e, size_t count_, CToken tok){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.COUNTOF;
        result.expr.token = tok;
        result.expr.type = &TYPE_SIZE_T;
        result.countof_type = null;
        result.countof_expr = e;
        result.count = count_;
        return &result.expr;
    }
}

struct CVaArg {
    CExpr expr;
    CExpr* va_list_expr;   // The va_list argument
    CType* arg_type;       // The type to extract

    static CExpr* make(Allocator a, CExpr* va_list_, CType* type, CToken tok){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.VA_ARG;
        result.expr.token = tok;
        result.expr.type = type;
        result.va_list_expr = va_list_;
        result.arg_type = type;
        return &result.expr;
    }
}

// What kind of thing an identifier resolved to
enum IdentifierRefKind : ubyte {
    UNKNOWN,       // Not yet resolved (shouldn't happen after parsing)
    LOCAL_VAR,     // Local variable (stack or register - codegen decides)
    GLOBAL_VAR,    // Global variable defined in this translation unit
    EXTERN_VAR,    // External variable (from dlimport)
    ENUM_CONST,    // Enum constant (has integer value)
    FUNCTION,      // Function defined in this translation unit
    EXTERN_FUNC,   // External function (from dlimport)
    BUILTIN,       // __builtin_* function
}

struct CIdentifier {
    CExpr expr;
    CToken name;
    IdentifierRefKind ref_kind;

    // Resolution data (depends on ref_kind)
    long enum_value;    // For ENUM_CONST: the constant's integer value
    str extern_module;  // For EXTERN_VAR/EXTERN_FUNC: module alias (e.g., "Libc")

    static CExpr* make(Allocator a, CToken t, CType* type){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.IDENTIFIER;
        result.expr.token = t;
        result.expr.type = type;
        result.name = t;
        result.ref_kind = IdentifierRefKind.UNKNOWN;
        return &result.expr;
    }

    static CExpr* make_local_var(Allocator a, CToken t, CType* type){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.IDENTIFIER;
        result.expr.token = t;
        result.expr.type = type;
        result.name = t;
        result.ref_kind = IdentifierRefKind.LOCAL_VAR;
        return &result.expr;
    }

    static CExpr* make_global_var(Allocator a, CToken t, CType* type){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.IDENTIFIER;
        result.expr.token = t;
        result.expr.type = type;
        result.name = t;
        result.ref_kind = IdentifierRefKind.GLOBAL_VAR;
        return &result.expr;
    }

    static CExpr* make_extern_var(Allocator a, CToken t, CType* type, str module_alias){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.IDENTIFIER;
        result.expr.token = t;
        result.expr.type = type;
        result.name = t;
        result.ref_kind = IdentifierRefKind.EXTERN_VAR;
        result.extern_module = module_alias;
        return &result.expr;
    }

    static CExpr* make_enum_const(Allocator a, CToken t, long value){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.IDENTIFIER;
        result.expr.token = t;
        result.expr.type = &TYPE_INT;
        result.name = t;
        result.ref_kind = IdentifierRefKind.ENUM_CONST;
        result.enum_value = value;
        return &result.expr;
    }

    static CExpr* make_func(Allocator a, CToken t, CType* type){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.IDENTIFIER;
        result.expr.token = t;
        result.expr.type = type;
        result.name = t;
        result.ref_kind = IdentifierRefKind.FUNCTION;
        return &result.expr;
    }

    static CExpr* make_extern_func(Allocator a, CToken t, CType* type, str module_alias){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.IDENTIFIER;
        result.expr.token = t;
        result.expr.type = type;
        result.name = t;
        result.ref_kind = IdentifierRefKind.EXTERN_FUNC;
        result.extern_module = module_alias;
        return &result.expr;
    }

    static CExpr* make_builtin(Allocator a, CToken t, CType* type){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.IDENTIFIER;
        result.expr.token = t;
        result.expr.type = type;
        result.name = t;
        result.ref_kind = IdentifierRefKind.BUILTIN;
        return &result.expr;
    }

    // Deprecated - use specific make_* variants
    static CExpr* make_var(Allocator a, CToken t, CType* type){
        return make_local_var(a, t, type);  // Default to local for backwards compat
    }
}

struct CBinary {
    CExpr expr;
    CExpr* left_;
    CTokenType op;
    CExpr* right_;

    CExpr* left(){ return left_.ungroup(); }
    CExpr* right(){ return right_.ungroup(); }

    static CExpr* make(Allocator a, CExpr* l, CTokenType op, CExpr* r, CToken t, CType* type){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.BINARY;
        result.expr.token = t;
        result.expr.type = type;
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

    CExpr* operand(){ return operand_.ungroup(); }

    static CExpr* make(Allocator a, CTokenType op, CExpr* operand, bool prefix, CToken t, CType* type){
        CUnary* result = a.zalloc!(typeof(this))(1).ptr;
        result.expr.kind = CExprKind.UNARY;
        result.expr.token = t;
        result.expr.type = type;
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

    CExpr* callee(){ return callee_.ungroup(); }

    static CExpr* make(Allocator a, CExpr* callee, CExpr*[] args, CToken t, CType* type){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.CALL;
        result.expr.token = t;
        result.expr.type = type;
        result.callee_ = callee;
        result.args = args;
        return &result.expr;
    }

    CType* callee_function_type(){
        CType* t = callee.type;
        if(t is null) return null; // XXX
        if(t.is_pointer)
            t = t.pointed_to;
        if(!t.is_function) return null; // XXX
        return t;
    }
}

struct CAssign {
    CExpr expr;
    CExpr* target_;
    CTokenType op;  // EQUAL, PLUS_EQUAL, etc.
    CExpr* value_;

    CExpr* target(){ return target_.ungroup(); }
    CExpr* value(){ return value_.ungroup(); }

    static CExpr* make(Allocator a, CExpr* target, CTokenType op, CExpr* value, CToken t, CType* type){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.ASSIGN;
        result.expr.token = t;
        result.expr.type = type;
        result.target_ = target;
        result.op = op;
        result.value_ = value;
        return &result.expr;
    }
}

struct CGrouping {
    CExpr expr;
    CExpr* expression;

    static CExpr* make(Allocator a, CExpr* e, CToken t){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.GROUPING;
        result.expr.token = t;
        result.expr.type = e.type;
        result.expression = e;
        return &result.expr;
    }
}

struct CTernary {
    CExpr expr;
    CExpr* condition_;
    CExpr* if_true_;
    CExpr* if_false_;

    CExpr* condition(){ return condition_.ungroup(); }
    CExpr* if_true(){ return if_true_.ungroup(); }
    CExpr* if_false(){ return if_false_.ungroup(); }

    static CExpr* make(Allocator a, CExpr* cond, CExpr* t, CExpr* f, CToken tok, CType* type){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.TERNARY;
        result.expr.token = tok;
        result.expr.type = type;
        result.condition_ = cond;
        result.if_true_ = t;
        result.if_false_ = f;
        return &result.expr;
    }
}

// =============================================================================
// Designated Initializers (C2y §6.7.11)
// =============================================================================
//
// (6.7.11) designator:
//     [ constant-expression ]
//     . identifier

enum CDesignatorKind {
    FIELD,  // .identifier
    INDEX,  // [constant-expression]
}

struct CDesignator {
    CDesignatorKind kind;
    str field_name;      // for FIELD
    long index_value;    // for INDEX (evaluated constant)
    CToken token;
}

// Element in an initializer list, with optional designators
struct CInitElement {
    CDesignator[] designators;  // empty if positional
    CExpr* value;               // the initializer value
}

// (6.7.11) braced-initializer: { } | { initializer-list } | { initializer-list , }
struct CInitList {
    CExpr expr;
    CInitElement[] elements;

    static CExpr* make(Allocator a, CInitElement[] elems, CToken tok){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.INIT_LIST;
        result.expr.token = tok;
        result.expr.type = &TYPE_INIT_LIST;
        result.elements = elems;
        return &result.expr;
    }
}

struct CCast {
    CExpr expr;
    CType* cast_type;
    CExpr* operand_;

    CExpr* operand(){ return operand_.ungroup(); }

    static CExpr* make(Allocator a, CType* t, CExpr* e, CToken tok){
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

struct CCompoundLiteral {
    CExpr expr;
    CType* literal_type;
    CExpr* initializer_;

    CExpr* initializer(){ return initializer_.ungroup(); }

    static CExpr* make(Allocator a, CType* t, CExpr* init, CToken tok){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.COMPOUND_LITERAL;
        result.expr.token = tok;
        result.expr.type = t;
        result.literal_type = t;
        result.initializer_ = init;
        return &result.expr;
    }
}

struct CSubscript {
    CExpr expr;
    CExpr* array_;
    CExpr* index_;

    CExpr* array(){ return array_.ungroup(); }
    CExpr* index(){ return index_.ungroup(); }

    static CExpr* make(Allocator a, CExpr* arr, CExpr* idx, CToken t, CType* type){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.SUBSCRIPT;
        result.expr.type = type;
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

    CExpr* object(){ return object_.ungroup(); }

    static CExpr* make(Allocator a, CExpr* obj, CToken member_, bool arrow, CToken t, CType* type){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.MEMBER_ACCESS;
        result.expr.token = t;
        result.expr.type = type;
        result.object_ = obj;
        result.member = member_;
        result.is_arrow = arrow;
        return &result.expr;
    }
}

struct CGenericAssoc {
    CType* type;    // null for default
    CExpr* result;
}

struct CGeneric {
    CExpr expr;
    CExpr* _controlling_;
    CGenericAssoc[] _associations;
    CExpr* picked;

    CExpr* _controlling(){ return _controlling_.ungroup(); }

    static CExpr* make(Allocator a, CExpr* ctrl, CGenericAssoc[] assocs, CToken tok, CExpr* picked, CType* type){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.GENERIC;
        result.expr.token = tok;
        result.expr.type = type;
        result._controlling_ = ctrl;
        result._associations = assocs;
        result.picked = picked;
        return &result.expr;
    }
}

// __embed("path", offset, length) - emitted by preprocessor for #embed
struct CEmbed {
    CExpr expr;
    str path;       // Resolved file path
    long offset;    // Byte offset into file
    long length;    // Number of bytes

    static CExpr* make(Allocator a, str path, long offset, long length, CToken tok){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.EMBED;
        result.expr.token = tok;
        result.expr.type = &TYPE_EMBED;
        result.path = path;
        result.offset = offset;
        result.length = length;
        return &result.expr;
    }
}

// GNU statement expression: ({ stmt; stmt; expr; })
// The value is the last expression in the block
struct CStmtExpr {
    CExpr expr;
    CStmt*[] statements;  // The statements in the block

    // The final expression (value of the whole thing), may be null
    CExpr* result_expr(){
        if(!statements.length) return null;
        CExprStmt* s = statements[$-1].as_expr_stmt();
        if(!s) return null;
        return s.expression;
    }

    static CExpr* make(Allocator a, CStmt*[] stmts, CToken tok, CType* type){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.expr.kind = CExprKind.STMT_EXPR;
        result.expr.token = tok;
        result.expr.type = type;
        result.statements = stmts;
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
    DO_WHILE,
    FOR,
    BLOCK,
    VAR_DECL,
    BREAK,
    CONTINUE,
    EMPTY,
    SWITCH,
    GOTO,
    LABEL,
    DASM,
    CASE_LABEL,
    ASM,  // GCC inline assembly (__asm__)
}

struct CStmt {
    CStmtKind kind;
    CToken token;  // For error reporting

    inout(CExprStmt)* as_expr_stmt() inout {
        return kind == CStmtKind.EXPR ? cast(typeof(return))&this : null;
    }
}

struct CExprStmt {
    CStmt stmt;
    CExpr* expression;

    static CStmt* make(Allocator a, CExpr* e, CToken t){
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

    static CStmt* make(Allocator a, CExpr* v, CToken t){
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

    static CStmt* make(Allocator a, CExpr* cond, CStmt* then_, CStmt* else_, CToken t){
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

    static CStmt* make(Allocator a, CExpr* cond, CStmt* body_, CToken t){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.stmt.kind = CStmtKind.WHILE;
        result.stmt.token = t;
        result.condition = cond;
        result.body = body_;
        return &result.stmt;
    }
}

struct CDoWhileStmt {
    CStmt stmt;
    CExpr* condition;
    CStmt* body;

    static CStmt* make(Allocator a, CStmt* body_, CExpr* cond, CToken t){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.stmt.kind = CStmtKind.DO_WHILE;
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

    static CStmt* make(Allocator a, CStmt* init_, CExpr* cond, CExpr* incr, CStmt* body__, CToken t){
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

    static CStmt* make(Allocator a, CStmt*[] stmts, CToken t){
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

    static CStmt* make(Allocator a, CType* type, CToken name_, CExpr* init, CToken t){
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

    static CStmt* make(Allocator a, CToken t){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.stmt.kind = CStmtKind.BREAK;
        result.stmt.token = t;
        return &result.stmt;
    }
}

struct CContinueStmt {
    CStmt stmt;

    static CStmt* make(Allocator a, CToken t){
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

    static CStmt* get(){
        return &singleton.stmt;
    }
}

// (6.8.4.2) switch statement
// Switch body can contain case/default labels at any nesting level (Duff's device)
struct CSwitchStmt {
    CStmt stmt;
    CExpr* condition;
    CStmt* body_;  // Usually a block containing case labels

    static CStmt* make(Allocator a, CExpr* cond, CStmt* body__, CToken t){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.stmt.kind = CStmtKind.SWITCH;
        result.stmt.token = t;
        result.condition = cond;
        result.body_ = body__;
        return &result.stmt;
    }
}

// (6.8.1) case/default labeled statement:
//     case constant-expression : statement
//     default : statement
struct CCaseLabelStmt {
    CStmt stmt;
    CExpr* case_value;  // null for default
    CStmt* statement;   // The labeled statement
    bool is_default;

    static CStmt* make(Allocator a, CExpr* case_val, CStmt* s, bool is_def, CToken t){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.stmt.kind = CStmtKind.CASE_LABEL;
        result.stmt.token = t;
        result.case_value = case_val;
        result.statement = s;
        result.is_default = is_def;
        return &result.stmt;
    }
}

// (6.8.6.1) goto statement:
//     goto identifier ;
struct CGotoStmt {
    CStmt stmt;
    CToken label;  // The label to jump to

    static CStmt* make(Allocator a, CToken lbl, CToken t){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.stmt.kind = CStmtKind.GOTO;
        result.stmt.token = t;
        result.label = lbl;
        return &result.stmt;
    }
}

// (6.8.1) labeled-statement:
//     identifier : statement
struct CLabelStmt {
    CStmt stmt;
    CToken label;      // The label identifier
    CStmt* statement;  // The statement following the label

    static CStmt* make(Allocator a, CToken lbl, CStmt* s, CToken t){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.stmt.kind = CStmtKind.LABEL;
        result.stmt.token = t;
        result.label = lbl;
        result.statement = s;
        return &result.stmt;
    }
}

struct CDasmStmt {
    CStmt stmt;
    str code;  // Raw dasm code to emit

    static CStmt* make(Allocator a, str code, CToken t){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.stmt.kind = CStmtKind.DASM;
        result.stmt.token = t;
        result.code = code;
        return &result.stmt;
    }
}

// GCC inline assembly statement: __asm__("..." : outputs : inputs : clobbers)
struct CAsmStmt {
    CStmt stmt;

    static CStmt* make(Allocator a, CToken t){
        auto data = a.zalloc(typeof(this).sizeof);
        auto result = cast(typeof(this)*)data.ptr;
        result.stmt.kind = CStmtKind.ASM;
        result.stmt.token = t;
        return &result.stmt;
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
    bool is_static;      // static linkage - cannot be dlimported
    bool is_inline;      // inline function - only emit if used
    str library;         // From #pragma library (for declarations)
}

struct CGlobalVar {
    CToken name;
    CType* var_type;
    CExpr* initializer;  // null if uninitialized (will be zero-initialized)
    bool is_extern;      // extern declaration - object defined elsewhere (dlimport)
    str library;         // From #pragma library (for extern declarations)
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
    CGlobalVar[] globals;
    CStructDef[] structs;
    CUnionDef[] unions;
    CEnumDef[] enums;
    str current_library;  // Set by #pragma library
}

// Resolve _Generic - find matching association
CExpr* resolve_generic(CGenericAssoc[] associations, CType* ctrl_type){
    CExpr* default_result = null;
    foreach(ref CGenericAssoc assoc; associations){
        if(assoc.type is null){
            default_result = assoc.result;
        } else if(types_compatible(ctrl_type, assoc.type)){
            return assoc.result;
        }
    }
    return default_result;
}

// FIXME: this impl seems sus
// Check if two types are compatible for _Generic matching
bool types_compatible(CType* a, CType* b){
    if(a is null || b is null) return false;
    if(a is b) return true;
    if(a.kind != b.kind) return false;
    if(a.is_signed != b.is_signed) return false;

    // For pointers, check pointed-to type
    if(a.kind == CTypeKind.POINTER){
        return types_compatible(a.pointed_to, b.pointed_to);
    }
    // sus
    // For arrays, check element type
    if(a.kind == CTypeKind.ARRAY){
        return types_compatible(a.pointed_to, b.pointed_to);
    }
    // For structs/unions, compare by name or identity
    if(a.kind == CTypeKind.STRUCT || a.kind == CTypeKind.UNION){
        if(a.struct_name.length > 0 && b.struct_name.length > 0){
            return a.struct_name == b.struct_name;
        }
        return a is b;  // anonymous structs must be same instance
    }
    // For functions, check return type and params
    if(a.kind == CTypeKind.FUNCTION){
        if(!types_compatible(a.return_type, b.return_type)) return false;
        if(a.param_types.length != b.param_types.length) return false;
        foreach(i, pt; a.param_types){
            if(!types_compatible(pt, b.param_types[i])) return false;
        }
        return true;
    }
    // Primitive types match if same kind and signedness (already checked above)
    return true;
}
