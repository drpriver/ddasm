/*
 * C to DASM Code Generator for ddasm
 * Copyright 2025, David Priver
 */
module cfront.c_to_dasm;

import core.stdc.stdio : fprintf, stderr;
import dlib.aliases;

// Platform-specific default C library name
version(OSX) {
    enum str DEFAULT_LIBC = "libSystem.B.dylib";
} else version(linux) {
    enum str DEFAULT_LIBC = "libc.so.6";
} else {
    enum str DEFAULT_LIBC = "libc";
}

// Normalize library name - "libc" is a special alias for the platform's C library
str normalize_lib(str lib) {
    import cfront.c_pp_token : str_eq;
    if (lib.length == 0) return DEFAULT_LIBC;
    if (str_eq(lib, "libc")) return DEFAULT_LIBC;
    // Also handle common libc variants
    if (str_eq(lib, "libc.so.6")) return DEFAULT_LIBC;
    if (str_eq(lib, "libSystem.B.dylib")) return DEFAULT_LIBC;
    return lib;
}
import dlib.allocator : Allocator;
import dlib.barray : Barray, make_barray;
import dlib.stringbuilder : StringBuilder, P;
import dlib.table : Table;
import dlib.parse_numbers : parse_unsigned_human;

import cfront.c_pp_to_c : CToken, CTokenType;
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
    int compound_literal_slots;    // Total slots needed for compound literals

    void analyze_function(CFunction* func) {
        addr_taken.data.allocator = allocator;
        arrays.data.allocator = allocator;
        structs.data.allocator = allocator;
        all_vars.bdata.allocator = allocator;
        addr_taken.cleanup();
        arrays.cleanup();
        structs.cleanup();
        all_vars.clear();
        compound_literal_slots = 0;

        foreach (stmt; func.body) {
            analyze_stmt(stmt);
        }
    }

    bool should_use_stack() {
        // Use stack if any address is taken, any arrays, structs, compound literals, or more than 4 variables
        return addr_taken.count > 0 || arrays.count > 0 || structs.count > 0 || compound_literal_slots > 0 || all_vars[].length > 4;
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
            case DO_WHILE:
                auto s = cast(CDoWhileStmt*)stmt;
                analyze_stmt(s.body);
                analyze_expr(s.condition);
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
            case SWITCH:
                auto s = cast(CSwitchStmt*)stmt;
                analyze_expr(s.condition);
                foreach (ref c; s.cases) {
                    if (c.case_value) analyze_expr(c.case_value);
                    foreach (cs; c.statements) analyze_stmt(cs);
                }
                break;
            case GOTO:
                break;  // goto doesn't need analysis
            case LABEL:
                auto s = cast(CLabelStmt*)stmt;
                analyze_stmt(s.statement);
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
                analyze_expr((cast(CCast*)expr).operand);
                break;
            case MEMBER_ACCESS:
                analyze_expr((cast(CMemberAccess*)expr).object);
                break;
            case SIZEOF:
                if (auto se = cast(CSizeof*)expr)
                    if (se.sizeof_expr) analyze_expr(se.sizeof_expr);
                break;
            case ALIGNOF:
                break;
            case COUNTOF:
                if (auto ce = cast(CCountof*)expr)
                    if (ce.countof_expr) analyze_expr(ce.countof_expr);
                break;
            case VA_ARG:
                analyze_expr((cast(CVaArg*)expr).va_list_expr);
                break;
            case GENERIC:
                auto e = cast(CGeneric*)expr;
                analyze_expr(e.controlling);
                foreach (assoc; e.associations)
                    analyze_expr(assoc.result);
                break;
            case TERNARY:
                auto e = cast(CTernary*)expr;
                analyze_expr(e.condition);
                analyze_expr(e.if_true);
                analyze_expr(e.if_false);
                break;
            case INIT_LIST:
                auto e = cast(CInitList*)expr;
                foreach (elem; e.elements)
                    analyze_expr(elem.value);
                break;
            case COMPOUND_LITERAL:
                auto e = cast(CCompoundLiteral*)expr;
                // Count slots needed for this compound literal
                size_t size = e.literal_type.size_of();
                compound_literal_slots += cast(int)((size + 7) / 8);
                analyze_expr(e.initializer);
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
    Table!(str, bool) used_funcs;    // Track which extern functions are actually called
    Table!(str, str) extern_objs;    // Map: extern object name -> module alias
    Table!(str, bool) used_objs;     // Track which extern objects are actually referenced
    Table!(str, bool) called_funcs;  // Track all internal function calls (for inline function generation)
    Table!(str, bool) generated_funcs; // Track which functions have been generated
    Table!(str, bool) addr_taken;    // Variables whose address is taken
    Table!(str, bool) arrays;        // Array variables (need special handling)
    Table!(str, CType*) global_types; // Global variable types
    Table!(str, CType*) func_return_types; // Function return types (for struct returns)
    Table!(str, long) enum_constants; // Enum constant values (name -> value)

    // Loop control
    int current_continue_target = -1;
    int current_break_target = -1;

    // Goto/label support
    Table!(str, int) label_table;  // Map label names to label numbers

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
            case ALIGNOF:
            case COUNTOF:
            case VA_ARG:
            case GROUPING:
                return null;
            case GENERIC:
                // _Generic resolves at compile time; return type of matching result
                auto gen = cast(CGeneric*)e;
                CType* ctrl_type = get_expr_type(gen.controlling);
                CExpr* result = resolve_generic(gen, ctrl_type);
                if (result) return get_expr_type(result);
                return null;
            case TERNARY:
                auto tern = cast(CTernary*)e;
                // Type of ternary is the common type of branches
                auto tt = get_expr_type(tern.if_true);
                if (tt) return tt;
                return get_expr_type(tern.if_false);
            case INIT_LIST:
                // Init list type is determined by context (array/struct type)
                return null;
            case COMPOUND_LITERAL:
                // Compound literal has an explicit type
                auto cl = cast(CCompoundLiteral*)e;
                return cl.literal_type;
        }
    }

    // Check if two types are compatible for _Generic matching
    static bool types_compatible(CType* a, CType* b) {
        if (a is null || b is null) return false;
        if (a is b) return true;
        if (a.kind != b.kind) return false;
        if (a.is_unsigned != b.is_unsigned) return false;

        // For pointers, check pointed-to type
        if (a.kind == CTypeKind.POINTER) {
            return types_compatible(a.pointed_to, b.pointed_to);
        }
        // For arrays, check element type
        if (a.kind == CTypeKind.ARRAY) {
            return types_compatible(a.pointed_to, b.pointed_to);
        }
        // For structs/unions, compare by name or identity
        if (a.kind == CTypeKind.STRUCT || a.kind == CTypeKind.UNION) {
            if (a.struct_name.length > 0 && b.struct_name.length > 0) {
                return a.struct_name == b.struct_name;
            }
            return a is b;  // anonymous structs must be same instance
        }
        // For functions, check return type and params
        if (a.kind == CTypeKind.FUNCTION) {
            if (!types_compatible(a.return_type, b.return_type)) return false;
            if (a.param_types.length != b.param_types.length) return false;
            foreach (i, pt; a.param_types) {
                if (!types_compatible(pt, b.param_types[i])) return false;
            }
            return true;
        }
        // Primitive types match if same kind and signedness (already checked above)
        return true;
    }

    // Resolve _Generic - find matching association
    static CExpr* resolve_generic(CGeneric* gen, CType* ctrl_type) {
        CExpr* default_result = null;
        foreach (assoc; gen.associations) {
            if (assoc.type is null) {
                default_result = assoc.result;
            } else if (types_compatible(ctrl_type, assoc.type)) {
                return assoc.result;
            }
        }
        return default_result;
    }

    // Get the read instruction for a given type size and signedness
    static str read_instr_for_size(size_t size, bool is_unsigned = true) {
        if (is_unsigned) {
            switch (size) {
                case 1: return "read1";
                case 2: return "read2";
                case 4: return "read4";
                default: return "read";
            }
        } else {
            switch (size) {
                case 1: return "sread1";
                case 2: return "sread2";
                case 4: return "sread4";
                default: return "read";
            }
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

    // Check if a size can be written with a single write instruction (1, 2, 4, or 8 bytes)
    static bool needs_memcpy(size_t size) {
        return size != 1 && size != 2 && size != 4 && size != 8;
    }

    @disable this();

    this(StringBuilder* s, Allocator a) {
        allocator = a;
        sb = s;
        reglocals.data.allocator = a;
        stacklocals.data.allocator = a;
        var_types.data.allocator = a;
        extern_funcs.data.allocator = a;
        used_funcs.data.allocator = a;
        extern_objs.data.allocator = a;
        used_objs.data.allocator = a;
        called_funcs.data.allocator = a;
        generated_funcs.data.allocator = a;
        addr_taken.data.allocator = a;
        arrays.data.allocator = a;
        global_types.data.allocator = a;
        label_table.data.allocator = a;
    }

    void cleanup() {
        reglocals.cleanup();
        stacklocals.cleanup();
        var_types.cleanup();
        extern_funcs.cleanup();
        used_funcs.cleanup();
        extern_objs.cleanup();
        used_objs.cleanup();
        called_funcs.cleanup();
        generated_funcs.cleanup();
        addr_taken.cleanup();
        arrays.cleanup();
        global_types.cleanup();
    }

    void error(CToken token, str message) {
        ERROR_OCCURRED = true;
        fprintf(stderr, "%.*s:%d:%d: Code gen error at '%.*s': %.*s\n",
                cast(int)token.file.length, token.file.ptr,
                token.line,
                token.column,
                cast(int)token.lexeme.length, token.lexeme.ptr,
                cast(int)message.length, message.ptr);
        if(token.expansion_file){
            fprintf(stderr, "  note: expanded from macro defined at %.*s:%d:%d\n",
                cast(int)token.expansion_file.length, token.expansion_file.ptr,
                token.expansion_line, token.expansion_column);
        }
    }

    // Generate a module alias from library name
    // "libc.so.6" -> "Libc", "libfoo.so" -> "Libfoo", "/path/to/libbar.dylib" -> "Libbar"
    str make_alias(str lib, int counter) {
        // Strip directory path
        str name = lib;
        foreach_reverse (i, c; lib) {
            if (c == '/' || c == '\\') {
                name = lib[i+1 .. $];
                break;
            }
        }

        // Strip .so, .dylib, .dll and version suffixes like .so.6
        foreach (i, c; name) {
            if (c == '.') {
                name = name[0 .. i];
                break;
            }
        }

        StringBuilder sb;
        sb.allocator = allocator;

        if (name.length == 0) {
            sb.writef("Lib%", counter);
        } else {
            // Capitalize first letter
            if (name[0] >= 'a' && name[0] <= 'z') {
                sb.writef("%", cast(char)(name[0] - 32));
                sb.write(name[1 .. $]);
            } else {
                sb.write(name);
            }
        }
        return sb.detach().data;
    }

    // =========================================================================
    // Main Entry Point
    // =========================================================================

    int generate(CTranslationUnit* unit) {
        // Collect unique libraries and assign aliases for extern declarations
        Table!(str, str) lib_to_alias;
        lib_to_alias.data.allocator = allocator;
        scope(exit) lib_to_alias.cleanup();

        // Build extern_funcs from function declarations (not definitions)
        int lib_counter = 0;
        foreach (ref func; unit.functions) {
            // Skip definitions and static functions
            if (func.is_definition) continue;
            if (func.is_static) continue;

            str fname = func.name.lexeme;
            // Normalize library name and handle SDL special case
            str lib = normalize_lib(func.library);
            if (lib == "libSDL2.so" && (fname.length < 4 || fname[0..4] != "SDL_")) {
                lib = DEFAULT_LIBC;
            }
            if (lib !in lib_to_alias) {
                // Generate alias from library name
                str alias_name = make_alias(lib, lib_counter++);
                lib_to_alias[lib] = alias_name;
            }
            // Track function -> module alias mapping
            extern_funcs[fname] = lib_to_alias[lib];
        }

        // Build extern_objs from extern global variable declarations
        foreach (ref gvar; unit.globals) {
            if (!gvar.is_extern) continue;

            str oname = gvar.name.lexeme;
            str lib = normalize_lib(gvar.library);
            if (lib !in lib_to_alias) {
                str alias_name = make_alias(lib, lib_counter++);
                lib_to_alias[lib] = alias_name;
            }
            extern_objs[oname] = lib_to_alias[lib];
        }

        // Use a temp buffer for globals and functions so we can emit dlimports first
        StringBuilder code_sb;
        code_sb.allocator = allocator;
        scope(exit) code_sb.cleanup();
        StringBuilder* main_sb = sb;
        sb = &code_sb;

        // Generate global variables (skip extern objects - they're dlimported)
        bool emitted_any_vars = false;
        foreach (ref gvar; unit.globals) {
            // Track global type for all globals (even extern)
            global_types[gvar.name.lexeme] = gvar.var_type;

            // Skip extern objects - they're defined in external libraries
            if (gvar.is_extern) continue;

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
            emitted_any_vars = true;
        }
        if (emitted_any_vars) sb.write("\n");

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

        // Build function lookup map for inline function resolution
        Table!(str, CFunction*) func_map;
        func_map.data.allocator = allocator;
        scope(exit) func_map.cleanup();
        foreach (ref func; unit.functions) {
            func_map[func.name.lexeme] = &func;
        }

        // First pass: generate non-inline function definitions
        foreach (ref func; unit.functions) {
            if (func.is_definition && !func.is_inline) {
                if (func.name.lexeme == "main") has_main = true;
                if (func.name.lexeme == "start") has_start = true;
                generated_funcs[func.name.lexeme] = true;
                int err = gen_function(&func);
                if (err) return err;
            }
        }

        // Iteratively generate inline functions that are called
        // Keep going until no new inline functions are needed
        bool made_progress = true;
        while (made_progress) {
            made_progress = false;
            foreach (ref item; called_funcs.items()) {
                str fname = item.key;
                // Skip if already generated
                if (fname in generated_funcs) continue;
                // Find the function
                if (auto fp = fname in func_map) {
                    CFunction* func = *fp;
                    // Only generate if it's an inline definition
                    if (func.is_definition && func.is_inline) {
                        generated_funcs[fname] = true;
                        int err = gen_function(func);
                        if (err) return err;
                        made_progress = true;  // Generated a new function, may have added more calls
                    }
                }
            }
        }

        // If there's a main() but no start(), generate start wrapper
        if (has_main && !has_start) {
            sb.write("function start 0\n");
            sb.write("    call function main 0\n");
            sb.write("    ret\n");
            sb.write("end\n");
        }

        // Switch back to main buffer
        sb = main_sb;

        // Generate dlimport blocks for used external symbols (functions and objects)
        // Collect symbols by library first, then emit
        if (used_funcs.count > 0 || used_objs.count > 0) {
            // Build a list of (library, symbol list) pairs
            // We'll collect function/object info per library
            Table!(str, bool) lib_has_symbols;
            lib_has_symbols.data.allocator = allocator;
            scope(exit) lib_has_symbols.cleanup();

            // First pass: determine which libraries have symbols to emit
            foreach (ref func; unit.functions) {
                str fname = func.name.lexeme;
                if (fname !in used_funcs) continue;
                if (func.is_definition) continue;
                if (func.is_static) continue;
                if (func.return_type is null) continue;

                str lib = normalize_lib(func.library);
                if (lib == "libSDL2.so" && (fname.length < 4 || fname[0..4] != "SDL_")) {
                    lib = DEFAULT_LIBC;
                }
                lib_has_symbols[lib] = true;
            }

            foreach (ref gvar; unit.globals) {
                str oname = gvar.name.lexeme;
                if (oname !in used_objs) continue;
                if (!gvar.is_extern) continue;

                str lib = normalize_lib(gvar.library);
                lib_has_symbols[lib] = true;
            }

            // Second pass: emit dlimport blocks per library
            bool first_lib = true;
            foreach (item; lib_has_symbols.items) {
                str lib = item.key;
                if (!first_lib) sb.write("\n");
                first_lib = false;

                str alias_name = lib_to_alias[lib];
                sb.writef("dlimport %\n", alias_name);
                sb.writef("  \"%\"\n", lib);

                // Emit functions from this library
                foreach (ref func; unit.functions) {
                    str fname = func.name.lexeme;
                    if (fname !in used_funcs) continue;
                    if (func.is_definition) continue;
                    if (func.is_static) continue;
                    if (func.return_type is null) continue;

                    str flib = normalize_lib(func.library);
                    if (flib == "libSDL2.so" && (fname.length < 4 || fname[0..4] != "SDL_")) {
                        flib = DEFAULT_LIBC;
                    }
                    if (flib != lib) continue;

                    ubyte n_ret = func.return_type.is_void() ? 0 : 1;
                    auto n_params = func.params.length > 8 ? 8 : func.params.length;
                    sb.writef("  % % %", fname, n_params, n_ret);
                    if (func.is_varargs) sb.write(" varargs");
                    sb.write("\n");
                }

                // Emit objects from this library
                foreach (ref gvar; unit.globals) {
                    str oname = gvar.name.lexeme;
                    if (oname !in used_objs) continue;
                    if (!gvar.is_extern) continue;

                    str olib = normalize_lib(gvar.library);
                    if (olib != lib) continue;

                    sb.writef("  % object\n", oname);
                }

                sb.write("end\n");
            }
            if (lib_has_symbols.count > 0) sb.write("\n");
        }

        // Append the code (globals and functions)
        sb.write(code_sb.borrow());

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
            if (label_table.count > 0) label_table.cleanup();  // Reset label mappings for new function
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
            // Count compound literal slots
            num_stack_slots += analyzer.compound_literal_slots;
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
            case DO_WHILE: return gen_do_while(cast(CDoWhileStmt*)stmt);
            case FOR:      return gen_for(cast(CForStmt*)stmt);
            case BLOCK:    return gen_block(cast(CBlock*)stmt);
            case VAR_DECL: return gen_var_decl(cast(CVarDecl*)stmt);
            case BREAK:    return gen_break(cast(CBreakStmt*)stmt);
            case CONTINUE: return gen_continue(cast(CContinueStmt*)stmt);
            case EMPTY:    return 0;
            case SWITCH:   return gen_switch(cast(CSwitchStmt*)stmt);
            case GOTO:     return gen_goto(cast(CGotoStmt*)stmt);
            case LABEL:    return gen_label(cast(CLabelStmt*)stmt);
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

    int gen_do_while(CDoWhileStmt* stmt) {
        int prev_continue = current_continue_target;
        int prev_break = current_break_target;
        scope(exit) {
            current_continue_target = prev_continue;
            current_break_target = prev_break;
        }

        int top_label = labelallocator.allocate();
        int cond_label = labelallocator.allocate();
        int after_label = labelallocator.allocate();
        current_continue_target = cond_label;  // continue goes to condition check
        current_break_target = after_label;

        // Body executes first (at least once)
        sb.writef("  label L%\n", top_label);

        int err = gen_statement(stmt.body);
        if (err) return err;

        // Condition check
        sb.writef("  label L%\n", cond_label);
        int before = regallocator.alloced;
        int cond = regallocator.allocate();
        err = gen_expression(stmt.condition, cond);
        if (err) return err;
        regallocator.reset_to(before);

        sb.writef("    cmp r% 0\n", cond);
        sb.writef("    jump ne label L%\n", top_label);  // Loop if condition is true

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

    int gen_switch(CSwitchStmt* stmt) {
        int prev_break = current_break_target;
        scope(exit) {
            current_break_target = prev_break;
        }

        int end_label = labelallocator.allocate();
        current_break_target = end_label;

        // Evaluate switch expression once
        int before = regallocator.alloced;
        int cond_reg = regallocator.allocate();
        int err = gen_expression(stmt.condition, cond_reg);
        if (err) return err;

        // Allocate labels for each case
        auto case_labels = make_barray!int(allocator);
        int default_label = -1;  // -1 means no default

        foreach (ref c; stmt.cases) {
            int lbl = labelallocator.allocate();
            case_labels ~= lbl;
            if (c.is_default) {
                default_label = lbl;
            }
        }

        // Generate jump table: compare and jump to matching case
        foreach (i, ref c; stmt.cases) {
            if (!c.is_default) {
                // Generate comparison
                int case_reg = regallocator.allocate();
                err = gen_expression(c.case_value, case_reg);
                if (err) return err;

                sb.writef("    cmp r% r%\n", cond_reg, case_reg);
                sb.writef("    jump eq label L%\n", case_labels[i]);
                regallocator.reset_to(before + 1);  // Keep cond_reg
            }
        }

        // If no case matched, jump to default or end
        if (default_label >= 0) {
            sb.writef("    move rip label L%\n", default_label);
        } else {
            sb.writef("    move rip label L%\n", end_label);
        }

        regallocator.reset_to(before);

        // Generate case bodies (fallthrough behavior)
        foreach (i, ref c; stmt.cases) {
            sb.writef("  label L%\n", case_labels[i]);
            foreach (s; c.statements) {
                err = gen_statement(s);
                if (err) return err;
            }
            // Note: no implicit jump here - fallthrough to next case
        }

        sb.writef("  label L%\n", end_label);
        return 0;
    }

    // Get or allocate a label number for a named label
    int get_label_number(str name) {
        if (auto p = name in label_table) {
            return *p;
        }
        int lbl = labelallocator.allocate();
        label_table[name] = lbl;
        return lbl;
    }

    int gen_goto(CGotoStmt* stmt) {
        str label_name = stmt.label.lexeme;
        int lbl = get_label_number(label_name);
        sb.writef("    move rip label L%\n", lbl);
        return 0;
    }

    int gen_label(CLabelStmt* stmt) {
        str label_name = stmt.label.lexeme;
        int lbl = get_label_number(label_name);
        sb.writef("  label L%\n", lbl);

        // Generate the statement following the label
        if (stmt.statement !is null) {
            return gen_statement(stmt.statement);
        }
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

                // Handle struct initializer if present
                if (stmt.initializer !is null) {
                    if (CInitList* init_list = stmt.initializer.as_init_list()) {
                        before = regallocator.alloced;
                        addr_reg = regallocator.allocate();
                        int val_reg = regallocator.allocate();

                        // Get struct field info
                        auto fields = stmt.var_type.fields;
                        size_t num_elems = init_list.elements.length;
                        // Only limit when there are no designators (pure positional)
                        // With designators, multiple elements can target sub-fields of same field
                        bool has_designators = false;
                        foreach (e; init_list.elements) {
                            if (e.designators.length > 0) {
                                has_designators = true;
                                break;
                            }
                        }
                        if (!has_designators && num_elems > fields.length) num_elems = fields.length;

                        size_t field_idx = 0;
                        for (size_t i = 0; i < num_elems; i++) {
                            auto elem = init_list.elements[i];

                            size_t offset;
                            CType* field_type;
                            bool is_chained = false;

                            // Handle designators
                            if (elem.designators.length > 0) {
                                // Traverse all designators to compute final offset and type
                                offset = 0;
                                CType* current_type = stmt.var_type;

                                foreach (di, desig; elem.designators) {
                                    if (desig.kind == CDesignatorKind.FIELD) {
                                        if (current_type.kind != CTypeKind.STRUCT) {
                                            error(desig.token, "Field designator on non-struct type");
                                            return 1;
                                        }
                                        auto cur_fields = current_type.fields;
                                        bool found = false;
                                        foreach (f; cur_fields) {
                                            if (f.name == desig.field_name) {
                                                offset += f.offset;
                                                current_type = f.type;
                                                // Update field_idx for continuation (only for first designator)
                                                if (di == 0) {
                                                    for (size_t fi = 0; fi < fields.length; fi++) {
                                                        if (fields[fi].name == desig.field_name) {
                                                            field_idx = fi + 1;
                                                            break;
                                                        }
                                                    }
                                                }
                                                found = true;
                                                break;
                                            }
                                        }
                                        if (!found) {
                                            error(desig.token, "Unknown field in designator");
                                            return 1;
                                        }
                                    } else { // INDEX
                                        if (!current_type.is_array()) {
                                            error(desig.token, "Index designator on non-array type");
                                            return 1;
                                        }
                                        offset += desig.index_value * current_type.pointed_to.size_of();
                                        current_type = current_type.pointed_to;
                                    }
                                }

                                field_type = current_type;
                                is_chained = elem.designators.length > 1;
                            } else {
                                // Positional: use field_idx
                                if (field_idx >= fields.length) break;
                                offset = fields[field_idx].offset;
                                field_type = fields[field_idx].type;
                                field_idx++;
                            }

                            size_t field_size = field_type.size_of();

                            // Check if field value is a nested init list (for nested struct)
                            if (CInitList* nested_init = elem.value.as_init_list()) {
                                if (field_type.kind == CTypeKind.STRUCT) {
                                    // Initialize nested struct field
                                    auto nested_fields = field_type.fields;
                                    size_t num_nested = nested_init.elements.length;
                                    if (num_nested > nested_fields.length) num_nested = nested_fields.length;

                                    size_t nested_field_idx = 0;
                                    for (size_t j = 0; j < num_nested; j++) {
                                        auto nested_elem = nested_init.elements[j];

                                        // Handle field designators in nested init
                                        if (nested_elem.designators.length > 0) {
                                            if (nested_elem.designators[0].kind != CDesignatorKind.FIELD) {
                                                error(nested_elem.designators[0].token, "Expected field designator");
                                                return 1;
                                            }
                                            str fname = nested_elem.designators[0].field_name;
                                            bool found = false;
                                            for (size_t fi = 0; fi < nested_fields.length; fi++) {
                                                if (nested_fields[fi].name == fname) {
                                                    nested_field_idx = fi;
                                                    found = true;
                                                    break;
                                                }
                                            }
                                            if (!found) {
                                                error(nested_elem.designators[0].token, "Unknown field");
                                                return 1;
                                            }
                                        }

                                        if (nested_field_idx >= nested_fields.length) break;

                                        int err = gen_expression(nested_elem.value, val_reg);
                                        if (err) return err;

                                        size_t nested_offset = offset + nested_fields[nested_field_idx].offset;
                                        size_t nested_size = nested_fields[nested_field_idx].type.size_of();
                                        int nested_slot = slot + cast(int)(nested_offset / 8);
                                        sb.writef("    add r% rbp %\n", addr_reg, P(nested_slot));
                                        if (nested_offset % 8 != 0) {
                                            sb.writef("    add r% r% %\n", addr_reg, addr_reg, nested_offset % 8);
                                        }
                                        sb.writef("    % r% r%\n", write_instr_for_size(nested_size), addr_reg, val_reg);
                                        nested_field_idx++;
                                    }
                                } else {
                                    error(elem.value.token, "Nested initializer for non-struct field");
                                    return 1;
                                }
                            } else if (field_type.kind == CTypeKind.STRUCT && needs_memcpy(field_size)) {
                                // Large struct field - need memcpy
                                int src_reg = val_reg;
                                int err = gen_struct_address(elem.value, src_reg);
                                if (err) return err;
                                int field_slot = slot + cast(int)(offset / 8);
                                sb.writef("    add r% rbp %\n", addr_reg, P(field_slot));
                                if (offset % 8 != 0) {
                                    sb.writef("    add r% r% %\n", addr_reg, addr_reg, offset % 8);
                                }
                                sb.writef("    memcpy r% r% %\n", addr_reg, src_reg, field_size);
                            } else {
                                // Scalar field (or small struct <= 8 bytes)
                                int err = gen_expression(elem.value, val_reg);
                                if (err) return err;
                                int field_slot = slot + cast(int)(offset / 8);
                                sb.writef("    add r% rbp %\n", addr_reg, P(field_slot));
                                if (offset % 8 != 0) {
                                    sb.writef("    add r% r% %\n", addr_reg, addr_reg, offset % 8);
                                }
                                sb.writef("    % r% r%\n", write_instr_for_size(field_size), addr_reg, val_reg);
                            }
                        }

                        regallocator.reset_to(before);
                    } else {
                        // Not an init list - try to copy from another struct
                        size_t struct_size = stmt.var_type.size_of();
                        before = regallocator.alloced;
                        int dst_reg = regallocator.allocate();
                        int src_reg = regallocator.allocate();

                        // dst = address of this struct
                        sb.writef("    add r% rbp %\n", dst_reg, P(slot));

                        // src = address of source struct
                        int err = gen_struct_address(stmt.initializer, src_reg);
                        if (err) return err;

                        // memcpy
                        sb.writef("    memcpy r% r% %\n", dst_reg, src_reg, struct_size);

                        regallocator.reset_to(before);
                    }
                }
            } else if (is_array) {
                // Arrays need multiple slots
                size_t arr_size = stmt.var_type.array_size;
                stack_offset += cast(int)arr_size;

                // Handle array initializer
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
                            error(stmt.stmt.token, "Array initializers must be string literals or initializer lists");
                            return 1;
                        }
                    } else if (CInitList* init_list = stmt.initializer.as_init_list()) {
                        // Brace-enclosed initializer list for array
                        int before = regallocator.alloced;
                        int addr_reg = regallocator.allocate();
                        int val_reg = regallocator.allocate();

                        // Get address of array on stack
                        sb.writef("    add r% rbp %\n", addr_reg, P(slot));
                        // Zero-initialize the array first
                        sb.writef("    memzero r% %\n", addr_reg, arr_size * stmt.var_type.element_size());

                        // Get element size
                        size_t elem_size = stmt.var_type.element_size();

                        // Write each element
                        size_t num_elems = init_list.elements.length;

                        size_t current_idx = 0;
                        for (size_t i = 0; i < num_elems; i++) {
                            auto elem = init_list.elements[i];

                            size_t offset;
                            CType* target_type;
                            bool is_chained = false;

                            // Handle designators
                            if (elem.designators.length > 0) {
                                // Traverse all designators to compute final offset and type
                                offset = 0;
                                CType* current_type = stmt.var_type;

                                foreach (di, desig; elem.designators) {
                                    if (desig.kind == CDesignatorKind.INDEX) {
                                        if (!current_type.is_array()) {
                                            error(desig.token, "Index designator on non-array type");
                                            return 1;
                                        }
                                        size_t idx = cast(size_t)desig.index_value;
                                        offset += idx * current_type.pointed_to.size_of();
                                        current_type = current_type.pointed_to;
                                        // Update current_idx for continuation (only for first designator)
                                        if (di == 0) {
                                            current_idx = idx + 1;
                                        }
                                    } else { // FIELD
                                        if (current_type.kind != CTypeKind.STRUCT) {
                                            error(desig.token, "Field designator on non-struct type");
                                            return 1;
                                        }
                                        auto cur_fields = current_type.fields;
                                        bool found = false;
                                        foreach (f; cur_fields) {
                                            if (f.name == desig.field_name) {
                                                offset += f.offset;
                                                current_type = f.type;
                                                found = true;
                                                break;
                                            }
                                        }
                                        if (!found) {
                                            error(desig.token, "Unknown field in designator");
                                            return 1;
                                        }
                                    }
                                }

                                target_type = current_type;
                                is_chained = elem.designators.length > 1;
                            } else {
                                // Positional: use current_idx
                                if (current_idx >= arr_size) break;
                                offset = current_idx * elem_size;
                                target_type = stmt.var_type.pointed_to;
                                current_idx++;
                            }

                            int target_slot = slot + cast(int)(offset / 8);
                            size_t target_size = target_type.size_of();

                            // For chained designators, initialize directly at target
                            if (is_chained) {
                                if (target_type.kind == CTypeKind.STRUCT && needs_memcpy(target_size)) {
                                    int src_reg = val_reg;
                                    int err = gen_struct_address(elem.value, src_reg);
                                    if (err) return err;
                                    sb.writef("    add r% rbp %\n", addr_reg, P(target_slot));
                                    if (offset % 8 != 0) {
                                        sb.writef("    add r% r% %\n", addr_reg, addr_reg, offset % 8);
                                    }
                                    sb.writef("    memcpy r% r% %\n", addr_reg, src_reg, target_size);
                                } else {
                                    int err = gen_expression(elem.value, val_reg);
                                    if (err) return err;
                                    sb.writef("    add r% rbp %\n", addr_reg, P(target_slot));
                                    if (offset % 8 != 0) {
                                        sb.writef("    add r% r% %\n", addr_reg, addr_reg, offset % 8);
                                    }
                                    sb.writef("    % r% r%\n", write_instr_for_size(target_size), addr_reg, val_reg);
                                }
                                continue;
                            }

                            // Check if element is a nested init list (for array of structs)
                            if (CInitList* nested_init = elem.value.as_init_list()) {
                                CType* elem_type = stmt.var_type.pointed_to;
                                if (elem_type.kind == CTypeKind.STRUCT) {
                                    // Initialize nested struct at this array position
                                    auto fields = elem_type.fields;
                                    size_t num_nested = nested_init.elements.length;
                                    if (num_nested > fields.length) num_nested = fields.length;

                                    size_t field_idx = 0;
                                    for (size_t j = 0; j < num_nested; j++) {
                                        auto nested_elem = nested_init.elements[j];

                                        // Handle field designators in nested init
                                        if (nested_elem.designators.length > 0) {
                                            if (nested_elem.designators[0].kind != CDesignatorKind.FIELD) {
                                                error(nested_elem.designators[0].token, "Expected field designator");
                                                return 1;
                                            }
                                            str field_name = nested_elem.designators[0].field_name;
                                            bool found = false;
                                            for (size_t fi = 0; fi < fields.length; fi++) {
                                                if (fields[fi].name == field_name) {
                                                    field_idx = fi;
                                                    found = true;
                                                    break;
                                                }
                                            }
                                            if (!found) {
                                                error(nested_elem.designators[0].token, "Unknown field");
                                                return 1;
                                            }
                                        }

                                        if (field_idx >= fields.length) break;

                                        int err = gen_expression(nested_elem.value, val_reg);
                                        if (err) return err;

                                        size_t field_offset = offset + fields[field_idx].offset;
                                        size_t field_size = fields[field_idx].type.size_of();
                                        int field_slot = slot + cast(int)(field_offset / 8);
                                        sb.writef("    add r% rbp %\n", addr_reg, P(field_slot));
                                        if (field_offset % 8 != 0) {
                                            sb.writef("    add r% r% %\n", addr_reg, addr_reg, field_offset % 8);
                                        }
                                        sb.writef("    % r% r%\n", write_instr_for_size(field_size), addr_reg, val_reg);
                                        field_idx++;
                                    }
                                } else {
                                    error(elem.value.token, "Nested initializer for non-struct array element");
                                    return 1;
                                }
                            } else {
                                if (target_type.kind == CTypeKind.STRUCT && needs_memcpy(target_size)) {
                                    // Large struct element - need memcpy
                                    int src_reg = val_reg;
                                    int err = gen_struct_address(elem.value, src_reg);
                                    if (err) return err;
                                    sb.writef("    add r% rbp %\n", addr_reg, P(target_slot));
                                    if (offset % 8 != 0) {
                                        sb.writef("    add r% r% %\n", addr_reg, addr_reg, offset % 8);
                                    }
                                    sb.writef("    memcpy r% r% %\n", addr_reg, src_reg, target_size);
                                } else {
                                    // Scalar element (or small struct <= 8 bytes)
                                    int err = gen_expression(elem.value, val_reg);
                                    if (err) return err;
                                    sb.writef("    add r% rbp %\n", addr_reg, P(target_slot));
                                    if (offset % 8 != 0) {
                                        sb.writef("    add r% r% %\n", addr_reg, addr_reg, offset % 8);
                                    }
                                    sb.writef("    % r% r%\n", write_instr_for_size(target_size), addr_reg, val_reg);
                                }
                            }
                            // Note: current_idx is already updated in the designator/positional handling above
                        }

                        regallocator.reset_to(before);
                    } else {
                        error(stmt.stmt.token, "Array initializers must be string literals or initializer lists");
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
            case CAST:       return gen_cast(cast(CCast*)e, target);
            case MEMBER_ACCESS: return gen_member_access(cast(CMemberAccess*)e, target);
            case SIZEOF:     return gen_sizeof(cast(CSizeof*)e, target);
            case ALIGNOF:    return gen_alignof(cast(CAlignof*)e, target);
            case COUNTOF:    return gen_countof(cast(CCountof*)e, target);
            case VA_ARG:     return gen_va_arg(cast(CVaArg*)e, target);
            case TERNARY:    return gen_ternary(cast(CTernary*)e, target);
            case INIT_LIST:
                // Init lists are handled specially in gen_var_decl
                error(e.token, "Initializer list cannot be used as expression");
                return 1;
            case COMPOUND_LITERAL:
                return gen_compound_literal(cast(CCompoundLiteral*)e, target);
            case GENERIC:
                return gen_generic(cast(CGeneric*)e, target);
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
            // Hex literal - strip suffix (u, U, l, L) if present
            str hex_val = lex;
            while (hex_val.length > 0) {
                ubyte last = hex_val[$ - 1];
                if (last == 'u' || last == 'U' || last == 'l' || last == 'L') {
                    hex_val = hex_val[0 .. $ - 1];
                } else {
                    break;
                }
            }
            sb.writef("    move r% %\n", target, hex_val);
        } else {
            // Integer literal - strip suffix (u, U, l, L) if present
            str int_val = lex;
            while (int_val.length > 0) {
                ubyte last = int_val[$ - 1];
                if (last == 'u' || last == 'U' || last == 'l' || last == 'L') {
                    int_val = int_val[0 .. $ - 1];
                } else {
                    break;
                }
            }
            sb.writef("    move r% %\n", target, int_val);
        }

        return 0;
    }

    int gen_cast(CCast* expr, int target) {
        // Cast just generates the operand - type conversion is implicit at this level
        return gen_expression(expr.operand, target);
    }

    int gen_compound_literal(CCompoundLiteral* expr, int target) {
        // Compound literal: (type){...}
        // Use pre-allocated stack space and initialize, return address
        CType* lit_type = expr.literal_type;
        size_t size = lit_type.size_of();
        size_t num_slots = (size + 7) / 8;

        // Use next available stack slot (space was pre-allocated in function prologue)
        int slot = stack_offset;
        stack_offset += cast(int)num_slots;

        int before = regallocator.alloced;
        int addr_reg = regallocator.allocate();
        int val_reg = regallocator.allocate();

        // Zero-initialize
        sb.writef("    add r% rbp %\n", addr_reg, P(slot));
        sb.writef("    memzero r% %\n", addr_reg, size);

        // Handle initializer
        CExpr* init_expr = expr.initializer();
        if (init_expr !is null) {
            if (CInitList* init_list = init_expr.as_init_list()) {
                if (lit_type.is_struct_or_union()) {
                    // Initialize struct fields (with flattening for array fields)
                    auto fields = lit_type.fields;
                    size_t num_elems = init_list.elements.length;
                    size_t field_idx = 0;
                    size_t array_elem_idx = 0;  // For flattening into array fields

                    for (size_t i = 0; i < num_elems; i++) {
                        auto elem = init_list.elements[i];
                        size_t offset;
                        size_t write_size;

                        // Handle designators
                        if (elem.designators.length > 0) {
                            if (elem.designators[0].kind == CDesignatorKind.FIELD) {
                                str fname = elem.designators[0].field_name;
                                bool found = false;
                                foreach (fi, f; fields) {
                                    if (f.name == fname) {
                                        field_idx = fi;
                                        array_elem_idx = 0;
                                        found = true;
                                        break;
                                    }
                                }
                                if (!found) continue;
                            } else {
                                continue;  // Skip non-field designators in struct
                            }
                        }

                        // Check if we've exhausted all fields
                        if (field_idx >= fields.length) break;

                        auto field = &fields[field_idx];

                        // Check if field is an array and value is scalar (flattening)
                        if (field.type.is_array() && elem.value.as_init_list() is null) {
                            // Flatten scalar into array field
                            size_t elem_size = field.type.element_size();
                            size_t array_size = field.type.array_size;

                            if (array_elem_idx >= array_size) {
                                // Array is full, move to next field
                                field_idx++;
                                array_elem_idx = 0;
                                if (field_idx >= fields.length) break;
                                field = &fields[field_idx];
                            }

                            offset = field.offset + array_elem_idx * elem_size;
                            write_size = elem_size;
                            array_elem_idx++;
                        } else {
                            // Regular field or nested init_list
                            offset = field.offset;
                            write_size = field.type.size_of();
                            field_idx++;
                            array_elem_idx = 0;
                        }

                        int err = gen_expression(elem.value, val_reg);
                        if (err) return err;

                        int field_slot = slot + cast(int)(offset / 8);
                        sb.writef("    add r% rbp %\n", addr_reg, P(field_slot));
                        if (offset % 8 != 0) {
                            sb.writef("    add r% r% %\n", addr_reg, addr_reg, offset % 8);
                        }
                        sb.writef("    % r% r%\n", write_instr_for_size(write_size), addr_reg, val_reg);
                    }
                } else if (lit_type.is_array()) {
                    // Initialize array elements
                    size_t elem_size = lit_type.element_size();
                    size_t num_elems = init_list.elements.length;
                    size_t current_idx = 0;

                    for (size_t i = 0; i < num_elems; i++) {
                        auto elem = init_list.elements[i];
                        size_t offset = current_idx * elem_size;

                        int err = gen_expression(elem.value, val_reg);
                        if (err) return err;

                        int elem_slot = slot + cast(int)(offset / 8);
                        sb.writef("    add r% rbp %\n", addr_reg, P(elem_slot));
                        if (offset % 8 != 0) {
                            sb.writef("    add r% r% %\n", addr_reg, addr_reg, offset % 8);
                        }
                        sb.writef("    % r% r%\n", write_instr_for_size(elem_size), addr_reg, val_reg);
                        current_idx++;
                    }
                }
            }
        }

        regallocator.reset_to(before);

        // Return address of the compound literal
        if (target != TARGET_IS_NOTHING) {
            sb.writef("    add r% rbp %\n", target, P(slot));
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
        // Track if it's an extern object
        if (name in extern_objs) {
            used_objs[name] = true;
        }

        // Get the global's type for sized read
        if (CType** gtype = name in global_types) {
            size_t var_size = (*gtype) ? (*gtype).size_of() : 8;
            bool is_unsigned = (*gtype) ? (*gtype).is_unsigned : true;
            // Get address into a temp register, then do sized read
            int before = regallocator.alloced;
            int addr_reg = regallocator.allocate();
            // For extern objects, use qualified name with module alias
            if (str* mod_alias = name in extern_objs) {
                sb.writef("    move r% var %.%\n", addr_reg, *mod_alias, name);
            } else {
                sb.writef("    move r% var %\n", addr_reg, name);
            }
            sb.writef("    % r% r%\n", read_instr_for_size(var_size, is_unsigned), target, addr_reg);
            regallocator.reset_to(before);
        } else {
            // Unknown global (function pointer?) - use regular read
            if (str* mod_alias = name in extern_objs) {
                sb.writef("    read r% var %.%\n", target, *mod_alias, name);
            } else {
                sb.writef("    read r% var %\n", target, name);
            }
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

        // Comma operator: evaluate left for side effects, return right value
        if (expr.op == CTokenType.COMMA) {
            int before = regallocator.alloced;
            int tmp = regallocator.allocate();
            int err = gen_expression(expr.left, tmp);
            regallocator.reset_to(before);
            if (err) return err;
            return gen_expression(expr.right, target);
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

        // Check for literal RHS optimization (skip for && and || which need short-circuit)
        CExpr* right = expr.right;
        if (CLiteral* lit = right.as_literal()) {
            // Short-circuit operators can't use literal optimization
            if (expr.op == CTokenType.AMP_AMP || expr.op == CTokenType.PIPE_PIPE)
                goto general_case;
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

    general_case:
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
                // Short-circuit AND (lhs may equal target, so compare before overwriting)
                int after = labelallocator.allocate();
                sb.writef("    cmp r% 0\n", lhs);
                sb.writef("    move r% 0\n", target);
                sb.writef("    jump eq label L%\n", after);
                sb.writef("    cmp r% 0\n", rhs);
                sb.writef("    cmov ne r% 1\n", target);
                sb.writef("  label L%\n", after);
                break;
            case PIPE_PIPE:
                // Short-circuit OR (lhs may equal target, so compare before overwriting)
                int after2 = labelallocator.allocate();
                sb.writef("    cmp r% 0\n", lhs);
                sb.writef("    move r% 1\n", target);
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
                    // Global variable - track extern object usage
                    if (name in extern_objs) {
                        used_objs[name] = true;
                    }
                    // For extern objects, use qualified name with module alias
                    if (str* mod_alias = name in extern_objs) {
                        sb.writef("    move r% var %.%\n", target, *mod_alias, name);
                    } else {
                        sb.writef("    move r% var %\n", target, name);
                    }
                }
                return 0;
            }

            // Address of member access: &p.x or &pp->x
            if (CMemberAccess* ma = expr.operand.as_member_access()) {
                if (target == TARGET_IS_NOTHING) return 0;

                int before = regallocator.alloced;
                int addr_reg = regallocator.allocate();

                // Get the struct type
                CType* obj_type = get_expr_type(ma.object);
                if (obj_type is null) {
                    error(expr.expr.token, "Cannot determine type for member access");
                    return 1;
                }

                CType* struct_type = obj_type;
                if (ma.is_arrow) {
                    if (!obj_type.is_pointer()) {
                        error(expr.expr.token, "'->' requires pointer type");
                        return 1;
                    }
                    struct_type = obj_type.pointed_to;
                }

                if (struct_type is null || !struct_type.is_struct_or_union()) {
                    error(expr.expr.token, "Member access requires struct/union type");
                    return 1;
                }

                // Find the field offset
                StructField* field = struct_type.get_field(ma.member.lexeme);
                if (field is null) {
                    error(expr.expr.token, "Unknown field");
                    return 1;
                }

                if (ma.is_arrow) {
                    // &pp->x: get pointer value, add offset
                    int err = gen_expression(ma.object, addr_reg);
                    if (err) return err;
                } else {
                    // &p.x: get address of struct, add offset
                    int err = gen_struct_address(ma.object, addr_reg);
                    if (err) return err;
                }

                // Add field offset
                if (field.offset != 0) {
                    sb.writef("    add r% r% %\n", target, addr_reg, field.offset);
                } else {
                    sb.writef("    move r% r%\n", target, addr_reg);
                }

                regallocator.reset_to(before);
                return 0;
            }

            // Address of array subscript: &arr[i]
            if (CSubscript* sub = expr.operand.as_subscript()) {
                if (target == TARGET_IS_NOTHING) return 0;
                // Just compute the address without dereferencing
                return gen_subscript_address(sub, target);
            }

            // Address of compound literal: &(type){...}
            // gen_compound_literal already returns the address
            if (expr.operand.as_compound_literal() !is null) {
                return gen_expression(expr.operand, target);
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
                // Get the pointed-to type's size and signedness for proper sized read
                CType* ptr_type = get_expr_type(expr.operand);
                size_t elem_size = (ptr_type && ptr_type.is_pointer()) ? ptr_type.element_size() : 8;
                CType* elem_type = (ptr_type && ptr_type.is_pointer()) ? ptr_type.element_type() : null;
                bool is_unsigned = elem_type ? elem_type.is_unsigned : true;
                sb.writef("    % r% r%\n", read_instr_for_size(elem_size, is_unsigned), target, ptr_reg);
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
                used_funcs[id.name.lexeme] = true;  // Mark as used
                sb.writef("    call function %.% %\n", *mod_alias, id.name.lexeme, total_args);
            } else {
                called_funcs[id.name.lexeme] = true;  // Track internal call (for inline function generation)
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
                        case PERCENT_EQUAL:
                            sb.writef("    div rjunk r% r% r%\n", *r, *r, rhs_reg);
                            break;
                        case AMP_EQUAL:
                            sb.writef("    and r% r% r%\n", *r, *r, rhs_reg);
                            break;
                        case PIPE_EQUAL:
                            sb.writef("    or r% r% r%\n", *r, *r, rhs_reg);
                            break;
                        case CARET_EQUAL:
                            sb.writef("    xor r% r% r%\n", *r, *r, rhs_reg);
                            break;
                        case LESS_LESS_EQUAL:
                            sb.writef("    shl r% r% r%\n", *r, *r, rhs_reg);
                            break;
                        case GREATER_GREATER_EQUAL:
                            sb.writef("    shr r% r% r%\n", *r, *r, rhs_reg);
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
                    } else if (val.kind == CExprKind.ASSIGN) {
                        // Source is another assignment (chained: a = b = c)
                        // Generate the inner assignment, which returns address of its destination
                        int err = gen_expression(val, src_reg);
                        if (err) return err;
                        // src_reg now contains address of the inner assignment's destination
                    } else {
                        // Try gen_struct_address for other expressions (member access, etc.)
                        int err = gen_struct_address(val, src_reg);
                        if (err) return err;
                    }

                    // memcpy dst src size
                    size_t struct_size = (*var_type_ptr).size_of();
                    sb.writef("    memcpy r% r% %\n", dst_reg, src_reg, struct_size);

                    // Return address of destination for chained assignment
                    if (target != TARGET_IS_NOTHING) {
                        sb.writef("    move r% r%\n", target, dst_reg);
                    }

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
                        case PERCENT_EQUAL:
                            sb.writef("    div rjunk r% r% r%\n", val_reg, cur_reg, val_reg);
                            break;
                        case AMP_EQUAL:
                            sb.writef("    and r% r% r%\n", val_reg, cur_reg, val_reg);
                            break;
                        case PIPE_EQUAL:
                            sb.writef("    or r% r% r%\n", val_reg, cur_reg, val_reg);
                            break;
                        case CARET_EQUAL:
                            sb.writef("    xor r% r% r%\n", val_reg, cur_reg, val_reg);
                            break;
                        case LESS_LESS_EQUAL:
                            sb.writef("    shl r% r% r%\n", val_reg, cur_reg, val_reg);
                            break;
                        case GREATER_GREATER_EQUAL:
                            sb.writef("    shr r% r% r%\n", val_reg, cur_reg, val_reg);
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
                // Track extern object usage
                if (name in extern_objs) {
                    used_objs[name] = true;
                }

                int before = regallocator.alloced;
                int addr_reg = regallocator.allocate();
                int val_reg = regallocator.allocate();

                // Get address of global and its size
                // For extern objects, use qualified name with module alias
                if (str* mod_alias = name in extern_objs) {
                    sb.writef("    move r% var %.%\n", addr_reg, *mod_alias, name);
                } else {
                    sb.writef("    move r% var %\n", addr_reg, name);
                }
                size_t var_size = (*gtype) ? (*gtype).size_of() : 8;
                bool is_unsigned = (*gtype) ? (*gtype).is_unsigned : true;

                if (expr.op == CTokenType.EQUAL) {
                    // Simple assignment
                    int err = gen_expression(expr.value, val_reg);
                    if (err) return err;
                } else {
                    // Compound assignment - read current value first
                    int cur_reg = regallocator.allocate();
                    sb.writef("    % r% r%\n", read_instr_for_size(var_size, is_unsigned), cur_reg, addr_reg);

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
                        case PERCENT_EQUAL:
                            sb.writef("    div rjunk r% r% r%\n", val_reg, cur_reg, val_reg);
                            break;
                        case AMP_EQUAL:
                            sb.writef("    and r% r% r%\n", val_reg, cur_reg, val_reg);
                            break;
                        case PIPE_EQUAL:
                            sb.writef("    or r% r% r%\n", val_reg, cur_reg, val_reg);
                            break;
                        case CARET_EQUAL:
                            sb.writef("    xor r% r% r%\n", val_reg, cur_reg, val_reg);
                            break;
                        case LESS_LESS_EQUAL:
                            sb.writef("    shl r% r% r%\n", val_reg, cur_reg, val_reg);
                            break;
                        case GREATER_GREATER_EQUAL:
                            sb.writef("    shr r% r% r%\n", val_reg, cur_reg, val_reg);
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
                int err = gen_struct_address(ma.object, addr_reg);
                if (err) return err;
            }

            // Add field offset
            if (field.offset > 0) {
                sb.writef("    add r% r% %\n", addr_reg, addr_reg, field.offset);
            }

            // Write to field
            size_t field_size = field.type.size_of();
            if (field.type.is_struct_or_union() && needs_memcpy(field_size)) {
                // Struct/union field that needs memcpy
                int err = gen_struct_address(expr.value, val_reg);
                if (err) return err;
                sb.writef("    memcpy r% r% %\n", addr_reg, val_reg, field_size);
            } else {
                // Scalar or small struct field
                int err = gen_expression(expr.value, val_reg);
                if (err) return err;
                sb.writef("    % r% r%\n", write_instr_for_size(field_size), addr_reg, val_reg);
            }

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
        CType* elem_type = (arr_type && (arr_type.is_pointer() || arr_type.is_array())) ? arr_type.element_type() : null;
        bool is_unsigned = elem_type ? elem_type.is_unsigned : true;
        if (elem_size > 1) {
            sb.writef("    mul r% r% %\n", idx_reg, idx_reg, elem_size);
        }
        sb.writef("    add r% r% r%\n", arr_reg, arr_reg, idx_reg);

        if (target != TARGET_IS_NOTHING) {
            sb.writef("    % r% r%\n", read_instr_for_size(elem_size, is_unsigned), target, arr_reg);
        }

        regallocator.reset_to(before);
        return 0;
    }

    // Generate address of array subscript (for &arr[i])
    int gen_subscript_address(CSubscript* expr, int target) {
        int before = regallocator.alloced;
        int arr_reg = regallocator.allocate();
        int idx_reg = regallocator.allocate();

        int err = gen_expression(expr.array, arr_reg);
        if (err) return err;

        err = gen_expression(expr.index, idx_reg);
        if (err) return err;

        // Scale index by element size
        CType* arr_type = get_expr_type(expr.array);
        size_t elem_size = (arr_type && (arr_type.is_pointer() || arr_type.is_array())) ? arr_type.element_size() : 1;
        if (elem_size > 1) {
            sb.writef("    mul r% r% %\n", idx_reg, idx_reg, elem_size);
        }
        sb.writef("    add r% r% r%\n", target, arr_reg, idx_reg);

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
            // Use gen_struct_address which handles nested member access recursively
            int err = gen_struct_address(expr.object, obj_reg);
            if (err) return err;
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
                bool is_unsigned = field.type.is_unsigned;
                sb.writef("    % r% r%\n", read_instr_for_size(field_size, is_unsigned), target, obj_reg);
            }
        }

        regallocator.reset_to(before);
        return 0;
    }

    int gen_sizeof(CSizeof* expr, int target) {
        if (target == TARGET_IS_NOTHING) return 0;

        size_t size = expr.size;
        if (size == 0 && expr.sizeof_expr !is null) {
            // sizeof expr - compute size from expression type
            CType* t = get_expr_type(expr.sizeof_expr);
            size = t ? t.size_of() : 8;
        }

        sb.writef("    move r% %\n", target, size);
        return 0;
    }

    int gen_alignof(CAlignof* expr, int target) {
        if (target == TARGET_IS_NOTHING) return 0;

        size_t alignment = expr.alignment;
        if (alignment == 0 && expr.alignof_expr !is null) {
            // _Alignof(expr) - compute alignment from expression type
            CType* t = get_expr_type(expr.alignof_expr);
            alignment = t ? t.align_of() : 8;
        }

        sb.writef("    move r% %\n", target, alignment);
        return 0;
    }

    int gen_countof(CCountof* expr, int target) {
        if (target == TARGET_IS_NOTHING) return 0;

        size_t count = expr.count;
        if (count == 0 && expr.countof_expr !is null) {
            // _Countof(expr) - compute count from expression type
            CType* t = get_expr_type(expr.countof_expr);
            if (t !is null && t.is_array()) {
                count = t.array_size;
            } else {
                error(expr.expr.token, "_Countof requires an array expression");
                return 1;
            }
        }

        sb.writef("    move r% %\n", target, count);
        return 0;
    }

    int gen_generic(CGeneric* expr, int target) {
        // _Generic is resolved at compile time
        CType* ctrl_type = get_expr_type(expr.controlling);
        CExpr* result = resolve_generic(expr, ctrl_type);
        if (result is null) {
            error(expr.expr.token, "No matching type in _Generic");
            return 1;
        }
        return gen_expression(result, target);
    }

    int gen_va_arg(CVaArg* expr, int target) {
        if (target == TARGET_IS_NOTHING) return 0;

        // va_arg(ap, type): read value from va_list pointer and advance it
        // va_list is a pointer to the varargs on stack
        // We read the value and advance the pointer by the size of the type

        // Get va_list pointer into a register
        int va_reg = regallocator.allocate();
        int err = gen_expression(expr.va_list_expr, va_reg);
        if (err) return err;

        // Read value from va_list (pointer to pointer - need to dereference)
        sb.writef("    read r% r%\n", va_reg, va_reg);  // Load current va_list position
        sb.writef("    read r% r%\n", target, va_reg);  // Load the value

        // Advance va_list by size of type
        size_t type_size = expr.arg_type ? expr.arg_type.size_of() : 8;
        if (type_size < 8) type_size = 8;  // Arguments are at least word-sized

        // We need to write back the advanced pointer
        // But we've already dereferenced... this is tricky
        // For now, just return the value - proper va_arg handling would need
        // to track the va_list variable and update it

        regallocator.reset_to(regallocator.alloced - 1);
        return 0;
    }

    int gen_ternary(CTernary* expr, int target) {
        // condition ? if_true : if_false
        int else_label = labelallocator.allocate();
        int after_label = labelallocator.allocate();

        // Generate condition
        int before = regallocator.alloced;
        int cond = (target == TARGET_IS_NOTHING) ? regallocator.allocate() : target;
        int err = gen_expression(expr.condition, cond);
        if (err) return err;

        sb.writef("    cmp r% 0\n", cond);
        sb.writef("    jump eq label L%\n", else_label);  // Jump to else if condition is false

        // Generate true branch
        err = gen_expression(expr.if_true, target);
        if (err) return err;

        sb.writef("    move rip label L%\n", after_label);  // Skip else branch

        // Generate false branch
        sb.writef("  label L%\n", else_label);
        err = gen_expression(expr.if_false, target);
        if (err) return err;

        sb.writef("  label L%\n", after_label);
        regallocator.reset_to(before);
        return 0;
    }

    // Generate code to get the address of a struct expression (for pass-by-value)
    int gen_struct_address(CExpr* e, int target) {
        e = e.ungroup();

        // Identifier - get address of struct variable
        if (CIdentifier* id = e.as_identifier()) {
            str name = id.name.lexeme;
            // Local variable on stack
            if (int* offset = name in stacklocals) {
                sb.writef("    add r% rbp %\n", target, P(*offset));
                return 0;
            }
            // Global or extern variable
            if (name in global_types || name in extern_objs) {
                if (name in extern_objs) {
                    used_objs[name] = true;
                }
                if (str* mod_alias = name in extern_objs) {
                    sb.writef("    move r% var %.%\n", target, *mod_alias, name);
                } else {
                    sb.writef("    move r% var %\n", target, name);
                }
                return 0;
            }
            error(id.name, "Cannot take address of unknown variable for struct pass-by-value");
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
                // Recursively get address of the object
                int err = gen_struct_address(ma.object, target);
                if (err) return err;
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

        // Compound literal - gen_expression returns the address
        if (e.kind == CExprKind.COMPOUND_LITERAL) {
            return gen_expression(e, target);
        }

        error(e.token, "Cannot pass this expression as struct/union by value");
        return 1;
    }
}
