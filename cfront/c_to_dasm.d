/*
 * C to DASM Code Generator for ddasm
 * Copyright 2025, David Priver
 */
module cfront.c_to_dasm;

import core.stdc.stdio : fprintf, stderr;
import dlib.aliases;

// Platform-specific default C library name
version(OSX){
    enum str DEFAULT_LIBC = "libSystem.B.dylib";
    enum str DEFAULT_LIBM = "libSystem.B.dylib";
} else version(linux){
    enum str DEFAULT_LIBC = "libc.so.6";
    enum str DEFAULT_LIBM = "libm.so.6";
} else {
    enum str DEFAULT_LIBC = "libc";
    enum str DEFAULT_LIBM = "libc";
}

// Normalize library name - "libc" is a special alias for the platform's C library
str normalize_lib(str lib){
    if(lib == "c") return DEFAULT_LIBC;
    if(lib == "m") return DEFAULT_LIBM;
    if(lib == "libm") return DEFAULT_LIBM;
    if(lib == "libc") return DEFAULT_LIBC;
    // Also handle common libc variants
    if(lib == "libc.so.6") return DEFAULT_LIBC;
    if(lib == "libm.so.6") return DEFAULT_LIBM;
    if(lib == "libSystem.B.dylib") return DEFAULT_LIBC;
    return lib;
}
import dlib.allocator : Allocator;
import dlib.barray : Barray, make_barray;
import dlib.stringbuilder : StringBuilder, P, H;
import dlib.table : Table;
import dlib.parse_numbers : parse_unsigned_human;

import cfront.c_pp_to_c : CToken, CTokenType;
import cfront.c_ast;
import cfront.c_const_eval : try_eval_constant, ConstValue, EnumTable;

struct RegisterAllocator {
    int alloced = 0;

    int allocate(){
        int result = alloced++;
        return result;
    }

    void reset(){ alloced = 0; }
    void reset_to(int r){ alloced = r; }
}

struct LabelAllocator {
    int nalloced;
    int allocate(){ return nalloced++; }
    void reset(){ nalloced = 0; }
}

// Scope tracking for variable shadowing support
// When entering a block, we save any variable mappings that will be shadowed.
// When exiting, we restore them.
struct ShadowedVar {
    str name;
    bool had_reg;
    int reg_value;
    bool had_stack;
    int stack_value;
}

// Analysis pass to detect which variables need stack allocation
struct CAnalyzer {
    Table!(str, bool) addr_taken;  // Variables whose address is taken
    Table!(str, bool) arrays;      // Array variables (always need stack)
    Table!(str, bool) structs;     // Struct variables (always need stack)
    Table!(str, CType*) local_var_types;  // Local variable types (for function pointer analysis)
    Barray!(str) all_vars;         // All local variable names
    Allocator allocator;
    int compound_literal_slots;    // Total slots needed for compound literals
    bool has_calls;                // Whether function contains any calls (non-leaf)
    int max_call_slots = 0;

    void analyze_function(CFunction* func){
        addr_taken.data.allocator = allocator;
        arrays.data.allocator = allocator;
        structs.data.allocator = allocator;
        local_var_types.data.allocator = allocator;
        all_vars.bdata.allocator = allocator;
        addr_taken.cleanup();
        arrays.cleanup();
        structs.cleanup();
        local_var_types.cleanup();
        all_vars.clear();
        compound_literal_slots = 0;
        has_calls = false;

        foreach(stmt; func.body){
            analyze_stmt(stmt);
        }
    }

    bool should_use_stack(){
        // Use stack if any address is taken, any arrays, structs, compound literals, or more than 4 variables
        return addr_taken.count > 0 || arrays.count > 0 || structs.count > 0 || compound_literal_slots > 0 || all_vars[].length > 4;
    }

    bool is_leaf(){
        return !has_calls;
    }

    void analyze_stmt(CStmt* stmt){
        if(stmt is null) return;
        final switch(stmt.kind) with (CStmtKind){
            case EXPR:
                analyze_expr((cast(CExprStmt*)stmt).expression);
                break;
            case RETURN:
                if((cast(CReturnStmt*)stmt).value)
                    analyze_expr((cast(CReturnStmt*)stmt).value);
                break;
            case IF:
                auto s = cast(CIfStmt*)stmt;
                analyze_expr(s.condition);
                analyze_stmt(s.then_branch);
                if(s.else_branch) analyze_stmt(s.else_branch);
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
                if(s.init_stmt) analyze_stmt(s.init_stmt);
                if(s.condition) analyze_expr(s.condition);
                if(s.increment) analyze_expr(s.increment);
                analyze_stmt(s.body_);
                break;
            case BLOCK:
                foreach(s; (cast(CBlock*)stmt).statements)
                    analyze_stmt(s);
                break;
            case VAR_DECL:
                auto decl = cast(CVarDecl*)stmt;
                all_vars.push(decl.name.lexeme);  // Track variable name
                // Track variable type for function pointer analysis
                if(decl.var_type){
                    local_var_types[decl.name.lexeme] = decl.var_type;
                }
                // Track array variables
                if(decl.var_type && decl.var_type.is_array()){
                    arrays[decl.name.lexeme] = true;
                }
                // Track struct/union variables
                if(decl.var_type && decl.var_type.is_struct_or_union()){
                    structs[decl.name.lexeme] = true;
                }
                if(decl.initializer)
                    analyze_expr(decl.initializer);
                break;
            case SWITCH:
                auto s = cast(CSwitchStmt*)stmt;
                analyze_expr(s.condition);
                analyze_stmt(s.body_);
                break;
            case GOTO:
                break;  // goto doesn't need analysis
            case LABEL:
                auto l = cast(CLabelStmt*)stmt;
                analyze_stmt(l.statement);
                break;
            case CASE_LABEL:
                auto c = cast(CCaseLabelStmt*)stmt;
                if(c.case_value) analyze_expr(c.case_value);
                analyze_stmt(c.statement);
                break;
            case BREAK:
            case CONTINUE:
            case EMPTY:
            case DASM:
            case ASM:
                break;
        }
    }

    void analyze_expr(CExpr* expr){
        if(expr is null) return;
        expr = expr.ungroup();

        final switch(expr.kind) with (CExprKind){
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
                if(e.op == CTokenType.AMP){
                    if(auto id = e.operand.as_identifier()){
                        addr_taken[id.name.lexeme] = true;
                    }
                }
                analyze_expr(e.operand);
                break;
            case CALL:
                CCall *e = expr.as_call;
                // inline analysis
                {
                    has_calls = true;  // Mark as non-leaf function
                    CType *func = e.callee_function_type();
                    int slots = 0;
                    assert(func.return_type);
                    if(func.return_type.is_struct_or_union() && !fits_in_registers(func.return_type)){
                        slots += 1;  // Hidden return pointer
                    }
                    foreach(CType* arg; func.param_types){
                        // small structs can take more than one slot, otherwise they
                        // are passed via a pointer.
                        if(arg.is_struct_or_union() && fits_in_registers(arg))
                            slots += struct_return_regs(arg);
                        else
                            slots++;
                    }
                    if(slots > max_call_slots) max_call_slots = slots;
                }
                analyze_expr(e.callee);
                foreach(arg; e.args)
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
                if(auto se = cast(CSizeof*)expr)
                    if(se.sizeof_expr) analyze_expr(se.sizeof_expr);
                break;
            case ALIGNOF:
                break;
            case COUNTOF:
                if(auto ce = cast(CCountof*)expr)
                    if(ce.countof_expr) analyze_expr(ce.countof_expr);
                break;
            case VA_ARG:
                analyze_expr((cast(CVaArg*)expr).va_list_expr);
                break;
            case GENERIC:
                auto e = cast(CGeneric*)expr;
                analyze_expr(e.picked);
                break;
            case TERNARY:
                auto e = cast(CTernary*)expr;
                analyze_expr(e.condition);
                analyze_expr(e.if_true);
                analyze_expr(e.if_false);
                break;
            case INIT_LIST:
                auto e = cast(CInitList*)expr;
                foreach(elem; e.elements)
                    analyze_expr(elem.value);
                break;
            case COMPOUND_LITERAL:
                auto e = cast(CCompoundLiteral*)expr;
                // Count slots needed for this compound literal
                size_t size = e.literal_type.size_of();
                compound_literal_slots += cast(int)((size + 7) / 8);
                analyze_expr(e.initializer);
                break;
            case EMBED:
                break;  // Nothing to analyze
            case STMT_EXPR:
                auto e = cast(CStmtExpr*)expr;
                foreach(stmt; e.statements)
                    analyze_stmt(stmt);
                break;
        }
    }

    void cleanup(){
        addr_taken.cleanup();
        arrays.cleanup();
        structs.cleanup();
        local_var_types.cleanup();
        all_vars.cleanup();
    }
}

// Check if a struct/union type can be returned in registers (1-2 registers for <= 16 bytes)
bool fits_in_registers(CType* t){
    return t.size_of() <= 16;
}
// Get number of registers needed to return a struct/union (0, 1, or 2)
int struct_return_regs(CType* t){
    if(t is null || !t.is_struct_or_union()) return 0;
    size_t size = t.size_of();
    if(size == 0) return 0;
    if(size <= 8) return 1;
    if(size <= 16) return 2;
    return 0;  // Too big, use hidden pointer
}

// Check if type is a Homogeneous Floating-Point Aggregate (HFA)
// HFA = struct with 1-4 members, all same floating-point type (ARM64 ABI)
// Note: ARM64 HFAs can be up to 32 bytes (4 doubles), unlike x86-64's 16 byte limit
bool is_float_hfa(CType* t){
    if(t is null || !t.is_struct_or_union()) return false;
    if(t.fields.length == 0 || t.fields.length > 4) return false;
    // Check all fields are same float type (float or double, not mixed)
    CTypeKind first_kind = t.fields[0].type.kind;
    if(first_kind != CTypeKind.FLOAT && first_kind != CTypeKind.DOUBLE) return false;
    foreach(ref field; t.fields){
        if(field.type is null) return false;
        if(field.type.kind != first_kind) return false;
    }
    return true;
}

// Get number of float/double members in an HFA
int count_hfa_members(CType* t){
    return cast(int)t.fields.length;
}

// Check if HFA contains doubles (vs floats)
bool is_double_hfa(CType* t){
    if(t.fields.length == 0) return false;
    return t.fields[0].type.kind == CTypeKind.DOUBLE;
}

// Get number of DVM register slots an HFA occupies
// Float HFA: (N * 4 bytes + 7) / 8 = ceil(N/2) slots
// Double HFA: N * 8 bytes / 8 = N slots
int hfa_slots(CType* t){
    if(t.fields.length == 0) return 0;
    if(is_double_hfa(t)){
        return cast(int)t.fields.length;  // Each double is 8 bytes = 1 slot
    } else {
        // Float: 4 bytes each, packed into 8-byte slots
        return (cast(int)t.fields.length + 1) / 2;
    }
}



// Get the read instruction for a given type size and signedness
str read_instr_for_size(size_t size, bool is_signed){
    if(!is_signed){
        switch(size){
            case 1: return "read1";
            case 2: return "read2";
            case 4: return "read4";
            default: return "read";
        }
    } else {
        switch(size){
            case 1: return "sread1";
            case 2: return "sread2";
            case 4: return "sread4";
            default: return "read";
        }
    }
}

// Get the write instruction for a given type size
str write_instr_for_size(size_t size){
    switch(size){
        case 1: return "write1";
        case 2: return "write2";
        case 4: return "write4";
        default: return "write";
    }
}

// Check if a size can be written with a single write instruction (1, 2, 4, or 8 bytes)
bool needs_memcpy(size_t size){
    return size != 1 && size != 2 && size != 4 && size != 8;
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

    // Scope tracking for variable shadowing
    Barray!(Barray!(ShadowedVar)) scope_stack;  // Stack of scopes, each containing shadowed vars
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
    Table!(str, CFunction*) func_info;     // Function info (for varargs check)

    // Loop control
    int current_continue_target = -1;
    int current_break_target = -1;

    // Goto/label support
    Table!(str, int) label_table;  // Map label names to label numbers

    // Switch case label tracking (for Duff's device support)
    // Parallel arrays: case statements and their label numbers
    Barray!(CCaseLabelStmt*) case_label_stmts;
    Barray!(int) case_label_nums;

    // Current function info
    CType* current_return_type = null;  // Return type of current function
    bool returns_struct = false;         // True if current function returns a struct
    bool uses_hidden_return_ptr = false; // True if struct is too big for registers (> 16 bytes)
    int return_ptr_slot = -1;            // Stack slot for hidden return pointer (large struct returns)

    bool ERROR_OCCURRED = false;
    bool use_stack = false;  // Set true when we need stack-based locals
    bool is_leaf_func = false;  // True if function makes no calls (can use R0-R7 freely)
    int n_param_regs = 0;    // Number of registers used by params (for leaf: R0-R(n-1), else R8-R(8+n-1))
    int funcdepth = 0;
    int stack_offset = 1;    // Current stack offset for locals (start at 1, slot 0 is saved RBP)

    enum { TARGET_IS_NOTHING = -1 }
    enum { RARG1 = 0 }  // First argument register (rarg1 = r0)
    enum N_REG_ARGS = 8;  // Number of register arguments (rarg1-rarg8 = r0-r7)
    // Note: Return value uses named register 'rout1', not a numeric register

    @disable this();

    this(StringBuilder* s, Allocator a){
        allocator = a;
        sb = s;
        reglocals.data.allocator = a;
        stacklocals.data.allocator = a;
        var_types.data.allocator = a;
        scope_stack.bdata.allocator = a;
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
        case_label_stmts.bdata.allocator = a;
        case_label_nums.bdata.allocator = a;
    }

    void cleanup(){
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

    // Scope management for variable shadowing
    void push_var_scope(){
        auto new_scope = make_barray!ShadowedVar(allocator);
        scope_stack ~= new_scope;
    }

    void pop_var_scope(){
        if(scope_stack.count == 0) return;

        // Restore any shadowed variables
        auto current = scope_stack.pop();
        foreach(ref sv; current[]){
            // Restore the outer variable's mapping (overwrites the inner one)
            if(sv.had_reg){
                reglocals[sv.name] = sv.reg_value;
            }
            if(sv.had_stack){
                stacklocals[sv.name] = sv.stack_value;
            }
        }
    }

    // Save current mapping for a variable that's about to be shadowed
    void save_shadowed_var(str name){
        if(scope_stack.count == 0) return;

        ShadowedVar sv;
        sv.name = name;
        sv.had_reg = false;
        sv.had_stack = false;

        if(auto r = name in reglocals){
            sv.had_reg = true;
            sv.reg_value = *r;
        }
        if(auto s = name in stacklocals){
            sv.had_stack = true;
            sv.stack_value = *s;
        }

        // Only save if the variable existed before (it's being shadowed)
        if(sv.had_reg || sv.had_stack){
            scope_stack.bdata.data[scope_stack.count - 1] ~= sv;
        }
    }

    void error(CToken token, str message){
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
    void errorf(A...)(CToken token, A args){
        StringBuilder sb = {allocator:allocator};
        scope(exit) sb.cleanup;
        sb.FORMAT(token.file, ':', token.line, ':',token.column,": Code gen error at '", token.lexeme, "': ");
        foreach(a; args)
            sb.write(a);
        sb.write('\n');
        if(token.expansion_file.length){
            sb.FORMAT("  note: expanded from macro defined at ", token.expansion_file, ':', token.expansion_line, ':', token.expansion_column, '\n');
        }
        str s = sb.borrow;
        fprintf(stderr, "%.*s\n", cast(int)s.length, s.ptr);
    }

    // Generate a module alias from library name
    // "libc.so.6" -> "Libc", "libfoo.so" -> "Libfoo", "/path/to/libbar.dylib" -> "Libbar"
    str make_alias(str lib, int counter){
        // Strip directory path
        str name = lib;
        foreach_reverse (i, c; lib){
            if(c == '/' || c == '\\'){
                name = lib[i+1 .. $];
                break;
            }
        }

        // Strip .so, .dylib, .dll and version suffixes like .so.6
        foreach(i, c; name){
            if(c == '.'){
                name = name[0 .. i];
                break;
            }
        }

        StringBuilder sb;
        sb.allocator = allocator;

        if(name.length == 0){
            sb.writef("Lib%", counter);
        } else {
            // Capitalize first letter
            if(name[0] >= 'a' && name[0] <= 'z'){
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

    int generate(CTranslationUnit* unit){
        // Collect unique libraries and assign aliases for extern declarations
        Table!(str, str) lib_to_alias;
        lib_to_alias.data.allocator = allocator;
        scope(exit) lib_to_alias.cleanup();

        // Build extern_funcs from function declarations (not definitions)
        int lib_counter = 0;
        foreach(ref func; unit.functions){
            // Skip definitions and static functions
            if(func.is_definition) continue;
            if(func.is_static) continue;

            str fname = func.name.lexeme;
            // Normalize library name and handle SDL special case
            str lib = normalize_lib(func.library);
            if(lib == "libSDL2.so" && (fname.length < 4 || fname[0..4] != "SDL_")){
                lib = DEFAULT_LIBC;
            }
            if(lib !in lib_to_alias){
                // Generate alias from library name
                str alias_name = make_alias(lib, lib_counter++);
                lib_to_alias[lib] = alias_name;
            }
            // Track function -> module alias mapping
            extern_funcs[fname] = lib_to_alias[lib];
        }

        // Build extern_objs from extern global variable declarations
        foreach(ref gvar; unit.globals){
            if(!gvar.is_extern) continue;

            str oname = gvar.name.lexeme;
            str lib = normalize_lib(gvar.library);
            if(lib !in lib_to_alias){
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
        foreach(ref gvar; unit.globals){
            // Track global type for all globals (even extern)
            global_types[gvar.name.lexeme] = gvar.var_type;

            // Skip extern objects - they're defined in external libraries
            if(gvar.is_extern) continue;

            // Generate var declaration with new syntax: var name N init... end
            if(int err = gen_global_var(&gvar))
                return err;
            emitted_any_vars = true;
        }
        if(emitted_any_vars) sb.write("\n");

        // First pass: collect function info for struct return and varargs handling
        // Include both definitions and declarations (for extern varargs functions)
        func_return_types.data.allocator = allocator;
        func_info.data.allocator = allocator;
        foreach(ref func; unit.functions){
            func_return_types[func.name.lexeme] = func.return_type;
            func_info[func.name.lexeme] = &func;
        }

        // Generate functions and track if we have main/start
        bool has_main = false;
        bool has_start = false;

        // Build function lookup map for inline function resolution
        Table!(str, CFunction*) func_map;
        func_map.data.allocator = allocator;
        scope(exit) func_map.cleanup();
        foreach(ref func; unit.functions){
            func_map[func.name.lexeme] = &func;
        }

        // First pass: generate non-inline function definitions
        foreach(ref func; unit.functions){
            if(func.is_definition && !func.is_inline){
                if(func.name.lexeme == "main") has_main = true;
                if(func.name.lexeme == "start") has_start = true;
                generated_funcs[func.name.lexeme] = true;
                int err = gen_function(&func);
                if(err) return err;
            }
        }

        // Iteratively generate inline functions that are called
        // Keep going until no new inline functions are needed
        bool made_progress = true;
        while(made_progress){
            made_progress = false;
            foreach(ref item; called_funcs.items()){
                str fname = item.key;
                // Skip if already generated
                if(fname in generated_funcs) continue;
                // Find the function
                if(auto fp = fname in func_map){
                    CFunction* func = *fp;
                    // Only generate if it's an inline definition
                    if(func.is_definition && func.is_inline){
                        generated_funcs[fname] = true;
                        int err = gen_function(func);
                        if(err) return err;
                        made_progress = true;  // Generated a new function, may have added more calls
                    }
                }
            }
        }

        // If there's a main() but no start(), generate start wrapper
        if(has_main && !has_start){
            sb.write("import misc\n");
            sb.write("function start 0\n");
            sb.write("    call function main 0\n");
            sb.write("    move rarg1 rout1\n");
            sb.write("    call function misc.exit 1\n");
            sb.write("    ret\n");
            sb.write("end\n");
        }

        // Switch back to main buffer
        sb = main_sb;

        // Generate dlimport blocks for used external symbols (functions and objects)
        // Collect symbols by library first, then emit
        if(used_funcs.count > 0 || used_objs.count > 0){
            // Build a list of (library, symbol list) pairs
            // We'll collect function/object info per library
            Table!(str, bool) lib_has_symbols;
            lib_has_symbols.data.allocator = allocator;
            scope(exit) lib_has_symbols.cleanup();

            // First pass: determine which libraries have symbols to emit
            foreach(ref func; unit.functions){
                str fname = func.name.lexeme;
                if(fname !in used_funcs) continue;
                if(func.is_definition) continue;
                if(func.is_static) continue;
                if(func.return_type is null) continue;

                str lib = normalize_lib(func.library);
                if(lib == "libSDL2.so" && (fname.length < 4 || fname[0..4] != "SDL_")){
                    lib = DEFAULT_LIBC;
                }
                lib_has_symbols[lib] = true;
            }

            foreach(ref gvar; unit.globals){
                str oname = gvar.name.lexeme;
                if(oname !in used_objs) continue;
                if(!gvar.is_extern) continue;

                str lib = normalize_lib(gvar.library);
                lib_has_symbols[lib] = true;
            }

            // Second pass: emit dlimport blocks per library
            bool first_lib = true;
            foreach(item; lib_has_symbols.items){
                str lib = item.key;
                if(!first_lib) sb.write("\n");
                first_lib = false;

                str alias_name = lib_to_alias[lib];
                sb.writef("dlimport %\n", alias_name);
                sb.writef("  path \"%\"\n", lib);

                // Emit functions from this library
                foreach(ref func; unit.functions){
                    str fname = func.name.lexeme;
                    if(fname !in used_funcs) continue;
                    if(func.is_definition) continue;
                    if(func.is_static) continue;
                    if(func.return_type is null) continue;

                    str flib = normalize_lib(func.library);
                    if(flib == "libSDL2.so" && (fname.length < 4 || fname[0..4] != "SDL_")){
                        flib = DEFAULT_LIBC;
                    }
                    if(flib != lib) continue;

                    // Check if this function uses hidden pointer for struct return
                    bool uses_hidden_ptr = func.return_type.is_struct_or_union() &&
                                          !fits_in_registers(func.return_type);

                    // Calculate number of return registers
                    // For structs that fit in registers, they may use 1-2 return registers
                    ubyte n_ret = 0;
                    if(!func.return_type.is_void()){
                        if(func.return_type.is_struct_or_union() && fits_in_registers(func.return_type)){
                            n_ret = cast(ubyte)struct_return_regs(func.return_type);
                        } else if(!uses_hidden_ptr){
                            n_ret = 1;
                        }
                    }

                    // Count register slots, not just parameters
                    // Struct params that fit in registers take 1-2 slots
                    int total_slots = uses_hidden_ptr ? 1 : 0;
                    foreach(ref param; func.params){
                        if(param.type && param.type.is_struct_or_union() && fits_in_registers(param.type)){
                            total_slots += struct_return_regs(param.type);
                        } else {
                            total_slots++;
                        }
                    }
                    auto n_params = total_slots > 8 ? 8 : total_slots;

                    // Compute float arg mask (2 bits per param: 00=int, 01=float32, 10=double)
                    // Account for struct params taking multiple slots
                    // ARM64 vs x86-64 differences:
                    //   - ARM64: only HFAs use FP registers, non-HFA structs use integer registers
                    //   - x86-64: per-eightword classification (float-only eightwords use XMM)
                    uint float_arg_mask = 0;
                    int slot_idx = uses_hidden_ptr ? 1 : 0;
                    foreach(ref param; func.params){
                        if(slot_idx >= 8) break;
                        if(param.type && param.type.is_struct_or_union() && fits_in_registers(param.type)){
                            int num_slots = struct_return_regs(param.type);
                            // On ARM64, HFAs are handled via struct_arg_sizes, and non-HFA
                            // structs use integer registers (don't set float bits).
                            // On x86-64, we classify each eightword independently.
                            version(AArch64) {
                                // ARM64: HFA float args are handled by trampoline via struct_arg_sizes
                                // Non-HFA structs use integer registers, so leave float_arg_mask = 0
                                slot_idx += num_slots;
                            } else {
                                // x86-64: Classify each eightword (8-byte chunk) of the struct
                                // per System V ABI: each eightword is classified independently
                                for(int slot = 0; slot < num_slots && slot_idx + slot < 8; slot++){
                                    size_t slot_start = slot * 8;
                                    size_t slot_end = slot_start + 8;
                                    bool slot_has_float = false;
                                    bool slot_has_int = false;
                                    foreach(ref field; param.type.fields){
                                        if(field.type is null) continue;
                                        size_t field_end = field.offset + field.type.size_of();
                                        // Check if field overlaps with this slot
                                        if(field.offset < slot_end && field_end > slot_start){
                                            if(field.type.is_float())
                                                slot_has_float = true;
                                            else
                                                slot_has_int = true;
                                        }
                                    }
                                    // If slot has only floats (no ints), use XMM
                                    if(slot_has_float && !slot_has_int){
                                        float_arg_mask |= (0b01 << ((slot_idx + slot) * 2));
                                    }
                                    // Otherwise leave as integer (mask stays 0 for this slot)
                                }
                                slot_idx += num_slots;
                            }
                        } else if(param.type && param.type.is_float()){
                            if(param.type.kind == CTypeKind.FLOAT)
                                float_arg_mask |= (0b01 << (slot_idx * 2));  // float32
                            else
                                float_arg_mask |= (0b10 << (slot_idx * 2));  // double
                            slot_idx++;
                        } else {
                            slot_idx++;
                        }
                    }

                    // Compute float return mask (0x0=int, 0x1=float32, 0x2=double)
                    // For structs with only floats, they're returned in XMM registers
                    // For ARM64 HFAs, each float member uses a separate FP register
                    uint float_ret_mask = 0;
                    if(func.return_type.is_float()){
                        if(func.return_type.kind == CTypeKind.FLOAT)
                            float_ret_mask = 0x1;  // float32
                        else
                            float_ret_mask = 0x2;  // double
                    } else if(func.return_type.is_struct_or_union() && is_float_hfa(func.return_type)){
                        // HFA return: set a bit for each float/double member (ARM64)
                        // Note: HFAs can be up to 32 bytes (4 doubles), larger than x86-64's 16 byte limit
                        int n_members = count_hfa_members(func.return_type);
                        bool is_double = is_double_hfa(func.return_type);
                        for(int slot = 0; slot < n_members; slot++){
                            if(is_double)
                                float_ret_mask |= (0x2 << (slot * 2));  // double in this slot
                            else
                                float_ret_mask |= (0x1 << (slot * 2));  // float32 in this slot
                        }
                    } else if(func.return_type.is_struct_or_union() && fits_in_registers(func.return_type)){
                        // Non-HFA struct that fits in registers
                        // ARM64: always use integer registers (x0/x1), leave float_ret_mask = 0
                        // x86-64: classify each eightword independently per System V ABI
                        version(AArch64) {
                            // ARM64: non-HFA structs always return in integer registers
                            // float_ret_mask stays 0
                        } else {
                            int num_ret_regs = struct_return_regs(func.return_type);
                            for(int slot = 0; slot < num_ret_regs; slot++){
                                // Check what types are in this eightword
                                size_t slot_start = slot * 8;
                                size_t slot_end = slot_start + 8;
                                bool slot_has_float = false;
                                bool slot_has_int = false;
                                foreach(ref field; func.return_type.fields){
                                    size_t field_end = field.offset + field.type.size_of();
                                    // Check if field overlaps with this slot
                                    if(field.offset < slot_end && field_end > slot_start){
                                        if(field.type.is_float())
                                            slot_has_float = true;
                                        else
                                            slot_has_int = true;
                                    }
                                }
                                // If slot has only floats (no ints), use XMM
                                if(slot_has_float && !slot_has_int){
                                    float_ret_mask |= (0x1 << (slot * 2));  // float32 in this slot
                                }
                                // Otherwise use integer register (mask stays 0 for this slot)
                            }
                        }
                    }

                    // Compute struct_arg_sizes for struct args
                    // Encoding:
                    //   0 = not a struct
                    //   1-4 = HFA with N float members (ARM64: expand into N FP registers)
                    //   5-8 = HFA with 1-4 double members (ARM64: 4 + count)
                    //   9 = hidden return pointer (ARM64: goes in x8, not x0)
                    //   >16 = large struct size (x86_64/ARM64: copy to native stack)
                    ushort[16] struct_arg_sizes;
                    bool has_struct_args = false;
                    int struct_slot_idx = 0;
                    if(uses_hidden_ptr){
                        // ARM64: hidden return pointer goes in x8, mark with special code 9
                        struct_arg_sizes[0] = 9;
                        has_struct_args = true;
                        struct_slot_idx = 1;
                    }
                    foreach(ref param; func.params){
                        if(struct_slot_idx >= n_params) break;
                        if(param.type && param.type.is_struct_or_union()){
                            // Check for HFA first - ARM64 HFAs can be up to 32 bytes (4 doubles)
                            if(is_float_hfa(param.type)){
                                // HFA: encode member count for trampoline to expand
                                int hfa_members = count_hfa_members(param.type);
                                int num_slots = hfa_slots(param.type);
                                if(is_double_hfa(param.type)){
                                    // Double HFA: encode as 4 + count (5-8)
                                    struct_arg_sizes[struct_slot_idx] = cast(ushort)(4 + hfa_members);
                                } else {
                                    // Float HFA: encode as count (1-4)
                                    struct_arg_sizes[struct_slot_idx] = cast(ushort)hfa_members;
                                }
                                has_struct_args = true;
                                struct_slot_idx++;
                                // Skip additional DVM slots for multi-slot HFAs
                                for(int i = 1; i < num_slots && struct_slot_idx < n_params; i++){
                                    struct_arg_sizes[struct_slot_idx++] = 0;
                                }
                            } else if(fits_in_registers(param.type)){
                                // Non-HFA small struct - no special handling needed
                                int num_slots = struct_return_regs(param.type);
                                for(int i = 0; i < num_slots && struct_slot_idx < n_params; i++){
                                    struct_arg_sizes[struct_slot_idx++] = 0;
                                }
                            } else {
                                // Large struct - needs to be copied to native stack
                                struct_arg_sizes[struct_slot_idx] = cast(ushort)param.type.size_of();
                                has_struct_args = true;
                                struct_slot_idx++;
                            }
                        } else {
                            struct_arg_sizes[struct_slot_idx++] = 0;
                        }
                    }

                    sb.writef("  function % % %", fname, n_params, n_ret);
                    if(func.is_varargs) sb.write(" varargs");
                    if(float_arg_mask != 0 || float_ret_mask != 0 || has_struct_args){
                        sb.writef(" %", H(float_arg_mask));
                        if(float_ret_mask != 0 || has_struct_args)
                            sb.writef(" %", H(float_ret_mask));
                    }
                    if(has_struct_args){
                        sb.write(" struct_args [");
                        for(int i = 0; i < n_params; i++){
                            if(i > 0) sb.write(" ");
                            sb.writef("%", struct_arg_sizes[i]);
                        }
                        sb.write("]");
                    }
                    sb.write("\n");
                }

                // Emit objects from this library
                foreach(ref gvar; unit.globals){
                    str oname = gvar.name.lexeme;
                    if(oname !in used_objs) continue;
                    if(!gvar.is_extern) continue;

                    str olib = normalize_lib(gvar.library);
                    if(olib != lib) continue;

                    sb.writef("  var %\n", oname);
                }

                sb.write("end\n");
            }
            if(lib_has_symbols.count > 0) sb.write("\n");
        }

        // Append the code (globals and functions)
        sb.write(code_sb.borrow());

        return 0;
    }

    // =========================================================================
    // Helper Functions
    // =========================================================================

    // Parse a character literal lexeme (e.g., "'q'" or "'\n'") and return its integer value
    static int parse_char_literal(str lex){
        if(lex.length < 3) return 0;
        char c = lex[1];  // Skip opening quote
        if(c == '\\' && lex.length >= 4){
            switch(lex[2]){
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
    static int count_var_slots(CStmt*[] stmts){
        int count = 0;
        foreach(stmt; stmts){
            if(stmt.kind == CStmtKind.VAR_DECL){
                auto decl = cast(CVarDecl*)stmt;
                count += cast(int)decl.var_type.stack_slots();
            } else if(stmt.kind == CStmtKind.BLOCK){
                count += count_var_slots((cast(CBlock*)stmt).statements);
            } else if(stmt.kind == CStmtKind.IF){
                auto s = cast(CIfStmt*)stmt;
                if(s.then_branch) count += count_var_slots((&s.then_branch)[0..1]);
                if(s.else_branch) count += count_var_slots((&s.else_branch)[0..1]);
            } else if(stmt.kind == CStmtKind.WHILE){
                auto s = cast(CWhileStmt*)stmt;
                if(s.body) count += count_var_slots((&s.body)[0..1]);
            } else if(stmt.kind == CStmtKind.FOR){
                auto s = cast(CForStmt*)stmt;
                if(s.init_stmt) count += count_var_slots((&s.init_stmt)[0..1]);
                if(s.body_) count += count_var_slots((&s.body_)[0..1]);
            }
        }
        return count;
    }

    // =========================================================================
    // Global Variable Generation
    // =========================================================================

    // Try to evaluate a constant expression and return its value
    // Returns true if successful, false if not a constant
    bool try_eval_const(CExpr* expr, out long value){
        if(expr is null) return false;

        if(CLiteral* lit = expr.as_literal()){
            if(lit.value.type == CTokenType.CHAR_LITERAL){
                value = parse_char_literal(lit.value.lexeme);
                return true;
            } else if(lit.value.type == CTokenType.NUMBER ||
                      lit.value.type == CTokenType.HEX){
                // Strip suffix
                str lex = lit.value.lexeme;
                while(lex.length > 0){
                    ubyte last = lex[$ - 1];
                    if(last == 'u' || last == 'U' || last == 'l' || last == 'L'){
                        lex = lex[0 .. $ - 1];
                    } else break;
                }
                auto parsed = parse_unsigned_human(lex);
                if(!parsed.errored){
                    value = cast(long)parsed.value;
                    return true;
                }
            }
            return false;
        }

        if(CUnary* unary = expr.as_unary()){
            long operand_val;
            if(!try_eval_const(unary.operand, operand_val)) return false;
            switch(unary.op) with(CTokenType){
                case MINUS: value = -operand_val; return true;
                case PLUS:  value = operand_val; return true;
                case TILDE: value = ~operand_val; return true;
                case BANG:  value = operand_val == 0 ? 1 : 0; return true;
                default: return false;
            }
        }

        if(CBinary* bin = expr.as_binary()){
            long lhs, rhs;
            if(!try_eval_const(bin.left, lhs)) return false;
            if(!try_eval_const(bin.right, rhs)) return false;
            switch(bin.op) with(CTokenType){
                case PLUS:  value = lhs + rhs; return true;
                case MINUS: value = lhs - rhs; return true;
                case STAR:  value = lhs * rhs; return true;
                case SLASH: if(rhs == 0) return false; value = lhs / rhs; return true;
                case PERCENT: if(rhs == 0) return false; value = lhs % rhs; return true;
                case AMP:   value = lhs & rhs; return true;
                case PIPE:  value = lhs | rhs; return true;
                case CARET: value = lhs ^ rhs; return true;
                case LESS_LESS: value = lhs << rhs; return true;
                case GREATER_GREATER: value = lhs >> rhs; return true;
                default: return false;
            }
        }

        if(CCast* cast_expr = expr.as_cast()){
            return try_eval_const(cast_expr.operand, value);
        }

        // Check for enum constants - use resolved ref_kind from parsing
        if(CIdentifier* ident = expr.as_identifier()){
            if(ident.ref_kind == IdentifierRefKind.ENUM_CONST){
                value = ident.enum_value;
                return true;
            }
        }

        return false;
    }

    // Get the base scalar type for an array (e.g., int for int[2][3])
    CType* get_scalar_type(CType* t){
        while(t !is null && t.is_array()){
            t = t.pointed_to;
        }
        return t;
    }

    // Count total scalar elements in a type (e.g., 6 for int[2][3])
    size_t count_scalar_elements(CType* t){
        size_t count = 1;
        while(t !is null && t.is_array()){
            count *= t.array_size;
            t = t.pointed_to;
        }
        return count;
    }

    // Flatten nested initializers and emit packed words
    // Returns number of scalar values emitted
    size_t emit_init_values(CExpr* init, CType* target_type, size_t scalar_size,
                            ref ulong word_val, ref size_t byte_offset, size_t size_words){
        size_t count = 0;

        if(CInitList* ilist = init.as_init_list()){
            // Nested init list - recurse into each element
            if(target_type.is_array()){
                CType* elem_type = target_type.pointed_to;
                foreach(ref elem; ilist.elements[]){
                    count += emit_init_values(elem.value, elem_type, elem_type.size_of(),
                                             word_val, byte_offset, size_words);
                }
            } else if(target_type.is_struct_or_union()){
                // For structs, match elements to fields in order
                auto fields = target_type.fields;
                size_t field_idx = 0;
                foreach(ref elem; ilist.elements[]){
                    if(field_idx >= fields.length) break;
                    CType* field_type = fields[field_idx].type;
                    CType* field_scalar = get_scalar_type(field_type);
                    size_t field_scalar_size = field_scalar ? field_scalar.size_of() : field_type.size_of();
                    count += emit_init_values(elem.value, field_type, field_scalar_size,
                                             word_val, byte_offset, size_words);
                    field_idx++;
                }
            } else {
                // Scalar type with init list - just use first element
                if(ilist.elements[].length > 0){
                    count += emit_init_values(ilist.elements[0].value, target_type, scalar_size,
                                             word_val, byte_offset, size_words);
                }
            }
        } else if(CLiteral* lit = init.as_literal()){
            // Check for string literal initializing char array
            if(lit.value.type == CTokenType.STRING && target_type.is_array() &&
               target_type.pointed_to !is null && target_type.pointed_to.kind == CTypeKind.CHAR){
                str s = lit.value.lexeme;
                if(s.length >= 2) s = s[1 .. $ - 1];  // Remove quotes
                size_t i = 0;
                while(i < s.length){
                    long char_val = 0;
                    if(s[i] == '\\' && i + 1 < s.length){
                        switch(s[i + 1]){
                            case 'n': char_val = '\n'; break;
                            case 't': char_val = '\t'; break;
                            case 'r': char_val = '\r'; break;
                            case '0': char_val = '\0'; break;
                            case '\\': char_val = '\\'; break;
                            default: char_val = s[i + 1]; break;
                        }
                        i += 2;
                    } else {
                        char_val = s[i];
                        i++;
                    }
                    // Pack this byte
                    size_t word_idx = byte_offset / 8;
                    size_t bit_offset = (byte_offset % 8) * 8;
                    if(word_idx * 8 == byte_offset && word_idx > 0){
                        // Emit previous word
                        sb.writef("% ", H(word_val));
                        word_val = 0;
                    }
                    word_val |= (cast(ulong)char_val & 0xFF) << bit_offset;
                    byte_offset++;
                    count++;
                }
                // Add null terminator
                size_t word_idx = byte_offset / 8;
                size_t bit_offset = (byte_offset % 8) * 8;
                if(word_idx * 8 == byte_offset && word_idx > 0){
                    sb.writef("% ", H(word_val));
                    word_val = 0;
                }
                // null terminator is 0, already there
                byte_offset++;
                count++;
            } else {
                // Scalar literal - skip if past array bounds (after non-word-aligned embed)
                if(byte_offset >= size_words * 8){
                    return count;
                }
                long val = 0;
                try_eval_const(init, val);
                // Convert to target type if needed (especially int->float)
                if(target_type.is_float32()){
                    // Integer constant to float32: convert value and get bits
                    float fval = cast(float)val;
                    val = *cast(int*)&fval;
                } else if(target_type.kind == CTypeKind.DOUBLE || target_type.kind == CTypeKind.LONG_DOUBLE){
                    // Integer constant to double: convert value and get bits
                    double dval = cast(double)val;
                    val = *cast(long*)&dval;
                }
                ulong mask = (scalar_size >= 8) ? ulong.max : ((1UL << (scalar_size * 8)) - 1);
                size_t word_idx = byte_offset / 8;
                size_t bit_offset = (byte_offset % 8) * 8;
                if(word_idx * 8 == byte_offset && word_idx > 0){
                    // Emit previous word
                    sb.writef("% ", H(word_val));
                    word_val = 0;
                }
                word_val |= (cast(ulong)val & mask) << bit_offset;
                byte_offset += scalar_size;
                count++;
            }
        } else if(CEmbed* embed = init.as_embed()){
            // embed must be at a word boundary
            if(byte_offset % 8 != 0){
                error(init.token, "#embed requires word-aligned position (prefix must be multiple of 8 bytes)");
                return count;
            }
            // Non-word-aligned length is OK if we're at the end (would zero-pad anyway)
            size_t embed_end_words = (byte_offset + embed.length + 7) / 8;
            if(embed.length % 8 != 0 && embed_end_words != size_words){
                error(init.token, "#embed with non-word-aligned length must be at end of array");
                return count;
            }
            // Emit any pending word
            if(word_val != 0){
                sb.writef("% ", H(word_val));
                word_val = 0;
            }
            // Emit embed directive for DASM to process
            sb.writef("embed \"%\" % % ", embed.path, embed.offset, embed.length);
            // Round up byte_offset to word boundary (embed consumes partial word)
            byte_offset = ((byte_offset + embed.length) + 7) & ~cast(size_t)7;
            count += embed.length;
        } else {
            // Skip if we're already past the array bounds (can happen after non-word-aligned embed)
            if(byte_offset >= size_words * 8){
                return count;
            }
            // Try to evaluate as constant expression
            long val = 0;
            try_eval_const(init, val);
            // Convert to target type if needed (especially int->float)
            if(target_type.is_float32()){
                // Integer constant to float32: convert value and get bits
                float fval = cast(float)val;
                val = *cast(int*)&fval;
            } else if(target_type.kind == CTypeKind.DOUBLE || target_type.kind == CTypeKind.LONG_DOUBLE){
                // Integer constant to double: convert value and get bits
                double dval = cast(double)val;
                val = *cast(long*)&dval;
            }
            ulong mask = (scalar_size >= 8) ? ulong.max : ((1UL << (scalar_size * 8)) - 1);
            size_t word_idx = byte_offset / 8;
            size_t bit_offset = (byte_offset % 8) * 8;
            if(word_idx * 8 == byte_offset && word_idx > 0){
                sb.writef("% ", H(word_val));
                word_val = 0;
            }
            word_val |= (cast(ulong)val & mask) << bit_offset;
            byte_offset += scalar_size;
            count++;
        }
        return count;
    }

    int gen_global_var(CGlobalVar* gvar){
        CType* vtype = gvar.var_type;
        size_t size_bytes = vtype.size_of();
        size_t size_words = (size_bytes + 7) / 8;  // Round up to words
        if(size_words == 0) size_words = 1;

        sb.writef("var % % ", gvar.name.lexeme, size_words);

        // Check if we have an initializer
        if(gvar.initializer !is null){
            // Handle array and struct initializers
            if((vtype.is_array() && vtype.pointed_to !is null) || vtype.is_struct_or_union()){
                CType* scalar_type = get_scalar_type(vtype);
                size_t scalar_size = scalar_type ? scalar_type.size_of() : 1;

                ulong word_val = 0;
                size_t byte_offset = 0;
                size_t emitted = emit_init_values(gvar.initializer, vtype, scalar_size,
                                                  word_val, byte_offset, size_words);

                // Emit final word only if there's a partial word with non-zero value
                // (embeds handle their own data, linker zero-inits remaining space)
                if(word_val != 0){
                    sb.writef("% ", H(word_val));
                } else if(byte_offset % 8 != 0 && byte_offset <= size_bytes){
                    // Partial word with zero value - only emit if within array bounds
                    sb.writef("% ", H(word_val));
                }
                // No need to emit trailing zeros - linker zero-inits
            } else {
                // Scalar variable - use try_eval_constant which handles floats
                ConstValue cv = try_eval_constant(gvar.initializer);
                if(!cv.is_const()){
                    error(gvar.name, "initializer element is not a compile-time constant");
                    sb.write("0 ");
                } else {
                    ulong bits = 0;
                    if(vtype.is_float32()){
                        // Float32: get as double, convert to float, extract bits
                        float fval = cast(float)cv.as_double();
                        bits = *cast(uint*)&fval;
                    } else if(vtype.kind == CTypeKind.DOUBLE || vtype.kind == CTypeKind.LONG_DOUBLE){
                        // Double: get as double, extract bits
                        double dval = cv.as_double();
                        bits = *cast(ulong*)&dval;
                    } else {
                        // Integer type
                        bits = cv.as_ulong();
                    }
                    sb.writef("% ", H(bits));
                }
            }
        } else {
            // No initializer - emit zeros
            for(size_t w = 0; w < size_words; w++){
                sb.write("0 ");
            }
        }

        sb.write("end\n");
        return 0;
    }

    // =========================================================================
    // Function Generation
    // =========================================================================

    int gen_function(CFunction* func){
        funcdepth++;
        scope(exit){
            funcdepth--;
            reglocals.cleanup();
            stacklocals.cleanup();
            var_types.cleanup();
            addr_taken.cleanup();
            arrays.cleanup();
            regallocator.reset();
            labelallocator.reset();
            if(label_table.count > 0) label_table.cleanup();  // Reset label mappings for new function
            use_stack = false;
            stack_offset = 1;  // Reset to 1 (slot 0 is saved RBP)
            current_return_type = null;
            returns_struct = false;
            uses_hidden_return_ptr = false;
            return_ptr_slot = -1;
        }

        // Track return type for struct/union returns
        current_return_type = func.return_type;
        returns_struct = func.return_type.is_struct_or_union();
        // Only use hidden pointer for structs/unions > 16 bytes (can't fit in 2 registers)
        uses_hidden_return_ptr = !fits_in_registers(current_return_type);

        // Run analysis to detect address-taken variables
        CAnalyzer analyzer;
        analyzer.allocator = allocator;
        analyzer.analyze_function(func);
        scope(exit) analyzer.cleanup();

        // Calculate max register slots used by any call in this function
        // Params in registers < max_call_slots must be saved (calls clobber them)
        // Params in registers >= max_call_slots can stay in place
        int max_call_slots = analyzer.max_call_slots;

        // Copy address-taken and array info
        foreach(ref item; analyzer.addr_taken.items()){
            addr_taken[item.key] = true;
        }
        foreach(ref item; analyzer.arrays.items()){
            arrays[item.key] = true;
        }
        // For large struct returns, caller passes hidden pointer as first arg
        int arg_offset = uses_hidden_return_ptr ? 1 : 0;

        // Calculate total register slots needed for parameters
        int total_param_slots = arg_offset;
        foreach(ref param; func.params){
            if(param.type.is_struct_or_union() && fits_in_registers(param.type)){
                total_param_slots += struct_return_regs(param.type);
            } else {
                total_param_slots += 1;
            }
        }

        // Use stack if any address is taken OR if we have many variables (> 4)
        use_stack = analyzer.should_use_stack();

        // Also force stack if any parameters are structs/unions or we use hidden return pointer
        if(uses_hidden_return_ptr) use_stack = true;
        foreach(ref param; func.params){
            if(param.type.is_struct_or_union()){
                use_stack = true;
                break;
            }
        }

        // Force stack if there are stack parameters (params beyond N_REG_ARGS)
        if(total_param_slots > N_REG_ARGS){
            use_stack = true;
        }

        // First, count how many stack slots we need
        int num_stack_slots = 0;
        if(use_stack){
            // Reserve slot for hidden return pointer if returning large struct
            if(uses_hidden_return_ptr){
                num_stack_slots += 1;
            }
            // Count parameters (struct params need multiple slots)
            foreach(ref param; func.params){
                num_stack_slots += cast(int)param.type.stack_slots();
            }
            // Count local variables, accounting for array sizes
            num_stack_slots += count_var_slots(func.body);
            // Count compound literal slots
            num_stack_slots += analyzer.compound_literal_slots;
        }

        // Emit function header with correct number of register slots
        sb.writef("function % %\n", func.name.lexeme, total_param_slots);

        // Set up stack frame if we have address-taken variables
        if(use_stack){
            sb.write("    push rbp\n");
            sb.write("    move rbp rsp\n");
            // Allocate all stack slots upfront (slots 1..n, so need n+1 slots total for push safety)
            sb.writef("    add rsp rsp %\n", P(num_stack_slots + 1));
        }

        // Save hidden return pointer if returning large struct
        if(uses_hidden_return_ptr && use_stack){
            return_ptr_slot = stack_offset++;
            sb.writef("    local_write % rarg1\n", P(return_ptr_slot));
        }

        // Calculate register slots for parameters (structs may use 1-2 slots)
        int current_reg_slot = arg_offset;

        // Calculate how many params are on stack (total_param_slots already calculated above)
        int n_stack_params = total_param_slots > N_REG_ARGS ? total_param_slots - N_REG_ARGS : 0;

        // For register-based locals, start allocating from max_call_slots
        // This lets us use R(max_call_slots)..R7 for saved params instead of R8+
        // Note: reglocals in R0-R7 must be saved around calls (handled in gen_call)
        int reg_base = N_REG_ARGS;
        if(use_stack){
            if(max_call_slots < N_REG_ARGS)
                reg_base = max_call_slots;
            // Also need to avoid clobbering rarg registers used by struct params
            if(total_param_slots > reg_base)
                reg_base = total_param_slots;
        }
        else {
            if(max_call_slots < N_REG_ARGS)
                reg_base = max_call_slots;
            if(total_param_slots > reg_base)
                reg_base = total_param_slots;
        }
        regallocator.reset_to(reg_base);

        // Move arguments from rarg registers (or stack) to locals
        foreach(i, ref param; func.params){
            str pname = param.name.lexeme;
            var_types[pname] = param.type;

            int reg_slot = current_reg_slot;
            int regs_used = 1;
            if(param.type.is_struct_or_union() && fits_in_registers(param.type)){
                regs_used = struct_return_regs(param.type);
            }
            current_reg_slot += regs_used;

            bool is_stack_param = (reg_slot >= N_REG_ARGS);
            bool spans_to_stack = (reg_slot < N_REG_ARGS && reg_slot + regs_used > N_REG_ARGS);

            if(use_stack){
                int slot = stack_offset;
                int num_slots = cast(int)param.type.stack_slots();
                stack_offset += num_slots;
                stacklocals[pname] = slot;

                if(is_stack_param){
                    // Parameter is on caller's stack
                    // Stack layout (DVM stack grows UP):
                    //   Before call: caller pushes args, RSP increases
                    //   After push rbp: RBP = RSP (which is original + n_stack_args * 8 + 8)
                    //   Stack arg[i] is at RBP - (n_stack_params - stack_idx) * 8 - 8
                    int stack_idx = reg_slot - N_REG_ARGS;
                    int offset = (n_stack_params - stack_idx) * 8 + 8;  // +8 for pushed rbp
                    if(param.type.is_struct_or_union()){
                        if(fits_in_registers(param.type)){
                            // Small struct on stack
                            sb.writef("    sub r1 rbp %\n", P(offset));
                            sb.write("    read r1 r1\n");
                            sb.writef("    add r0 rbp %\n", P(slot));
                            sb.write("    write r0 r1\n");
                            if(regs_used > 1){
                                sb.writef("    sub r1 rbp %\n", P(offset - 8));
                                sb.write("    read r1 r1\n");
                                sb.write("    add r0 r0 8\n");
                                sb.write("    write r0 r1\n");
                            }
                        } else {
                            // Large struct: pointer on stack
                            sb.writef("    sub r1 rbp %\n", P(offset));
                            sb.write("    read r1 r1\n");
                            sb.writef("    add r0 rbp %\n", P(slot));
                            sb.writef("    memcpy r0 r1 %\n", param.type.size_of());
                        }
                    } else {
                        // Read scalar from stack
                        sb.writef("    sub r0 rbp %\n", P(offset));
                        sb.write("    read r0 r0\n");
                        sb.writef("    local_write % r0\n", P(slot));
                    }
                } else if(spans_to_stack){
                    // Struct spans register and stack
                    if(param.type.is_struct_or_union() && fits_in_registers(param.type)){
                        sb.writef("    add r0 rbp %\n", P(slot));
                        sb.writef("    write r0 rarg%\n", 1 + reg_slot);
                        // Second word from stack
                        int stack_offset_val = 8 + 8;  // n_stack_params=1, stack_idx=0, offset = 1*8+8=16... wait
                        // Actually for spans_to_stack, the second word is first on stack (stack_idx = 0)
                        int offset = n_stack_params * 8 + 8;
                        sb.write("    add r0 r0 8\n");
                        sb.writef("    sub r1 rbp %\n", P(offset));
                        sb.write("    read r1 r1\n");
                        sb.write("    write r0 r1\n");
                    }
                } else if(param.type.is_struct_or_union()){
                    if(fits_in_registers(param.type)){
                        // Small struct: received in 1-2 registers, store to stack
                        // Use regallocator to avoid clobbering rarg registers (rarg1=r0, etc.)
                        int before = regallocator.alloced;
                        int addr_reg = regallocator.allocate();
                        sb.writef("    add r% rbp %\n", addr_reg, P(slot));
                        sb.writef("    write r% rarg%\n", addr_reg, 1 + reg_slot);
                        if(regs_used > 1){
                            sb.writef("    add r% r% 8\n", addr_reg, addr_reg);
                            sb.writef("    write r% rarg%\n", addr_reg, 2 + reg_slot);
                        }
                        regallocator.reset_to(before);
                    } else {
                        // Large struct: caller passed a pointer, we copy to our stack
                        int before = regallocator.alloced;
                        int addr_reg = regallocator.allocate();
                        sb.writef("    add r% rbp %\n", addr_reg, P(slot));
                        sb.writef("    memcpy r% rarg% %\n", addr_reg, 1 + reg_slot, param.type.size_of());
                        regallocator.reset_to(before);
                    }
                } else {
                    // Non-struct in register: just store the value
                    sb.writef("    local_write % rarg%\n", P(slot), 1 + reg_slot);
                }
            } else {
                // Register-based locals (no stack frame)
                if(is_stack_param){
                    // Read from caller's stack
                    int r = regallocator.allocate();
                    reglocals[pname] = r;
                    int stack_idx = reg_slot - N_REG_ARGS;
                    int offset = (n_stack_params - stack_idx) * 8 + 8;
                    sb.writef("    sub r% rbp %\n", r, P(offset));
                    sb.writef("    read r% r%\n", r, r);
                } else if(reg_slot >= max_call_slots){
                    // Param is in a register that won't be clobbered by any call
                    // (reg_slot >= max args used by biggest call)
                    // No move needed - just map the name to the register
                    reglocals[pname] = RARG1 + reg_slot;
                } else {
                    // Param is in a register that could be clobbered by calls
                    // Must save to scratch regs (R8+)
                    int r = regallocator.allocate();
                    reglocals[pname] = r;
                    sb.writef("    move r% rarg%\n", r, 1 + reg_slot);
                }
            }
        }

        // For functions where params stay in arg registers, we need to skip those
        // when allocating temporaries. Only move forward, never backward.
        if(!use_stack){
            int n_reg_params = total_param_slots < N_REG_ARGS ? total_param_slots : N_REG_ARGS;
            // Params that stayed in place use R0..R(n_reg_params-1)
            // Only advance allocator if it would skip past those registers
            if(n_reg_params > regallocator.alloced){
                regallocator.reset_to(n_reg_params);
            }
        }

        // Generate body
        foreach(stmt; func.body){
            int err = gen_statement(stmt);
            if(err) return err;
        }

        // Add implicit return if needed (skip for dasm blocks - they handle their own return)
        if(func.body.length == 0 || (func.body[$ - 1].kind != CStmtKind.RETURN && func.body[$ - 1].kind != CStmtKind.DASM)){
            if(use_stack){
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

    int gen_statement(CStmt* stmt){
        final switch(stmt.kind) with (CStmtKind){
            case EXPR:       return gen_expr_stmt(cast(CExprStmt*)stmt);
            case RETURN:     return gen_return(cast(CReturnStmt*)stmt);
            case IF:         return gen_if(cast(CIfStmt*)stmt);
            case WHILE:      return gen_while(cast(CWhileStmt*)stmt);
            case DO_WHILE:   return gen_do_while(cast(CDoWhileStmt*)stmt);
            case FOR:        return gen_for(cast(CForStmt*)stmt);
            case BLOCK:      return gen_block(cast(CBlock*)stmt);
            case VAR_DECL:   return gen_var_decl(cast(CVarDecl*)stmt);
            case BREAK:      return gen_break(cast(CBreakStmt*)stmt);
            case CONTINUE:   return gen_continue(cast(CContinueStmt*)stmt);
            case EMPTY:      return 0;
            case SWITCH:     return gen_switch(cast(CSwitchStmt*)stmt);
            case GOTO:       return gen_goto(cast(CGotoStmt*)stmt);
            case LABEL:      return gen_label(cast(CLabelStmt*)stmt);
            case DASM:       return gen_dasm(cast(CDasmStmt*)stmt);
            case CASE_LABEL: return gen_case_label(cast(CCaseLabelStmt*)stmt);
            case ASM:        return gen_asm(cast(CAsmStmt*)stmt);
        }
    }

    int gen_expr_stmt(CExprStmt* stmt){
        int before = regallocator.alloced;
        int err = gen_expression(stmt.expression, TARGET_IS_NOTHING);
        regallocator.reset_to(before);
        return err;
    }

    int gen_return(CReturnStmt* stmt){
        if(stmt.value !is null){
            int before = regallocator.alloced;

            if(uses_hidden_return_ptr){
                // Large struct return (> 16 bytes): copy to hidden return pointer
                int src_reg = regallocator.allocate();
                int err = gen_struct_address(stmt.value, src_reg);
                if(err) return err;

                int dst_reg = regallocator.allocate();
                sb.writef("    local_read r% %\n", dst_reg, P(return_ptr_slot));

                size_t struct_size = current_return_type.size_of();
                sb.writef("    memcpy r% r% %\n", dst_reg, src_reg, struct_size);

                // Return the pointer in rout1
                sb.writef("    move rout1 r%\n", dst_reg);
            } else if(returns_struct){
                // Small struct return (<= 16 bytes): return in registers
                int src_reg = regallocator.allocate();
                int err = gen_struct_address(stmt.value, src_reg);
                if(err) return err;

                size_t struct_size = current_return_type.size_of();
                int num_regs = struct_return_regs(current_return_type);

                // Load struct data into return registers
                if(struct_size <= 8){
                    // Read 8 bytes into rout1
                    sb.writef("    read rout1 r%\n", src_reg);
                } else {
                    // 9-16 bytes: read 8 bytes into rout1, next 8 into rout2
                    sb.writef("    read rout1 r%\n", src_reg);
                    sb.writef("    add r% r% 8\n", src_reg, src_reg);
                    sb.writef("    read rout2 r%\n", src_reg);
                }
            } else {
                // Non-struct return: generate directly to rout1
                // Check if value is already in a register
                int src = get_expr_reg(stmt.value);
                if(src >= 0){
                    sb.writef("    move rout1 r%\n", src);
                } else {
                    // Generate directly to r15 (rout1)
                    int err = gen_expression(stmt.value, 15);
                    if(err) return err;
                }
            }
            regallocator.reset_to(before);
        }
        if(use_stack){
            sb.write("    move rsp rbp\n");
            sb.write("    pop rbp\n");
        }
        sb.write("    ret\n");
        return 0;
    }

    int gen_if(CIfStmt* stmt){
        int after_label = labelallocator.allocate();

        // Generate condition
        int before = regallocator.alloced;
        int cond;
        int err = generate_expr_maybe_new_reg(stmt.condition, cond);
        if(err) return err;
        regallocator.reset_to(before);

        sb.writef("    cmp r% 0\n", cond);

        if(stmt.else_branch !is null){
            int else_label = labelallocator.allocate();
            sb.writef("    jump eq label L%\n", else_label);

            err = gen_statement(stmt.then_branch);
            if(err) return err;

            sb.writef("    move rip label L%\n", after_label);
            sb.writef("  label L%\n", else_label);

            err = gen_statement(stmt.else_branch);
            if(err) return err;
        } else {
            sb.writef("    jump eq label L%\n", after_label);
            err = gen_statement(stmt.then_branch);
            if(err) return err;
        }

        sb.writef("  label L%\n", after_label);
        return 0;
    }

    int gen_while(CWhileStmt* stmt){
        int prev_continue = current_continue_target;
        int prev_break = current_break_target;
        scope(exit){
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
        if(err) return err;
        regallocator.reset_to(before);

        sb.writef("    cmp r% 0\n", cond);
        sb.writef("    jump eq label L%\n", after_label);

        err = gen_statement(stmt.body);
        if(err) return err;

        sb.writef("    move rip label L%\n", top_label);
        sb.writef("  label L%\n", after_label);
        return 0;
    }

    int gen_do_while(CDoWhileStmt* stmt){
        int prev_continue = current_continue_target;
        int prev_break = current_break_target;
        scope(exit){
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
        if(err) return err;

        // Condition check
        sb.writef("  label L%\n", cond_label);
        int before = regallocator.alloced;
        int cond = regallocator.allocate();
        err = gen_expression(stmt.condition, cond);
        if(err) return err;
        regallocator.reset_to(before);

        sb.writef("    cmp r% 0\n", cond);
        sb.writef("    jump ne label L%\n", top_label);  // Loop if condition is true

        sb.writef("  label L%\n", after_label);
        return 0;
    }

    int gen_for(CForStmt* stmt){
        int prev_continue = current_continue_target;
        int prev_break = current_break_target;
        scope(exit){
            current_continue_target = prev_continue;
            current_break_target = prev_break;
        }

        // Initializer
        if(stmt.init_stmt !is null){
            int err = gen_statement(stmt.init_stmt);
            if(err) return err;
        }

        int top_label = labelallocator.allocate();
        int incr_label = labelallocator.allocate();
        int after_label = labelallocator.allocate();
        current_continue_target = incr_label;
        current_break_target = after_label;

        sb.writef("  label L%\n", top_label);

        // Condition
        if(stmt.condition !is null){
            int before = regallocator.alloced;
            int cond = regallocator.allocate();
            int err = gen_expression(stmt.condition, cond);
            if(err) return err;
            regallocator.reset_to(before);

            sb.writef("    cmp r% 0\n", cond);
            sb.writef("    jump eq label L%\n", after_label);
        }

        // Body
        int err = gen_statement(stmt.body_);
        if(err) return err;

        // Increment
        sb.writef("  label L%\n", incr_label);
        if(stmt.increment !is null){
            int before = regallocator.alloced;
            err = gen_expression(stmt.increment, TARGET_IS_NOTHING);
            if(err) return err;
            regallocator.reset_to(before);
        }

        sb.writef("    move rip label L%\n", top_label);
        sb.writef("  label L%\n", after_label);
        return 0;
    }

    // Recursively collect all case labels from a statement tree
    void collect_case_labels(CStmt* stmt, ref Barray!(CCaseLabelStmt*) cases){
        if(stmt is null) return;

        final switch(stmt.kind) with (CStmtKind){
            case CASE_LABEL:
                auto c = cast(CCaseLabelStmt*)stmt;
                cases ~= c;
                // Also collect from the labeled statement
                collect_case_labels(c.statement, cases);
                break;

            case BLOCK:
                auto b = cast(CBlock*)stmt;
                foreach(s; b.statements)
                    collect_case_labels(s, cases);
                break;

            case IF:
                auto i = cast(CIfStmt*)stmt;
                collect_case_labels(i.then_branch, cases);
                collect_case_labels(i.else_branch, cases);
                break;

            case WHILE:
                auto w = cast(CWhileStmt*)stmt;
                collect_case_labels(w.body, cases);
                break;

            case DO_WHILE:
                auto d = cast(CDoWhileStmt*)stmt;
                collect_case_labels(d.body, cases);
                break;

            case FOR:
                auto f = cast(CForStmt*)stmt;
                collect_case_labels(f.init_stmt, cases);
                collect_case_labels(f.body_, cases);
                break;

            case SWITCH:
                auto s = cast(CSwitchStmt*)stmt;
                collect_case_labels(s.body_, cases);
                break;

            case LABEL:
                auto l = cast(CLabelStmt*)stmt;
                collect_case_labels(l.statement, cases);
                break;

            // These don't contain nested statements
            case EXPR:
            case RETURN:
            case VAR_DECL:
            case BREAK:
            case CONTINUE:
            case EMPTY:
            case GOTO:
            case DASM:
            case ASM:
                break;
        }
    }

    int gen_switch(CSwitchStmt* stmt){
        int prev_break = current_break_target;
        scope(exit){
            current_break_target = prev_break;
        }

        int end_label = labelallocator.allocate();
        current_break_target = end_label;

        // First pass: collect all case labels from the body
        auto cases = make_barray!(CCaseLabelStmt*)(allocator);
        collect_case_labels(stmt.body_, cases);

        // Remember where our case labels start (for cleanup/lookup)
        size_t case_start = case_label_stmts.count;

        // Allocate labels for each case and register them
        int default_label = -1;
        foreach(c; cases[]){
            int lbl = labelallocator.allocate();
            case_label_stmts ~= c;
            case_label_nums ~= lbl;
            if(c.is_default){
                default_label = lbl;
            }
        }

        // Evaluate switch expression once
        int before = regallocator.alloced;
        int cond_reg = regallocator.allocate();
        int err = gen_expression(stmt.condition, cond_reg);
        if(err) return err;

        // Generate jump table: compare and jump to matching case
        foreach(i, c; cases[]){
            if(!c.is_default){
                // Generate comparison
                int case_reg = regallocator.allocate();
                err = gen_expression(c.case_value, case_reg);
                if(err) return err;

                sb.writef("    cmp r% r%\n", cond_reg, case_reg);
                sb.writef("    jump eq label L%\n", case_label_nums[case_start + i]);
                regallocator.reset_to(before + 1);  // Keep cond_reg
            }
        }

        // If no case matched, jump to default or end
        if(default_label >= 0){
            sb.writef("    move rip label L%\n", default_label);
        } else {
            sb.writef("    move rip label L%\n", end_label);
        }

        regallocator.reset_to(before);

        // Generate the body - case labels will emit their labels via gen_case_label
        err = gen_statement(stmt.body_);
        if(err) return err;

        sb.writef("  label L%\n", end_label);

        // Clean up case label tracking for this switch
        case_label_stmts.count = case_start;
        case_label_nums.count = case_start;

        return 0;
    }

    int gen_case_label(CCaseLabelStmt* stmt){
        // Find the label number for this case statement
        int lbl = -1;
        foreach(i, s; case_label_stmts[]){
            if(s is stmt){
                lbl = case_label_nums[i];
                break;
            }
        }

        if(lbl < 0){
            error(stmt.stmt.token, "case label not in switch");
            return 1;
        }

        sb.writef("  label L%\n", lbl);

        // Generate the labeled statement (fallthrough behavior)
        return gen_statement(stmt.statement);
    }

    // Get or allocate a label number for a named label
    int get_label_number(str name){
        if(auto p = name in label_table){
            return *p;
        }
        int lbl = labelallocator.allocate();
        label_table[name] = lbl;
        return lbl;
    }

    int gen_goto(CGotoStmt* stmt){
        str label_name = stmt.label.lexeme;
        int lbl = get_label_number(label_name);
        sb.writef("    move rip label L%\n", lbl);
        return 0;
    }

    int gen_label(CLabelStmt* stmt){
        str label_name = stmt.label.lexeme;
        int lbl = get_label_number(label_name);
        sb.writef("  label L%\n", lbl);

        // Generate the statement following the label
        if(stmt.statement !is null){
            return gen_statement(stmt.statement);
        }
        return 0;
    }

    int gen_dasm(CDasmStmt* stmt){
        // Emit raw dasm code directly
        sb.write(stmt.code);
        sb.write('\n');
        return 0;
    }

    int gen_asm(CAsmStmt* stmt){
        error(stmt.stmt.token, "GCC inline assembly (__asm__) is not supported");
        return 1;
    }

    int gen_block(CBlock* stmt){
        push_var_scope();
        foreach(s; stmt.statements){
            int err = gen_statement(s);
            if(err){
                pop_var_scope();
                return err;
            }
        }
        pop_var_scope();
        return 0;
    }

    int gen_var_decl(CVarDecl* stmt){
        str name = stmt.name.lexeme;

        // Save any existing variable with this name (for shadowing support)
        save_shadowed_var(name);

        var_types[name] = stmt.var_type;

        // Check if this is an array or struct/union
        bool is_array = stmt.var_type.is_array();
        bool is_struct = stmt.var_type.is_struct_or_union();

        if(use_stack){
            // All variables go on stack when use_stack is true
            int slot = stack_offset;

            if(is_struct){
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
                if(stmt.initializer !is null){
                    if(CInitList* init_list = stmt.initializer.as_init_list()){
                        before = regallocator.alloced;
                        addr_reg = regallocator.allocate();
                        int val_reg = regallocator.allocate();

                        // Get struct field info
                        auto fields = stmt.var_type.fields;
                        size_t num_elems = init_list.elements.length;
                        // Only limit when there are no designators (pure positional)
                        // With designators, multiple elements can target sub-fields of same field
                        bool has_designators = false;
                        foreach(e; init_list.elements){
                            if(e.designators.length > 0){
                                has_designators = true;
                                break;
                            }
                        }
                        if(!has_designators && num_elems > fields.length) num_elems = fields.length;

                        size_t field_idx = 0;
                        for(size_t i = 0; i < num_elems; i++){
                            auto elem = init_list.elements[i];

                            size_t offset;
                            CType* field_type;
                            bool is_chained = false;

                            // Handle designators
                            if(elem.designators.length > 0){
                                // Traverse all designators to compute final offset and type
                                offset = 0;
                                CType* current_type = stmt.var_type;

                                foreach(di, desig; elem.designators){
                                    if(desig.kind == CDesignatorKind.FIELD){
                                        if(current_type.kind != CTypeKind.STRUCT){
                                            error(desig.token, "Field designator on non-struct type");
                                            return 1;
                                        }
                                        auto cur_fields = current_type.fields;
                                        bool found = false;
                                        foreach(f; cur_fields){
                                            if(f.name == desig.field_name){
                                                offset += f.offset;
                                                current_type = f.type;
                                                // Update field_idx for continuation (only for first designator)
                                                if(di == 0){
                                                    for(size_t fi = 0; fi < fields.length; fi++){
                                                        if(fields[fi].name == desig.field_name){
                                                            field_idx = fi + 1;
                                                            break;
                                                        }
                                                    }
                                                }
                                                found = true;
                                                break;
                                            }
                                        }
                                        if(!found){
                                            error(desig.token, "Unknown field in designator");
                                            return 1;
                                        }
                                    } else { // INDEX
                                        if(!current_type.is_array()){
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
                                if(field_idx >= fields.length) break;
                                offset = fields[field_idx].offset;
                                field_type = fields[field_idx].type;
                                field_idx++;
                            }

                            size_t field_size = field_type.size_of();

                            // Check if field value is a nested init list (for nested struct)
                            if(CInitList* nested_init = elem.value.as_init_list()){
                                if(field_type.kind == CTypeKind.STRUCT){
                                    // Initialize nested struct field
                                    auto nested_fields = field_type.fields;
                                    size_t num_nested = nested_init.elements.length;
                                    if(num_nested > nested_fields.length) num_nested = nested_fields.length;

                                    size_t nested_field_idx = 0;
                                    for(size_t j = 0; j < num_nested; j++){
                                        auto nested_elem = nested_init.elements[j];

                                        // Handle field designators in nested init
                                        if(nested_elem.designators.length > 0){
                                            if(nested_elem.designators[0].kind != CDesignatorKind.FIELD){
                                                error(nested_elem.designators[0].token, "Expected field designator");
                                                return 1;
                                            }
                                            str fname = nested_elem.designators[0].field_name;
                                            bool found = false;
                                            for(size_t fi = 0; fi < nested_fields.length; fi++){
                                                if(nested_fields[fi].name == fname){
                                                    nested_field_idx = fi;
                                                    found = true;
                                                    break;
                                                }
                                            }
                                            if(!found){
                                                error(nested_elem.designators[0].token, "Unknown field");
                                                return 1;
                                            }
                                        }

                                        if(nested_field_idx >= nested_fields.length) break;

                                        int err = gen_expression(nested_elem.value, val_reg);
                                        if(err) return err;
                                        // Add implicit conversion from expression type to field type
                                        gen_implicit_conversion(val_reg, nested_elem.value, nested_fields[nested_field_idx].type);

                                        size_t nested_offset = offset + nested_fields[nested_field_idx].offset;
                                        size_t nested_size = nested_fields[nested_field_idx].type.size_of();
                                        int nested_slot = slot + cast(int)(nested_offset / 8);
                                        sb.writef("    add r% rbp %\n", addr_reg, P(nested_slot));
                                        if(nested_offset % 8 != 0){
                                            sb.writef("    add r% r% %\n", addr_reg, addr_reg, nested_offset % 8);
                                        }
                                        sb.writef("    % r% r%\n", write_instr_for_size(nested_size), addr_reg, val_reg);
                                        nested_field_idx++;
                                    }
                                } else {
                                    error(elem.value.token, "Nested initializer for non-struct field");
                                    return 1;
                                }
                            } else if(field_type.kind == CTypeKind.STRUCT && needs_memcpy(field_size)){
                                // Large struct field - need memcpy from another struct,
                                // OR handle scalar initialization (C allows scalar to init first subobject)
                                CExpr* val_expr = elem.value.ungroup();
                                int field_slot = slot + cast(int)(offset / 8);
                                sb.writef("    add r% rbp %\n", addr_reg, P(field_slot));
                                if(offset % 8 != 0){
                                    sb.writef("    add r% r% %\n", addr_reg, addr_reg, offset % 8);
                                }

                                // Check if value is a struct expression (has address) or scalar
                                bool is_struct_value = val_expr.type !is null && val_expr.type.is_struct_or_union();

                                if(is_struct_value){
                                    // Copy from another struct
                                    int src_reg = val_reg;
                                    int err = gen_struct_address(val_expr, src_reg);
                                    if(err) return err;
                                    sb.writef("    memcpy r% r% %\n", addr_reg, src_reg, field_size);
                                } else {
                                    // Scalar initializing struct: zero struct, then init first subobject
                                    // In C, {0} zeros everything; {N} sets first scalar to N, zeros rest
                                    sb.writef("    memzero r% %\n", addr_reg, field_size);
                                    // Find first scalar subfield and initialize it
                                    CType* first_scalar = field_type;
                                    size_t first_offset = 0;
                                    while(first_scalar.kind == CTypeKind.STRUCT && first_scalar.fields.length > 0){
                                        first_offset += first_scalar.fields[0].offset;
                                        first_scalar = first_scalar.fields[0].type;
                                    }
                                    if(first_scalar.kind != CTypeKind.STRUCT){
                                        // Generate value and write to first scalar field
                                        int err = gen_expression(val_expr, val_reg);
                                        if(err) return err;
                                        if(first_offset > 0){
                                            sb.writef("    add r% r% %\n", addr_reg, addr_reg, first_offset);
                                        }
                                        sb.writef("    % r% r%\n", write_instr_for_size(first_scalar.size_of()), addr_reg, val_reg);
                                    }
                                }
                            } else {
                                // Scalar field (or small struct <= 8 bytes)
                                int err = gen_expression(elem.value, val_reg);
                                if(err) return err;
                                // Add implicit conversion from expression type to field type
                                gen_implicit_conversion(val_reg, elem.value, field_type);
                                int field_slot = slot + cast(int)(offset / 8);
                                sb.writef("    add r% rbp %\n", addr_reg, P(field_slot));
                                if(offset % 8 != 0){
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

                        // src = address of source struct (do first - may involve function call)
                        int err = gen_struct_address(stmt.initializer, src_reg);
                        if(err) return err;

                        // dst = address of this struct (do after so not clobbered by call)
                        sb.writef("    add r% rbp %\n", dst_reg, P(slot));

                        // memcpy
                        sb.writef("    memcpy r% r% %\n", dst_reg, src_reg, struct_size);

                        regallocator.reset_to(before);
                    }
                }
            } else if(is_array){
                // Arrays need slots based on total byte size, not element count
                size_t arr_size = stmt.var_type.array_size;  // element count (for memzero etc.)
                size_t num_slots = stmt.var_type.stack_slots();  // actual slots needed
                stack_offset += cast(int)num_slots;

                // Helper to recursively initialize nested arrays
                int gen_nested_array_init(CInitList* init_list, CType* arr_type, int base_slot, size_t base_offset,
                                         int addr_reg, int val_reg){
                    if(!arr_type.is_array()) return 1;
                    CType* elem_type = arr_type.pointed_to;
                    size_t elem_size = elem_type ? elem_type.size_of() : 1;
                    size_t arr_size_ = arr_type.array_size;
                    size_t num_elems = init_list.elements.length;
                    if(num_elems > arr_size_) num_elems = arr_size_;

                    for(size_t i = 0; i < num_elems; i++){
                        auto elem = init_list.elements[i];
                        size_t elem_offset = base_offset + i * elem_size;

                        if(CInitList* nested = elem.value.as_init_list()){
                            if(elem_type.is_array()){
                                // Recurse for deeper nesting
                                int err = gen_nested_array_init(nested, elem_type, base_slot, elem_offset, addr_reg, val_reg);
                                if(err) return err;
                            } else if(elem_type.is_struct()){
                                // Handle struct initialization
                                auto fields = elem_type.fields;
                                size_t num_fields = nested.elements.length;
                                if(num_fields > fields.length) num_fields = fields.length;
                                for(size_t fi = 0; fi < num_fields; fi++){
                                    auto field_elem = nested.elements[fi];
                                    size_t field_offset = elem_offset + fields[fi].offset;
                                    size_t field_size = fields[fi].type.size_of();
                                    int field_slot = base_slot + cast(int)(field_offset / 8);

                                    int err = gen_expression(field_elem.value, val_reg);
                                    if(err) return err;

                                    sb.writef("    add r% rbp %\n", addr_reg, P(field_slot));
                                    if(field_offset % 8 != 0){
                                        sb.writef("    add r% r% %\n", addr_reg, addr_reg, field_offset % 8);
                                    }
                                    sb.writef("    % r% r%\n", write_instr_for_size(field_size), addr_reg, val_reg);
                                }
                            } else {
                                error(elem.value.token, "Nested initializer for non-array/non-struct element");
                                return 1;
                            }
                        } else {
                            // Scalar value
                            int elem_slot = base_slot + cast(int)(elem_offset / 8);
                            int err = gen_expression(elem.value, val_reg);
                            if(err) return err;

                            sb.writef("    add r% rbp %\n", addr_reg, P(elem_slot));
                            if(elem_offset % 8 != 0){
                                sb.writef("    add r% r% %\n", addr_reg, addr_reg, elem_offset % 8);
                            }
                            sb.writef("    % r% r%\n", write_instr_for_size(elem_size), addr_reg, val_reg);
                        }
                    }
                    return 0;
                }

                // Handle array initializer
                if(stmt.initializer !is null){
                    if(CLiteral* lit = stmt.initializer.as_literal()){
                        if(lit.value.type == CTokenType.STRING){
                            // String literal initializer for char array
                            // Compute string length (excluding quotes, accounting for escapes)
                            str lex = lit.value.lexeme;
                            size_t str_len = 0;
                            for(size_t i = 1; i < lex.length - 1; i++){
                                if(lex[i] == '\\' && i + 1 < lex.length - 1) i++;  // Skip escape
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
                    } else if(CInitList* init_list = stmt.initializer.as_init_list()){
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
                        for(size_t i = 0; i < num_elems; i++){
                            auto elem = init_list.elements[i];

                            size_t offset;
                            CType* target_type;
                            bool is_chained = false;

                            // Handle designators
                            if(elem.designators.length > 0){
                                // Traverse all designators to compute final offset and type
                                offset = 0;
                                CType* current_type = stmt.var_type;

                                foreach(di, desig; elem.designators){
                                    if(desig.kind == CDesignatorKind.INDEX){
                                        if(!current_type.is_array()){
                                            error(desig.token, "Index designator on non-array type");
                                            return 1;
                                        }
                                        size_t idx = cast(size_t)desig.index_value;
                                        offset += idx * current_type.pointed_to.size_of();
                                        current_type = current_type.pointed_to;
                                        // Update current_idx for continuation (only for first designator)
                                        if(di == 0){
                                            current_idx = idx + 1;
                                        }
                                    } else { // FIELD
                                        if(current_type.kind != CTypeKind.STRUCT){
                                            error(desig.token, "Field designator on non-struct type");
                                            return 1;
                                        }
                                        auto cur_fields = current_type.fields;
                                        bool found = false;
                                        foreach(f; cur_fields){
                                            if(f.name == desig.field_name){
                                                offset += f.offset;
                                                current_type = f.type;
                                                found = true;
                                                break;
                                            }
                                        }
                                        if(!found){
                                            error(desig.token, "Unknown field in designator");
                                            return 1;
                                        }
                                    }
                                }

                                target_type = current_type;
                                is_chained = elem.designators.length > 1;
                            } else {
                                // Positional: use current_idx
                                if(current_idx >= arr_size) break;
                                offset = current_idx * elem_size;
                                target_type = stmt.var_type.pointed_to;
                                current_idx++;
                            }

                            int target_slot = slot + cast(int)(offset / 8);
                            size_t target_size = target_type.size_of();

                            // For chained designators, initialize directly at target
                            if(is_chained){
                                if(target_type.kind == CTypeKind.STRUCT && needs_memcpy(target_size)){
                                    int src_reg = val_reg;
                                    int err = gen_struct_address(elem.value, src_reg);
                                    if(err) return err;
                                    sb.writef("    add r% rbp %\n", addr_reg, P(target_slot));
                                    if(offset % 8 != 0){
                                        sb.writef("    add r% r% %\n", addr_reg, addr_reg, offset % 8);
                                    }
                                    sb.writef("    memcpy r% r% %\n", addr_reg, src_reg, target_size);
                                } else {
                                    int err = gen_expression(elem.value, val_reg);
                                    if(err) return err;
                                    sb.writef("    add r% rbp %\n", addr_reg, P(target_slot));
                                    if(offset % 8 != 0){
                                        sb.writef("    add r% r% %\n", addr_reg, addr_reg, offset % 8);
                                    }
                                    sb.writef("    % r% r%\n", write_instr_for_size(target_size), addr_reg, val_reg);
                                }
                                continue;
                            }

                            // Check if element is a nested init list (for array of structs)
                            if(CInitList* nested_init = elem.value.as_init_list()){
                                CType* elem_type = stmt.var_type.pointed_to;
                                if(elem_type.kind == CTypeKind.STRUCT){
                                    // Initialize nested struct at this array position
                                    auto fields = elem_type.fields;
                                    size_t num_nested = nested_init.elements.length;
                                    if(num_nested > fields.length) num_nested = fields.length;

                                    size_t field_idx = 0;
                                    for(size_t j = 0; j < num_nested; j++){
                                        auto nested_elem = nested_init.elements[j];

                                        // Handle field designators in nested init
                                        if(nested_elem.designators.length > 0){
                                            if(nested_elem.designators[0].kind != CDesignatorKind.FIELD){
                                                error(nested_elem.designators[0].token, "Expected field designator");
                                                return 1;
                                            }
                                            str field_name = nested_elem.designators[0].field_name;
                                            bool found = false;
                                            for(size_t fi = 0; fi < fields.length; fi++){
                                                if(fields[fi].name == field_name){
                                                    field_idx = fi;
                                                    found = true;
                                                    break;
                                                }
                                            }
                                            if(!found){
                                                error(nested_elem.designators[0].token, "Unknown field");
                                                return 1;
                                            }
                                        }

                                        if(field_idx >= fields.length) break;

                                        int err = gen_expression(nested_elem.value, val_reg);
                                        if(err) return err;

                                        size_t field_offset = offset + fields[field_idx].offset;
                                        size_t field_size = fields[field_idx].type.size_of();
                                        int field_slot = slot + cast(int)(field_offset / 8);
                                        sb.writef("    add r% rbp %\n", addr_reg, P(field_slot));
                                        if(field_offset % 8 != 0){
                                            sb.writef("    add r% r% %\n", addr_reg, addr_reg, field_offset % 8);
                                        }
                                        sb.writef("    % r% r%\n", write_instr_for_size(field_size), addr_reg, val_reg);
                                        field_idx++;
                                    }
                                } else if(elem_type.is_array()){
                                    // Initialize nested array (for multi-dimensional arrays)
                                    // Recursively flatten and write scalar values
                                    int err = gen_nested_array_init(nested_init, elem_type, slot, offset, addr_reg, val_reg);
                                    if(err) return err;
                                } else {
                                    error(elem.value.token, "Nested initializer for non-struct/non-array element");
                                    return 1;
                                }
                            } else {
                                if(target_type.kind == CTypeKind.STRUCT && needs_memcpy(target_size)){
                                    // Large struct element - need memcpy
                                    int src_reg = val_reg;
                                    int err = gen_struct_address(elem.value, src_reg);
                                    if(err) return err;
                                    sb.writef("    add r% rbp %\n", addr_reg, P(target_slot));
                                    if(offset % 8 != 0){
                                        sb.writef("    add r% r% %\n", addr_reg, addr_reg, offset % 8);
                                    }
                                    sb.writef("    memcpy r% r% %\n", addr_reg, src_reg, target_size);
                                } else {
                                    // Scalar element (or small struct <= 8 bytes)
                                    int err = gen_expression(elem.value, val_reg);
                                    if(err) return err;
                                    sb.writef("    add r% rbp %\n", addr_reg, P(target_slot));
                                    if(offset % 8 != 0){
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
                if(stmt.initializer !is null){
                    int before = regallocator.alloced;
                    int temp = regallocator.allocate();
                    int err = gen_expression(stmt.initializer, temp);
                    if(err) return err;
                    // Convert double to float32 before storing
                    // Note: if initializer already has type float32 (e.g., from an implicit cast),
                    // then gen_expression already converted it, so no need for another dtof
                    CType* init_type = stmt.initializer.type;
                    bool init_is_float32 = init_type && init_type.is_float32();
                    if(stmt.var_type.is_float32() && !init_is_float32){
                        sb.writef("    dtof r% r%\n", temp, temp);
                    }
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
            if(stmt.initializer !is null){
                int err = gen_expression(stmt.initializer, r);
                if(err) return err;
            } else {
                // Default initialize to 0
                sb.writef("    move r% 0\n", r);
            }
        }

        return 0;
    }

    int gen_break(CBreakStmt* stmt){
        if(current_break_target == -1){
            error(stmt.stmt.token, "'break' outside of loop");
            return 1;
        }
        sb.writef("    move rip label L%\n", current_break_target);
        return 0;
    }

    int gen_continue(CContinueStmt* stmt){
        if(current_continue_target == -1){
            error(stmt.stmt.token, "'continue' outside of loop");
            return 1;
        }
        sb.writef("    move rip label L%\n", current_continue_target);
        return 0;
    }

    // =========================================================================
    // Expression Generation
    // =========================================================================

    int gen_expression(CExpr* e, int target){
        // NOTE: do not handle TARGET_IS_NOTHING here, any of these expressions could
        // have side-effects, so just handle that in the specific handler function.
        e = e.ungroup();

        // Try constant folding (enum constants are resolved at parse time)
        ConstValue cv = try_eval_constant(e);
        if(cv.is_const()){
            if(target != TARGET_IS_NOTHING) {
                if (cv.is_float()) {
                    // Check if target type is float32 vs double
                    CType* expr_type = e.type;
                    if(expr_type && expr_type.kind == CTypeKind.FLOAT){
                        // Emit as 32-bit float representation
                        float f32 = cast(float)cv.float_val;
                        uint bits = *cast(uint*)&f32;
                        sb.writef("    move r% %\n", target, H(bits));
                    } else {
                        // Emit as 64-bit double representation
                        ulong bits = *cast(ulong*)&cv.float_val;
                        sb.writef("    move r% %\n", target, H(bits));
                    }
                } else {
                    sb.writef("    move r% %\n", target, cv.as_long);
                }
            }
            return 0;
        }

        final switch(e.kind) with (CExprKind){
            case LITERAL:    return gen_literal(e.as_literal, target);
            case IDENTIFIER: return gen_identifier(e.as_identifier, target);
            case BINARY:     return gen_binary(e.as_binary, target);
            case UNARY:      return gen_unary(e.as_unary, target);
            case CALL:       return gen_call(e.as_call, target);
            case ASSIGN:     return gen_assign(e.as_assign, target);
            case SUBSCRIPT:  return gen_subscript(e.as_subscript, target);
            case GROUPING:   assert(0);  // Should be ungrouped
            case CAST:       return gen_cast(e.as_cast, target);
            case MEMBER_ACCESS: return gen_member_access(e.as_member_access, target);
            // FIXME: these are basically literals, they should be merged into one type
            case SIZEOF:     return gen_sizeof(e.as_sizeof, target);
            case ALIGNOF:    return gen_alignof(e.as_alignof, target);
            case COUNTOF:    return gen_countof(e.as_countof, target);

            case VA_ARG:     return gen_va_arg(e.as_va_arg, target);
            case TERNARY:    return gen_ternary(e.as_ternary, target);
            case INIT_LIST:
                // Init lists are handled specially in gen_var_decl
                error(e.token, "Initializer list cannot be used as expression");
                return 1;
            case COMPOUND_LITERAL: return gen_compound_literal(e.as_compound_literal, target);
            case GENERIC: return gen_generic(e.as_generic, target);
            case EMBED:
                error(e.token, "#embed not yet supported in code generation as an arbitrary exression");
                return 1;
            case STMT_EXPR: return gen_stmt_expr(e.as_stmt_expr, target);
        }
    }

    int gen_literal(CLiteral* expr, int target){
        if(target == TARGET_IS_NOTHING) return 0;

        str lex = expr.value.lexeme;

        if(expr.value.type == CTokenType.STRING){
            // String literal - emit as-is (DASM handles strings)
            sb.writef("    move r% %\n", target, lex);
        } else if(expr.value.type == CTokenType.CHAR_LITERAL){
            // Character literal - parse the value
            sb.writef("    move r% %\n", target, parse_char_literal(lex));
        } else if(expr.value.type == CTokenType.HEX){
            // Hex literal - strip suffix (u, U, l, L) if present
            str hex_val = lex;
            while(hex_val.length > 0){
                ubyte last = hex_val[$ - 1];
                if(last == 'u' || last == 'U' || last == 'l' || last == 'L'){
                    hex_val = hex_val[0 .. $ - 1];
                } else {
                    break;
                }
            }
            sb.writef("    move r% %\n", target, hex_val);
        } else if(expr.value.type == CTokenType.FLOAT_LITERAL){
            // Float literal - parse and bit-cast to integer
            import dlib.parse_numbers : parse_float;
            auto parsed = parse_float(lex);
            if(parsed.errored){
                error(expr.value, "Unable to parse float literal");
                return 1;
            }
            // Bit-cast double to ulong for storage in register
            ulong bits = *cast(ulong*)&parsed.value;
            sb.writef("    move r% %\n", target, bits);
        } else {
            // Integer literal - strip suffix (u, U, l, L) if present
            str int_val = lex;
            while(int_val.length > 0){
                ubyte last = int_val[$ - 1];
                if(last == 'u' || last == 'U' || last == 'l' || last == 'L'){
                    int_val = int_val[0 .. $ - 1];
                } else {
                    break;
                }
            }
            sb.writef("    move r% %\n", target, int_val);
        }

        return 0;
    }

    int gen_cast(CCast* expr, int target){
        if(expr.cast_type == &TYPE_VOID)
            target = TARGET_IS_NOTHING;

        int err = gen_expression(expr.operand, target);
        if(err) return err;

        if(target != TARGET_IS_NOTHING){
            CType* src_type = expr.operand.type;
            CType* dst_type = expr.cast_type;

            if(src_type is null || dst_type is null) return 0;

            bool src_is_double = src_type.kind == CTypeKind.DOUBLE || src_type.kind == CTypeKind.LONG_DOUBLE;
            bool src_is_float32 = src_type.kind == CTypeKind.FLOAT;
            bool src_is_int = src_type.is_integer();
            bool dst_is_double = dst_type.kind == CTypeKind.DOUBLE || dst_type.kind == CTypeKind.LONG_DOUBLE;
            bool dst_is_float32 = dst_type.kind == CTypeKind.FLOAT;
            bool dst_is_int = dst_type.is_integer();

            // Handle all arithmetic type conversions
            if(src_is_int && dst_is_double){
                // int -> double
                sb.writef("    itod r% r%\n", target, target);
            } else if(src_is_int && dst_is_float32){
                // int -> float32
                sb.writef("    itof r% r%\n", target, target);
            } else if(src_is_double && dst_is_int){
                // double -> int
                sb.writef("    dtoi r% r%\n", target, target);
            } else if(src_is_float32 && dst_is_int){
                // float32 -> int
                sb.writef("    ftoi r% r%\n", target, target);
            } else if(src_is_double && dst_is_float32){
                // double -> float32
                sb.writef("    dtof r% r%\n", target, target);
            } else if(src_is_float32 && dst_is_double){
                // float32 -> double
                sb.writef("    ftod r% r%\n", target, target);
            }
        }

        return 0;
    }

    // Check if an expression produces float32 bits directly (vs double that needs dtof)
    // Constant-folded float32 and cast-to-float32 produce float32 bits directly.
    // Variable reads of float32 produce double (gen_expression adds ftod).
    bool expr_produces_float32_bits(CExpr* expr){
        if(expr is null) return false;
        expr = expr.ungroup();
        CType* t = expr.type;
        if(t is null || t.kind != CTypeKind.FLOAT) return false;

        // Check if constant-foldable (produces float32 bits)
        ConstValue cv = try_eval_constant(expr);
        if(cv.is_const()) return true;

        // Cast to float32 produces float32 bits
        if(expr.kind == CExprKind.CAST) return true;

        return false;
    }

    // Generate implicit type conversion from src_type to dst_type for value in register
    // Note: float32 values are always float32 bits (no auto-promotion to double)
    void gen_implicit_conversion(int reg, CExpr* src_expr, CType* dst_type){
        CType* src_type = src_expr ? src_expr.type : null;
        if(src_type is null || dst_type is null) return;

        bool src_is_double = src_type.kind == CTypeKind.DOUBLE || src_type.kind == CTypeKind.LONG_DOUBLE;
        bool src_is_float32 = src_type.kind == CTypeKind.FLOAT;
        bool src_is_int = src_type.is_integer();
        bool dst_is_double = dst_type.kind == CTypeKind.DOUBLE || dst_type.kind == CTypeKind.LONG_DOUBLE;
        bool dst_is_float32 = dst_type.kind == CTypeKind.FLOAT;
        bool dst_is_int = dst_type.is_integer();

        // Handle all arithmetic type conversions
        if(src_is_int && dst_is_double){
            // int -> double
            sb.writef("    itod r% r%\n", reg, reg);
        } else if(src_is_int && dst_is_float32){
            // int -> float32
            sb.writef("    itof r% r%\n", reg, reg);
        } else if(src_is_double && dst_is_int){
            // double -> int
            sb.writef("    dtoi r% r%\n", reg, reg);
        } else if(src_is_float32 && dst_is_int){
            // float32 -> int
            sb.writef("    ftoi r% r%\n", reg, reg);
        } else if(src_is_double && dst_is_float32){
            // double -> float32
            sb.writef("    dtof r% r%\n", reg, reg);
        } else if(src_is_float32 && dst_is_double){
            // float32 -> double
            sb.writef("    ftod r% r%\n", reg, reg);
        }
        // float32 -> float32: no conversion needed
    }

    int gen_compound_literal(CCompoundLiteral* expr, int target){
        // Compound literal: (type){...}
        // Use pre-allocated stack space and initialize, return address

        // TODO: if TARGET_IS_NOTHING we just evaluate initializers for side effects,
        // but we need that optimization earlier as we are wasting space.
        // But also, who just writes a compound literal and doesn't use it, so very low priority.
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
        if(init_expr !is null){
            if(CInitList* init_list = init_expr.as_init_list()){
                if(lit_type.is_struct_or_union()){
                    // Initialize struct fields (with flattening for array fields)
                    auto fields = lit_type.fields;
                    size_t num_elems = init_list.elements.length;
                    size_t field_idx = 0;
                    size_t array_elem_idx = 0;  // For flattening into array fields

                    for(size_t i = 0; i < num_elems; i++){
                        auto elem = init_list.elements[i];
                        size_t offset;
                        size_t write_size;

                        // Handle designators
                        if(elem.designators.length > 0){
                            if(elem.designators[0].kind == CDesignatorKind.FIELD){
                                str fname = elem.designators[0].field_name;
                                bool found = false;
                                foreach(fi, f; fields){
                                    if(f.name == fname){
                                        field_idx = fi;
                                        array_elem_idx = 0;
                                        found = true;
                                        break;
                                    }
                                }
                                if(!found) continue;
                            } else {
                                continue;  // Skip non-field designators in struct
                            }
                        }

                        // Check if we've exhausted all fields
                        if(field_idx >= fields.length) break;

                        auto field = &fields[field_idx];

                        // Check if field is an array and value is scalar (flattening)
                        if(field.type.is_array() && elem.value.as_init_list() is null){
                            // Flatten scalar into array field
                            size_t elem_size = field.type.element_size();
                            size_t array_size = field.type.array_size;

                            if(array_elem_idx >= array_size){
                                // Array is full, move to next field
                                field_idx++;
                                array_elem_idx = 0;
                                if(field_idx >= fields.length) break;
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
                        if(err) return err;

                        // Add implicit conversion from expression type to field type
                        CType* write_type = field.type;
                        if(field.type.is_array()){
                            write_type = field.type.element_type;
                        }
                        gen_implicit_conversion(val_reg, elem.value, write_type);

                        int field_slot = slot + cast(int)(offset / 8);
                        sb.writef("    add r% rbp %\n", addr_reg, P(field_slot));
                        if(offset % 8 != 0){
                            sb.writef("    add r% r% %\n", addr_reg, addr_reg, offset % 8);
                        }
                        sb.writef("    % r% r%\n", write_instr_for_size(write_size), addr_reg, val_reg);
                    }
                } else if(lit_type.is_array()){
                    // Initialize array elements
                    size_t elem_size = lit_type.element_size();
                    size_t num_elems = init_list.elements.length;
                    size_t current_idx = 0;

                    for(size_t i = 0; i < num_elems; i++){
                        auto elem = init_list.elements[i];
                        size_t offset = current_idx * elem_size;

                        int err = gen_expression(elem.value, val_reg);
                        if(err) return err;

                        // Add implicit conversion from expression type to element type
                        gen_implicit_conversion(val_reg, elem.value, lit_type.element_type);

                        int elem_slot = slot + cast(int)(offset / 8);
                        sb.writef("    add r% rbp %\n", addr_reg, P(elem_slot));
                        if(offset % 8 != 0){
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
        if(target != TARGET_IS_NOTHING){
            sb.writef("    add r% rbp %\n", target, P(slot));
        }
        return 0;
    }

    // GNU statement expression: ({ stmt; stmt; expr; })
    // The value is the last expression statement's value
    int gen_stmt_expr(CStmtExpr* expr, int target){
        // Generate all statements
        foreach(ref stmt; expr.statements){
            // Check if this is the last statement and it's an expression statement
            // In that case, we want to capture its value
            if(&stmt is &expr.statements[$-1]){
                if(auto expr_stmt = stmt.as_expr_stmt()){
                    // Last statement is an expression - its value is the result
                    return gen_expression(expr_stmt.expression, target);
                }
            }
            // Generate the statement normally
            int err = gen_statement(stmt);
            if(err) return err;
        }
        // No result expression (empty block or last was not an expression)
        if(target != TARGET_IS_NOTHING){
            // This shouldn't happen, so we error here just in case.
            error(expr.expr.token, "ICE: void statement expression when value needed");
            return -1;
        }
        return 0;
    }

    // Returns register if expr is already in one, or -1 if it needs evaluation
    int get_expr_reg(CExpr* expr){
        expr = expr.ungroup();
        if(auto id = expr.as_identifier()){
            // Don't return register for arrays (they decay to pointers, need address computation)
            if(id.expr.type && id.expr.type.is_array()) return -1;
            if(int* r = id.name.lexeme in reglocals)
                return *r;
        }
        return -1;
    }

    int generate_expr_maybe_new_reg(CExpr* expr, out int target){
        target = get_expr_reg(expr);
        if(target == -1){
            target = regallocator.allocate();
            int err = gen_expression(expr, target);
            if(err) return err;
        }
        return 0;
    }

    // Returns true if evaluating expr won't clobber registers other than target
    // (i.e., it's a single instruction: literal, reglocal, or enum constant)
    bool is_simple_expr(CExpr* expr){
        if(expr is null) return true;
        expr = expr.ungroup();
        switch(expr.kind)with(expr.kind){
            case LITERAL: return true;
            case IDENTIFIER:{
                CIdentifier* id = expr.as_identifier();
                // Enum constants are simple (just a move immediate)
                if(id.ref_kind == IdentifierRefKind.ENUM_CONST) return true;
                // Arrays need address computation
                if(id.expr.type && id.expr.type.is_array()) return false;
                if(id.name.lexeme in reglocals) return true;
                return false;
            }break;
            case BINARY:{
                CBinary* b = expr.as_binary();
                if(b.op == CTokenType.COMMA)
                    return is_simple_expr(b.left) && is_simple_expr(b.right);
                return false;
            }
            case UNARY:{
                return false;
                // TODO: figure out if all unary can be done in one reg.
                // Probably not because of post increment?
                CUnary *u = expr.as_unary();
                return is_simple_expr(u.operand);
            }
            default:
                return false; // TODO
        }
    }

    int gen_identifier(CIdentifier* expr, int target){
        import cfront.c_ast : IdentifierRefKind;
        if(target == TARGET_IS_NOTHING) return 0;

        str name = expr.name.lexeme;
        CType* t = expr.expr.type;

        final switch(expr.ref_kind){
            case IdentifierRefKind.ENUM_CONST:
                sb.writef("    move r% %\n", target, expr.enum_value);
                return 0;

            case IdentifierRefKind.LOCAL_VAR:
                // Array-to-pointer decay for local arrays
                if(t && t.is_array()){
                    if(int* offset = name in stacklocals){
                        sb.writef("    add r% rbp %\n", target, P(*offset));
                        return 0;
                    }
                }
                // Register-allocated local
                if(int* r = name in reglocals){
                    if(target == *r) return 0;
                    sb.writef("    move r% r%\n", target, *r);
                    return 0;
                }
                // Stack-allocated local
                if(int* offset = name in stacklocals){
                    sb.writef("    local_read r% %\n", target, P(*offset));
                    // Note: float32 values stay as float32 bits - callers handle conversion
                    return 0;
                }
                error(expr.expr.token, "Local variable not found in stack or registers");
                return -1;

            case IdentifierRefKind.GLOBAL_VAR:
                // Array-to-pointer decay for global arrays
                if(t && t.is_array()){
                    sb.writef("    move r% var %\n", target, name);
                    return 0;
                }
                // Read global variable value
                if(t is null){
                    error(expr.expr.token, "Global variable has no type");
                    return -1;
                }
                sb.writef("    move r% var %\n", target, name);
                sb.writef("    % r% r%\n", read_instr_for_size(t.size_of(), t.is_signed), target, target);
                return 0;

            case IdentifierRefKind.EXTERN_VAR:
                used_objs[name] = true;  // Track for dlimport
                str* obj_alias = name in extern_objs;
                if(obj_alias is null){
                    error(expr.expr.token, "Extern variable not found in module alias table");
                    return -1;
                }
                // Array-to-pointer decay for extern arrays
                if(t && t.is_array()){
                    sb.writef("    move r% var %.%\n", target, *obj_alias, name);
                    return 0;
                }
                // Read extern variable value
                if(t is null){
                    error(expr.expr.token, "Extern variable has no type");
                    return -1;
                }
                sb.writef("    move r% var %.%\n", target, *obj_alias, name);
                sb.writef("    % r% r%\n", read_instr_for_size(t.size_of(), t.is_signed), target, target);
                return 0;

            case IdentifierRefKind.FUNCTION:
                // Function-to-pointer decay
                sb.writef("    move r% function %\n", target, name);
                return 0;

            case IdentifierRefKind.EXTERN_FUNC:
                used_funcs[name] = true;  // Track for dlimport
                str* func_alias = name in extern_funcs;
                if(func_alias is null){
                    error(expr.expr.token, "Extern function not found in module alias table");
                    return -1;
                }
                sb.writef("    move r% function %.%\n", target, *func_alias, name);
                return 0;

            case IdentifierRefKind.BUILTIN:
                // Builtins shouldn't be used as values - they should be called
                error(expr.expr.token, "Builtin function used as value");
                return -1;

            case IdentifierRefKind.UNKNOWN:
                error(expr.expr.token, "Unresolved identifier");
                return -1;
        }
    }

    int gen_binary(CBinary* expr, int target){
        // evaluate both sides for side effects
        if(target == TARGET_IS_NOTHING){
            int err = gen_expression(expr.left, TARGET_IS_NOTHING);
            if(err) return err;
            err = gen_expression(expr.right, TARGET_IS_NOTHING);
            return err;
        }

        // Comma operator: evaluate left for side effects, return right value
        if(expr.op == CTokenType.COMMA){
            int err = gen_expression(expr.left, TARGET_IS_NOTHING);
            if(err) return err;
            return gen_expression(expr.right, target);
        }

        // Constant folding: if both operands are literals, compute at compile time
        CLiteral* left_lit = expr.left.as_literal();
        CLiteral* right_lit = expr.right.as_literal();
        if(left_lit !is null && right_lit !is null){
            // TODO: pseudo literals like sizeof, _Countof, etc.
            // Probably we shouldn't do this here anyway?
            // Have a try-constant eval function?
            import dlib.parse_numbers : parse_unsigned_human;

            // Check if either operand is unsigned (has u/U suffix)
            static bool is_unsigned_literal(str lex){
                if(lex.length == 0) return false;
                char last = lex[$ - 1];
                if(last == 'u' || last == 'U') return true;
                if(lex.length >= 2){
                    char prev = lex[$ - 2];
                    // Check for ul, uL, Ul, UL, lu, lU, Lu, LU
                    if((last == 'l' || last == 'L') && (prev == 'u' || prev == 'U')) return true;
                    if((last == 'u' || last == 'U') && (prev == 'l' || prev == 'L')) return true;
                }
                return false;
            }

            bool is_unsigned = is_unsigned_literal(left_lit.value.lexeme) ||
                               is_unsigned_literal(right_lit.value.lexeme);

            // Parse left operand
            ulong lhs_val;
            if(left_lit.value.type == CTokenType.CHAR_LITERAL){
                lhs_val = parse_char_literal(left_lit.value.lexeme);
            } else {
                auto parsed = parse_unsigned_human(left_lit.value.lexeme);
                if(parsed.errored) goto no_fold;
                lhs_val = parsed.value;
            }

            // Parse right operand
            ulong rhs_val;
            if(right_lit.value.type == CTokenType.CHAR_LITERAL){
                rhs_val = parse_char_literal(right_lit.value.lexeme);
            } else {
                auto parsed = parse_unsigned_human(right_lit.value.lexeme);
                if(parsed.errored) goto no_fold;
                rhs_val = parsed.value;
            }

            // Compute result at compile time
            ulong result;
            switch(expr.op) with (CTokenType){
                // These operations are the same for signed/unsigned at bit level
                case PLUS:          result = lhs_val + rhs_val; break;
                case MINUS:         result = lhs_val - rhs_val; break;
                case STAR:          result = lhs_val * rhs_val; break;
                case AMP:           result = lhs_val & rhs_val; break;
                case PIPE:          result = lhs_val | rhs_val; break;
                case CARET:         result = lhs_val ^ rhs_val; break;
                case LESS_LESS:     result = lhs_val << rhs_val; break;
                case EQUAL_EQUAL:   result = lhs_val == rhs_val ? 1 : 0; break;
                case BANG_EQUAL:    result = lhs_val != rhs_val ? 1 : 0; break;
                case AMP_AMP:       result = (lhs_val != 0 && rhs_val != 0) ? 1 : 0; break;
                case PIPE_PIPE:     result = (lhs_val != 0 || rhs_val != 0) ? 1 : 0; break;

                // These differ for signed vs unsigned
                case SLASH:
                    if(rhs_val == 0) goto no_fold;
                    if(is_unsigned)
                        result = lhs_val / rhs_val;
                    else
                        result = cast(ulong)(cast(long)lhs_val / cast(long)rhs_val);
                    break;
                case PERCENT:
                    if(rhs_val == 0) goto no_fold;
                    if(is_unsigned)
                        result = lhs_val % rhs_val;
                    else
                        result = cast(ulong)(cast(long)lhs_val % cast(long)rhs_val);
                    break;
                case GREATER_GREATER:
                    if(is_unsigned)
                        result = lhs_val >> rhs_val;
                    else
                        result = cast(ulong)(cast(long)lhs_val >> rhs_val);
                    break;
                case LESS:
                    if(is_unsigned)
                        result = lhs_val < rhs_val ? 1 : 0;
                    else
                        result = cast(long)lhs_val < cast(long)rhs_val ? 1 : 0;
                    break;
                case LESS_EQUAL:
                    if(is_unsigned)
                        result = lhs_val <= rhs_val ? 1 : 0;
                    else
                        result = cast(long)lhs_val <= cast(long)rhs_val ? 1 : 0;
                    break;
                case GREATER:
                    if(is_unsigned)
                        result = lhs_val > rhs_val ? 1 : 0;
                    else
                        result = cast(long)lhs_val > cast(long)rhs_val ? 1 : 0;
                    break;
                case GREATER_EQUAL:
                    if(is_unsigned)
                        result = lhs_val >= rhs_val ? 1 : 0;
                    else
                        result = cast(long)lhs_val >= cast(long)rhs_val ? 1 : 0;
                    break;
                default:
                    goto no_fold;
            }

            sb.writef("    move r% %\n", target, result);
            return 0;
        }
    no_fold:

        int before = regallocator.alloced;

        // Check if left operand is already in a register
        int lhs = get_expr_reg(expr.left);
        if(lhs < 0){
            lhs = target;
            int err = gen_expression(expr.left, lhs);
            if(err) return err;
        }

        // Check for pointer/array arithmetic scaling
        CType* left_type = expr.left.type;
        CType* right_type = expr.right.type;
        bool left_is_ptr = left_type && (left_type.is_pointer() || left_type.is_array());
        bool right_is_ptr = right_type && (right_type.is_pointer() || right_type.is_array());
        size_t left_elem_size = left_is_ptr ? left_type.element_size() : 0;
        size_t right_elem_size = right_is_ptr ? right_type.element_size() : 0;

        // Check for float operations
        bool left_is_float = left_type && left_type.is_float();
        bool right_is_float = right_type && right_type.is_float();
        if(left_is_float || right_is_float){
            bool left_is_float32 = left_type && left_type.is_float32();
            bool right_is_float32 = right_type && right_type.is_float32();

            // Determine instruction set: F* for float32+float32, D* for double
            // After frontend usual_arithmetic_conversions:
            // - float+float stays float (use F*)
            // - float+double becomes double+double (use D*)
            // - float+int becomes double+double (use D*)
            bool use_float32 = left_is_float32 && right_is_float32;

            // Generate RHS into a register
            int rhs = get_expr_reg(expr.right);
            if(rhs < 0){
                rhs = regallocator.allocate();
                int err = gen_expression(expr.right, rhs);
                if(err) return err;
            }
            // Convert non-float operand to float if needed (fallback if frontend didn't)
            if(left_is_float && !right_is_float){
                sb.writef("    % r% r%\n", use_float32 ? "itof" : "itod", rhs, rhs);
            } else if(!left_is_float && right_is_float){
                sb.writef("    % r% r%\n", use_float32 ? "itof" : "itod", lhs, lhs);
            }
            // Generate float operation using F* or D* instructions
            switch(expr.op) with (CTokenType){
                case PLUS:
                    sb.writef("    % r% r% r%\n", use_float32 ? "fadd" : "dadd", target, lhs, rhs);
                    break;
                case MINUS:
                    sb.writef("    % r% r% r%\n", use_float32 ? "fsub" : "dsub", target, lhs, rhs);
                    break;
                case STAR:
                    sb.writef("    % r% r% r%\n", use_float32 ? "fmul" : "dmul", target, lhs, rhs);
                    break;
                case SLASH:
                    sb.writef("    % r% r% r%\n", use_float32 ? "fdiv" : "ddiv", target, lhs, rhs);
                    break;
                case EQUAL_EQUAL:
                    sb.writef("    % r% r%\n", use_float32 ? "fcmp" : "dcmp", lhs, rhs);
                    sb.writef("    move r% 0\n", target);
                    sb.writef("    cmov eq r% 1\n", target);
                    break;
                case BANG_EQUAL:
                    sb.writef("    % r% r%\n", use_float32 ? "fcmp" : "dcmp", lhs, rhs);
                    sb.writef("    move r% 0\n", target);
                    sb.writef("    cmov ne r% 1\n", target);
                    break;
                case LESS:
                    sb.writef("    % r% r%\n", use_float32 ? "fcmp" : "dcmp", lhs, rhs);
                    sb.writef("    move r% 0\n", target);
                    sb.writef("    cmov lt r% 1\n", target);
                    break;
                case LESS_EQUAL:
                    sb.writef("    % r% r%\n", use_float32 ? "fcmp" : "dcmp", lhs, rhs);
                    sb.writef("    move r% 0\n", target);
                    sb.writef("    cmov le r% 1\n", target);
                    break;
                case GREATER:
                    sb.writef("    % r% r%\n", use_float32 ? "fcmp" : "dcmp", lhs, rhs);
                    sb.writef("    move r% 0\n", target);
                    sb.writef("    cmov gt r% 1\n", target);
                    break;
                case GREATER_EQUAL:
                    sb.writef("    % r% r%\n", use_float32 ? "fcmp" : "dcmp", lhs, rhs);
                    sb.writef("    move r% 0\n", target);
                    sb.writef("    cmov ge r% 1\n", target);
                    break;
                default:
                    error(expr.expr.token, "Unsupported float operation");
                    return 1;
            }
            regallocator.reset_to(before);
            return 0;
        }

        // Check for literal RHS optimization
        CExpr* right = expr.right;
        if(CLiteral* lit = right.as_literal()){
            ConstValue cv = try_eval_constant(right);
            // TODO: try_eval_const should handle all cases since this is a literal.
            if(!cv.is_const){
                str rhs = right.token.lexeme;
                // FIXME: I think this is just for string literals at this point?
                switch(expr.op) with (CTokenType){
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
                    case AMP_AMP:
                        // Logical AND: result is 1 iff both lhs and rhs are non-zero
                        // For literal RHS, we know at compile time if rhs is 0
                        if(rhs == "0"){
                            // x && 0 is always 0
                            sb.writef("    move r% 0\n", target);
                        } else {
                            // x && non-zero: result is (x != 0)
                            sb.writef("    cmp r% 0\n", lhs);
                            sb.writef("    move r% 0\n", target);
                            sb.writef("    cmov ne r% 1\n", target);
                        }
                        break;
                    case PIPE_PIPE:
                        // Logical OR: result is 1 iff either lhs or rhs is non-zero
                        // For literal RHS, we know at compile time if rhs is 0
                        if(rhs != "0"){
                            // x || non-zero is always 1
                            sb.writef("    move r% 1\n", target);
                        } else {
                            // x || 0: result is (x != 0)
                            sb.writef("    cmp r% 0\n", lhs);
                            sb.writef("    move r% 0\n", target);
                            sb.writef("    cmov ne r% 1\n", target);
                        }
                        break;
                    default:
                        error(expr.expr.token, "Unhandled binary operator");
                        return 1;
                }
                return 0;
            }
            if(!cv.is_const){
                errorf(right.token, "Constant folding failed with a literal: ", str_for(right.type.kind));
                return 1;
            }
            auto rhs = cv.uint_val; // FIXME: type check

            switch(expr.op) with (CTokenType){
                case PLUS:
                    if(left_elem_size > 1){
                        rhs *= left_elem_size;
                    }
                    sb.writef("    add r% r% %\n", target, lhs, rhs);
                    break;
                case MINUS:
                    // Literals can't be pointers, so this is ptr - int
                    if(left_elem_size > 1){
                        rhs *= left_elem_size;
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
                case AMP_AMP:
                    // Logical AND: result is 1 iff both lhs and rhs are non-zero
                    // For literal RHS, we know at compile time if rhs is 0
                    if(rhs == 0){
                        // x && 0 is always 0
                        sb.writef("    move r% 0\n", target);
                    } else {
                        // x && non-zero: result is (x != 0)
                        sb.writef("    cmp r% 0\n", lhs);
                        sb.writef("    move r% 0\n", target);
                        sb.writef("    cmov ne r% 1\n", target);
                    }
                    break;
                case PIPE_PIPE:
                    // Logical OR: result is 1 iff either lhs or rhs is non-zero
                    // For literal RHS, we know at compile time if rhs is 0
                    if(rhs != 0){
                        // x || non-zero is always 1
                        sb.writef("    move r% 1\n", target);
                    } else {
                        // x || 0: result is (x != 0)
                        sb.writef("    cmp r% 0\n", lhs);
                        sb.writef("    move r% 0\n", target);
                        sb.writef("    cmov ne r% 1\n", target);
                    }
                    break;
                default:
                    error(expr.expr.token, "Unhandled binary operator");
                    return 1;
            }
            return 0;
        }

        // General case: check if RHS is already in a register
        int rhs = get_expr_reg(right);
        if(rhs < 0){
            rhs = regallocator.allocate();
            int err = gen_expression(right, rhs);
            if(err) return err;
        }

        switch(expr.op) with (CTokenType){
            case PLUS:
                if(left_elem_size > 1 && !right_is_ptr){
                    // Pointer + integer: scale integer by element size
                    sb.writef("    mul r% r% %\n", rhs, rhs, left_elem_size);
                } else if(right_elem_size > 1 && !left_is_ptr){
                    // Integer + pointer: scale integer by element size
                    sb.writef("    mul r% r% %\n", lhs, lhs, right_elem_size);
                }
                sb.writef("    add r% r% r%\n", target, lhs, rhs);
                break;
            case MINUS:
                if(left_is_ptr && right_is_ptr){
                    // Pointer - pointer: check element sizes match, subtract, divide by size
                    if(left_elem_size != right_elem_size){
                        error(expr.expr.token, "Subtraction of pointers to different types");
                        return 1;
                    }
                    sb.writef("    sub r% r% r%\n", target, lhs, rhs);
                    if(left_elem_size > 1){
                        sb.writef("    div r% rjunk r% %\n", target, target, left_elem_size);
                    }
                } else if(left_is_ptr){
                    // Pointer - integer: scale integer by element size
                    if(left_elem_size > 1){
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

    int gen_unary(CUnary* expr, int target){
        if(expr.op == CTokenType.AMP){
            // Address-of operator
            if(CIdentifier* id = expr.operand.as_identifier()){
                if(target == TARGET_IS_NOTHING) return 0;
                str name = id.name.lexeme;

                final switch(id.ref_kind){
                    case IdentifierRefKind.LOCAL_VAR:
                        if(int* offset = name in stacklocals){
                            sb.writef("    add r% rbp %\n", target, P(*offset));
                            return 0;
                        }
                        if(name in reglocals){
                            error(expr.expr.token, "Cannot take address of register variable");
                            return 1;
                        }
                        error(expr.expr.token, "Local variable not found");
                        return 1;

                    case IdentifierRefKind.GLOBAL_VAR:
                        sb.writef("    move r% var %\n", target, name);
                        return 0;

                    case IdentifierRefKind.EXTERN_VAR:
                        used_objs[name] = true;
                        if(str* obj_alias = name in extern_objs){
                            sb.writef("    move r% var %.%\n", target, *obj_alias, name);
                        } else {
                            error(expr.expr.token, "Extern variable not found in alias table");
                            return 1;
                        }
                        return 0;

                    case IdentifierRefKind.FUNCTION:
                        sb.writef("    move r% function %\n", target, name);
                        return 0;

                    case IdentifierRefKind.EXTERN_FUNC:
                        used_funcs[name] = true;
                        if(str* func_alias = name in extern_funcs){
                            sb.writef("    move r% function %.%\n", target, *func_alias, name);
                        } else {
                            if(CFunction** f = name in func_info){
                                if((*f).is_definition){
                                    sb.writef("    move r% function %\n", target, name);
                                }
                                else {
                                    error(expr.expr.token, "Extern function not found in alias table");
                                    return 1;
                                }
                            }
                            else{
                                error(expr.expr.token, "Extern function not found in alias table");
                                return 1;
                            }
                        }
                        return 0;

                    case IdentifierRefKind.ENUM_CONST:
                    case IdentifierRefKind.BUILTIN:
                    case IdentifierRefKind.UNKNOWN:
                        error(expr.expr.token, "Cannot take address of this identifier");
                        return 1;
                }
            }

            // Address of member access: &p.x or &pp->x
            if(CMemberAccess* ma = expr.operand.as_member_access()){
                if(target == TARGET_IS_NOTHING) return 0;

                // Get the struct type
                CType* obj_type = ma.object.type;
                if(obj_type is null){
                    error(expr.expr.token, "Cannot determine type for member access");
                    return 1;
                }

                CType* struct_type = obj_type;
                if(ma.is_arrow){
                    if(!obj_type.is_pointer()){
                        error(expr.expr.token, "'->' requires pointer type");
                        return 1;
                    }
                    struct_type = obj_type.pointed_to;
                }

                if(struct_type is null || !struct_type.is_struct_or_union()){
                    error(expr.expr.token, "Member access requires struct/union type");
                    return 1;
                }

                // Find the field offset
                StructField* field = struct_type.get_field(ma.member.lexeme);
                if(field is null){
                    error(expr.expr.token, "Unknown field");
                    return 1;
                }

                if(ma.is_arrow){
                    // &pp->x: get pointer value, add offset
                    int err = gen_expression(ma.object, target);
                    if(err) return err;
                } else {
                    // &p.x: get address of struct, add offset
                    int err = gen_struct_address(ma.object, target);
                    if(err) return err;
                }

                // Add field offset
                if(field.offset != 0){
                    sb.writef("    add r% r% %\n", target, target, field.offset);
                }
                return 0;
            }

            // Address of array subscript: &arr[i]
            if(CSubscript* sub = expr.operand.as_subscript()){
                if(target == TARGET_IS_NOTHING) return 0;
                // Just compute the address without dereferencing
                return gen_subscript_address(sub, target);
            }

            // Address of compound literal: &(type){...}
            // gen_compound_literal already returns the address
            if(expr.operand.as_compound_literal() !is null){
                return gen_expression(expr.operand, target);
            }

            // Address of dereference: &*ptr == ptr
            if(CUnary* deref = expr.operand.as_unary()){
                if(deref.op == CTokenType.STAR){
                    // &*ptr cancels out, just evaluate the pointer
                    return gen_expression(deref.operand, target);
                }
            }

            error(expr.expr.token, "Invalid operand for address-of");
            return 1;
        }

        if(expr.op == CTokenType.STAR){
            // Dereference operator
            int err = gen_expression(expr.operand, target);
            if(err) return err;
            if(target != TARGET_IS_NOTHING){
                // Get the pointed-to type's size and signedness for proper sized read
                CType* ptr_type = expr.operand.type;
                size_t elem_size = (ptr_type && ptr_type.is_pointer()) ? ptr_type.element_size() : 8;
                CType* elem_type = (ptr_type && ptr_type.is_pointer()) ? ptr_type.element_type() : null;
                bool is_signed = elem_type.is_signed;
                sb.writef("    % r% r%\n", read_instr_for_size(elem_size, is_signed), target, target);
            }
            return 0;
        }

        // Handle increment/decrement specially - they have side effects even when result is discarded
        // FIXME: the clanker probably broke this, probably we can no-op some stuff with TARGET_IS_NOTHING
        if(expr.op == CTokenType.PLUS_PLUS || expr.op == CTokenType.MINUS_MINUS){
            bool is_inc = expr.op == CTokenType.PLUS_PLUS;
            str op_instr = is_inc ? "add" : "sub";
            int before = regallocator.alloced;

            // Get operand type to determine increment amount (1 for scalars, element_size for pointers)
            CType* operand_type = expr.operand.type;
            size_t inc_amount = 1;
            if(operand_type && operand_type.is_pointer()){
                inc_amount = operand_type.element_size();
            }

            if(CIdentifier* id = expr.operand.as_identifier()){
                str name = id.name.lexeme;
                if(int* r = name in reglocals){
                    // Register variable
                    if(expr.is_prefix){
                        sb.writef("    % r% r% %\n", op_instr, *r, *r, inc_amount);
                        if(target != TARGET_IS_NOTHING && target != *r)
                            sb.writef("    move r% r%\n", target, *r);
                    } else {
                        if(target != TARGET_IS_NOTHING && target != *r)
                            sb.writef("    move r% r%\n", target, *r);
                        sb.writef("    % r% r% %\n", op_instr, *r, *r, inc_amount);
                    }
                } else if(int* offset = name in stacklocals){
                    // Stack variable
                    size_t var_size = operand_type.size_of();
                    bool is_signed = operand_type.is_signed;
                    str read_instr = read_instr_for_size(var_size, is_signed);
                    str write_instr = write_instr_for_size(var_size);
                    int addr_reg = regallocator.allocate();
                    int val_reg = target != TARGET_IS_NOTHING ? target : regallocator.allocate();

                    // Get address of variable
                    sb.writef("    add r% rbp %\n", addr_reg, P(*offset));
                    // Read current value
                    sb.writef("    % r% r%\n", read_instr, val_reg, addr_reg);

                    if(expr.is_prefix){
                        // ++x: increment, write, return new value
                        sb.writef("    % r% r% %\n", op_instr, val_reg, val_reg, inc_amount);
                        sb.writef("    % r% r%\n", write_instr, addr_reg, val_reg);
                    } else {
                        // x++: save old value, increment, write, return old value
                        int new_val = regallocator.allocate();
                        sb.writef("    % r% r% %\n", op_instr, new_val, val_reg, inc_amount);
                        sb.writef("    % r% r%\n", write_instr, addr_reg, new_val);
                    }
                } else {
                    // Global variable (including static locals)
                    size_t var_size = operand_type ? operand_type.size_of() : 4;
                    bool is_signed = operand_type ? operand_type.is_signed : true;
                    str read_instr = read_instr_for_size(var_size, is_signed);
                    str write_instr = write_instr_for_size(var_size);
                    int addr_reg = regallocator.allocate();
                    int val_reg = target != TARGET_IS_NOTHING ? target : regallocator.allocate();

                    // Get address of global variable
                    sb.writef("    move r% var %\n", addr_reg, name);
                    // Read current value
                    sb.writef("    % r% r%\n", read_instr, val_reg, addr_reg);

                    if(expr.is_prefix){
                        // ++x: increment, write, return new value
                        sb.writef("    % r% r% %\n", op_instr, val_reg, val_reg, inc_amount);
                        sb.writef("    % r% r%\n", write_instr, addr_reg, val_reg);
                    } else {
                        // x++: save old value, increment, write, return old value
                        int new_val = regallocator.allocate();
                        sb.writef("    % r% r% %\n", op_instr, new_val, val_reg, inc_amount);
                        sb.writef("    % r% r%\n", write_instr, addr_reg, new_val);
                    }
                }
                regallocator.reset_to(before);
                return 0;
            }

            // Increment/decrement on subscript: ++arr[i] or arr[i]++
            if(CSubscript* sub = expr.operand.as_subscript()){
                int addr_reg = regallocator.allocate();
                int err = gen_subscript_address(sub, addr_reg);
                if(err) return err;

                size_t elem_size = operand_type.size_of();
                bool is_signed = operand_type.is_signed;
                str read_instr = read_instr_for_size(elem_size, is_signed);
                str write_instr = write_instr_for_size(elem_size);
                int val_reg = target != TARGET_IS_NOTHING ? target : regallocator.allocate();

                // Read current value
                sb.writef("    % r% r%\n", read_instr, val_reg, addr_reg);

                if(expr.is_prefix){
                    sb.writef("    % r% r% %\n", op_instr, val_reg, val_reg, inc_amount);
                    sb.writef("    % r% r%\n", write_instr, addr_reg, val_reg);
                } else {
                    int new_val = regallocator.allocate();
                    sb.writef("    % r% r% %\n", op_instr, new_val, val_reg, inc_amount);
                    sb.writef("    % r% r%\n", write_instr, addr_reg, new_val);
                }
                regallocator.reset_to(before);
                return 0;
            }

            // Increment/decrement on dereference: ++*p or (*p)++
            if(CUnary* deref = expr.operand.as_unary()){
                if(deref.op == CTokenType.STAR){
                    int addr_reg = regallocator.allocate();
                    int err = gen_expression(deref.operand, addr_reg);
                    if(err) return err;

                    size_t elem_size = operand_type.size_of();
                    bool is_signed = operand_type.is_signed;
                    str read_instr = read_instr_for_size(elem_size, is_signed);
                    str write_instr = write_instr_for_size(elem_size);
                    int val_reg = target != TARGET_IS_NOTHING ? target : regallocator.allocate();

                    // Read current value
                    sb.writef("    % r% r%\n", read_instr, val_reg, addr_reg);

                    if(expr.is_prefix){
                        sb.writef("    % r% r% %\n", op_instr, val_reg, val_reg, inc_amount);
                        sb.writef("    % r% r%\n", write_instr, addr_reg, val_reg);
                    } else {
                        int new_val = regallocator.allocate();
                        sb.writef("    % r% r% %\n", op_instr, new_val, val_reg, inc_amount);
                        sb.writef("    % r% r%\n", write_instr, addr_reg, new_val);
                    }
                    regallocator.reset_to(before);
                    return 0;
                }
            }

            // Increment/decrement on member access: ++p->field or p->field++
            if(CMemberAccess* ma = expr.operand.as_member_access()){
                int addr_reg = regallocator.allocate();

                // Get the struct type
                CType* obj_type = ma.object.type;
                if(obj_type is null){
                    error(expr.expr.token, "Cannot determine type for member access");
                    return 1;
                }

                CType* struct_type = obj_type;
                if(ma.is_arrow){
                    if(!obj_type.is_pointer()){
                        error(expr.expr.token, "'->' requires pointer type");
                        return 1;
                    }
                    struct_type = obj_type.pointed_to;
                }

                if(struct_type is null || !struct_type.is_struct_or_union()){
                    error(expr.expr.token, "Member access requires struct/union type");
                    return 1;
                }

                // Find the field
                StructField* field = struct_type.get_field(ma.member.lexeme);
                if(field is null){
                    error(expr.expr.token, "Unknown field");
                    return 1;
                }

                // Get address of the field
                if(ma.is_arrow){
                    // p->field: get pointer value, add offset
                    int err2 = gen_expression(ma.object, addr_reg);
                    if(err2) return err2;
                } else {
                    // p.field: get address of struct, add offset
                    int err2 = gen_struct_address(ma.object, addr_reg);
                    if(err2) return err2;
                }
                if(field.offset != 0){
                    sb.writef("    add r% r% %\n", addr_reg, addr_reg, field.offset);
                }

                size_t elem_size = field.type.size_of();
                bool is_signed = field.type.is_signed;
                str read_instr = read_instr_for_size(elem_size, is_signed);
                str write_instr = write_instr_for_size(elem_size);
                int val_reg = target != TARGET_IS_NOTHING ? target : regallocator.allocate();

                // Read current value
                sb.writef("    % r% r%\n", read_instr, val_reg, addr_reg);

                if(expr.is_prefix){
                    sb.writef("    % r% r% %\n", op_instr, val_reg, val_reg, inc_amount);
                    sb.writef("    % r% r%\n", write_instr, addr_reg, val_reg);
                } else {
                    int new_val = regallocator.allocate();
                    sb.writef("    % r% r% %\n", op_instr, new_val, val_reg, inc_amount);
                    sb.writef("    % r% r%\n", write_instr, addr_reg, new_val);
                }
                regallocator.reset_to(before);
                return 0;
            }

            regallocator.reset_to(before);
            return 0;
        }

        // Other unary operators
        int err = gen_expression(expr.operand, target);
        if(err) return err;

        if(target != TARGET_IS_NOTHING){
            CType* operand_type = expr.operand.type;
            bool is_float_op = operand_type && operand_type.is_float();

            switch(expr.op) with (CTokenType){
                case PLUS:
                    // Unary plus is a no-op
                    break;
                case MINUS:
                    if(is_float_op){
                        if(operand_type.is_float32())
                            sb.writef("    fneg r% r%\n", target, target);
                        else
                            sb.writef("    dneg r% r%\n", target, target);
                    } else {
                        sb.writef("    neg r% r%\n", target, target);
                    }
                    break;
                case BANG:
                    sb.writef("    not r% r%\n", target, target);
                    break;
                case TILDE:
                    // Bitwise NOT - XOR with -1
                    sb.writef("    xor r% r% -1\n", target, target);
                    break;
                default:
                    error(expr.expr.token, "Unhandled unary operator");
                    return 1;
            }
        }
        return 0;
    }

    int gen_call(CCall* expr, int target){
        int before = regallocator.alloced;

        // Check if callee returns a struct and if it's varargs
        uint varargs_float_mask = 0;  // Bitmask of which args are floats (for varargs calls)
        CType* callee_func_type = expr.callee_function_type;
        if(!callee_func_type){
            error(expr.expr.token, "ICE: generating a call to a non-function or non-function pointer");
            return -1;
        }
        bool callee_is_varargs = callee_func_type.is_varargs;
        bool callee_uses_hidden_ptr = callee_func_type.return_type.is_struct_or_union && !fits_in_registers(callee_func_type.return_type);
        // For large struct/union returns, arg registers shift by 1 (rarg1 = hidden return ptr)
        int arg_offset = callee_uses_hidden_ptr ? 1 : 0;

        // Helper to get number of register slots for an argument
        int arg_slots(CType* arg){
            if(arg.is_struct_or_union() && fits_in_registers(arg)){
                return struct_return_regs(arg);
            }
            return 1;
        }
        int n_fixed_args = 0;
        foreach(CType* p; callee_func_type.param_types){
            n_fixed_args += arg_slots(p);
        }

        // Calculate starting slot for argument i
        int get_arg_slot(size_t idx){
            int slot = arg_offset;
            foreach(j, arg; callee_func_type.param_types){
                if(j == idx) return slot;
                slot += arg_slots(arg);
            }
            return slot;
        }

        // Calculate total slots (for arg count)
        int total_slots = arg_offset;
        foreach(arg; expr.args){
            total_slots += arg_slots(arg.type);
        }

        // Count stack args (slots beyond N_REG_ARGS, or varargs beyond fixed args)
        int n_stack_args = 0;
        foreach(i, arg; expr.args){
            int slot = get_arg_slot(i);
            bool is_vararg = callee_is_varargs && i >= n_fixed_args;
            if(slot >= N_REG_ARGS || is_vararg){
                n_stack_args += arg_slots(arg.type);
            }
        }

        // Collect reglocals in R0-R7 that need saving across calls
        // These are registers used by the optimization that puts params in R(max_call_slots)..R7
        int[N_REG_ARGS] low_reglocals_to_save;
        int n_low_reglocals = 0;
        foreach(ref item; reglocals.items()){
            int r = item.value;
            if(r >= 0 && r < N_REG_ARGS){
                low_reglocals_to_save[n_low_reglocals++] = r;
            }
        }

        // For varargs calls, evaluate varargs first (push them), then fixed args
        // This avoids clobbering fixed args when evaluating varargs into r0
        // IMPORTANT: We save registers BEFORE pushing varargs so they end up below
        // the varargs on the stack (varargs must be on top for native trampoline)
        if(callee_is_varargs){
            // Save low reglocals first (R0-R7 that hold our locals)
            for(int i = 0; i < n_low_reglocals; i++){
                sb.writef("    push r%\n", low_reglocals_to_save[i]);
            }
            // Save R8+ scratch regs
            for(int i = N_REG_ARGS; i < before; i++){
                sb.writef("    push r%\n", i);
            }

            // Push varargs (those beyond n_fixed_args)
            // Also build float mask: 2 bits per arg, 0b10 = double (floats promoted to double)
            for(size_t i = n_fixed_args; i < expr.args.length; i++){
                CExpr* arg = expr.args[i];
                CType* arg_type = arg.type;

                // Track if this arg is a float/double (for calling convention)
                if(arg_type && arg_type.is_float() && i < 16){
                    // Set bits for this arg position: 0b10 = double (float promoted to double)
                    varargs_float_mask |= (0b10 << (i * 2));
                }

                if(arg_type && arg_type.is_struct_or_union()){
                    if(fits_in_registers(arg_type)){
                        // Small struct: push each word
                        int addr_reg = regallocator.allocate();
                        int err = gen_struct_address(arg, addr_reg);
                        if(err) return err;
                        sb.writef("    read r0 r%\n", addr_reg);
                        sb.write("    push r0\n");
                        if(arg_type.size_of() > 8){
                            sb.writef("    add r% r% 8\n", addr_reg, addr_reg);
                            sb.writef("    read r0 r%\n", addr_reg);
                            sb.write("    push r0\n");
                        }
                        regallocator.reset_to(regallocator.alloced - 1);
                    } else {
                        // Large struct: push address
                        int err = gen_struct_address(arg, 0);
                        if(err) return err;
                        sb.write("    push r0\n");
                    }
                } else {
                    int err = gen_expression(arg, 0);
                    if(err) return err;
                    sb.write("    push r0\n");
                }
            }
            // Now evaluate fixed args into registers (no preservation needed)
            for(size_t i = 0; i < n_fixed_args; i++){
                CExpr* arg = expr.args[i];
                int slot = get_arg_slot(i);
                CType* arg_type = arg.type;
                int num_slots = arg_slots(arg.type);

                // Track if this fixed arg is a float/double (for calling convention)
                if(arg_type && arg_type.is_float() && i < 16){
                    varargs_float_mask |= (0b10 << (i * 2));
                }

                if(arg_type && arg_type.is_struct_or_union()){
                    if(fits_in_registers(arg_type)){
                        int addr_reg = regallocator.allocate();
                        int err = gen_struct_address(arg, addr_reg);
                        if(err) return err;
                        sb.writef("    read rarg% r%\n", 1 + slot, addr_reg);
                        if(arg_type.size_of() > 8){
                            sb.writef("    add r% r% 8\n", addr_reg, addr_reg);
                            sb.writef("    read rarg% r%\n", 2 + slot, addr_reg);
                        }
                        regallocator.reset_to(regallocator.alloced - 1);
                    } else {
                        int err = gen_struct_address(arg, RARG1 + slot);
                        if(err) return err;
                    }
                } else {
                    int err = gen_expression(arg, RARG1 + slot);
                    if(err) return err;
                }
            }
        }

        // Check if all args are simple (no push/pop needed)
        bool all_args_simple = true;
        if(!callee_is_varargs){
            foreach(arg; expr.args){
                CType* arg_type = arg.type;
                // Struct args are not simple
                if(arg_type.is_struct_or_union()){
                    all_args_simple = false;
                    break;
                }
                if(!is_simple_expr(arg)){
                    all_args_simple = false;
                    break;
                }
            }
        }

        // Non-varargs: evaluate arguments in order with push/pop preservation
        if(!callee_is_varargs)
        foreach(i, arg; expr.args){
            CType* arg_type = arg.type;
            int slot = get_arg_slot(i);
            int num_slots = arg_slots(arg.type);
            // Varargs beyond fixed args always go on stack
            bool is_vararg = callee_is_varargs && i >= n_fixed_args;
            bool is_stack_arg = (slot >= N_REG_ARGS) || is_vararg;
            bool spans_to_stack = !is_vararg && (slot < N_REG_ARGS && slot + num_slots > N_REG_ARGS);

            if(is_stack_arg){
                // Stack argument: evaluate to temp and push
                if(arg_type && arg_type.is_struct_or_union()){
                    if(fits_in_registers(arg_type)){
                        // Small struct on stack: push each word
                        int addr_reg = regallocator.allocate();
                        int err = gen_struct_address(arg, addr_reg);
                        if(err) return err;
                        size_t struct_size = arg_type.size_of();
                        sb.writef("    read r0 r%\n", addr_reg);
                        sb.write("    push r0\n");
                        if(struct_size > 8){
                            sb.writef("    add r% r% 8\n", addr_reg, addr_reg);
                            sb.writef("    read r0 r%\n", addr_reg);
                            sb.write("    push r0\n");
                        }
                        regallocator.reset_to(regallocator.alloced - 1);
                    } else {
                        // Large struct: push address
                        int err = gen_struct_address(arg, 0);
                        if(err) return err;
                        sb.write("    push r0\n");
                    }
                } else {
                    int err = gen_expression(arg, 0);
                    if(err) return err;
                    sb.write("    push r0\n");
                }
                // Stack args stay on stack (no pop)
            } else if(spans_to_stack){
                // Arg starts in registers but spills to stack (e.g., 2-word struct at slot 7)
                // First part goes to register, second part to stack
                if(arg_type && arg_type.is_struct_or_union() && fits_in_registers(arg_type)){
                    int addr_reg = regallocator.allocate();
                    int err = gen_struct_address(arg, addr_reg);
                    if(err) return err;
                    size_t struct_size = arg_type.size_of();
                    // First word to register
                    sb.writef("    read rarg% r%\n", 1 + slot, addr_reg);
                    // Second word to stack
                    if(struct_size > 8){
                        sb.writef("    add r% r% 8\n", addr_reg, addr_reg);
                        sb.writef("    read r0 r%\n", addr_reg);
                        sb.write("    push r0\n");
                    }
                    regallocator.reset_to(regallocator.alloced - 1);
                }
                // Push register part to preserve (skip if all args are simple)
                if(!all_args_simple && i != expr.args.length - 1){
                    sb.writef("    push rarg%\n", 1 + slot);
                }
            } else {
                // Register argument (slot < N_REG_ARGS)
                if(arg_type && arg_type.is_struct_or_union()){
                    if(fits_in_registers(arg_type)){
                        // Small struct: load data into register(s)
                        int b = regallocator.alloced;
                        int addr_reg = regallocator.allocate();
                        int err = gen_struct_address(arg, addr_reg);
                        if(err) return err;

                        size_t struct_size = arg_type.size_of();
                        sb.writef("    read rarg% r%\n", 1 + slot, addr_reg);
                        if(struct_size > 8){
                            sb.writef("    add r% r% 8\n", addr_reg, addr_reg);
                            sb.writef("    read rarg% r%\n", 2 + slot, addr_reg);
                        }
                        regallocator.reset_to(b);
                    } else {
                        // Large struct: pass address (callee copies)
                        int err = gen_struct_address(arg, RARG1 + slot);
                        if(err) return err;
                    }
                } else {
                    int err = gen_expression(arg, RARG1 + slot);
                    if(err) return err;
                }

                // Push to preserve across subsequent arg evaluation (skip if all args are simple)
                if(!all_args_simple && i != expr.args.length - 1){
                    for(int s = 0; s < num_slots; s++){
                        sb.writef("    push rarg%\n", 1 + slot + s);
                    }
                }
            }
        }

        // Pop register arguments back in reverse order (skip stack args)
        // For varargs calls, we handled args separately above (no push/pop preservation)
        // Skip if all args were simple (no push was done)
        if(!callee_is_varargs && !all_args_simple){
            for(int i = cast(int)expr.args.length - 2; i >= 0; i--){
                int slot = get_arg_slot(i);
                int num_slots = arg_slots(expr.args[i].type);
                // Only pop if this was a register arg (slot < N_REG_ARGS)
                if(slot < N_REG_ARGS){
                    int regs_to_pop = num_slots;
                    if(slot + num_slots > N_REG_ARGS){
                        regs_to_pop = N_REG_ARGS - slot;  // Only pop register portion
                    }
                    for(int s = regs_to_pop - 1; s >= 0; s--){
                        sb.writef("    pop rarg%\n", 1 + slot + s);
                    }
                }
            }
        }

        // For large struct returns, pass destination address in rarg1
        // We use rsp as the temp location (it points past the allocated frame)
        int struct_slots_needed = 0;
        if(callee_uses_hidden_ptr){
            struct_slots_needed = cast(int)callee_func_type.return_type.stack_slots();
            // Allocate temp space by advancing rsp
            sb.writef("    add rsp rsp %\n", P(struct_slots_needed));
            // Pass address of temp space as rarg1 (rsp - slots = start of temp area)
            sb.writef("    sub rarg1 rsp %\n", P(struct_slots_needed));
        }

        int total_args = total_slots;  // Accounts for multi-register struct params

        // Generate call
        // Check if callee is a direct function call (identifier with FUNCTION or EXTERN_FUNC ref_kind)
        CIdentifier* id = expr.callee.as_identifier();
        bool is_direct_call = id !is null &&
            (id.ref_kind == IdentifierRefKind.FUNCTION ||
             id.ref_kind == IdentifierRefKind.EXTERN_FUNC);

        if(is_direct_call){
            // Save registers before call:
            // 1. Reglocals in R0-R7 (from max_call_slots optimization)
            // 2. Scratch registers R8+ that were in use
            // For varargs calls, we already saved R8+ before pushing varargs
            if(!callee_is_varargs){
                // Save low reglocals first (they get clobbered by callee's temps)
                for(int i = 0; i < n_low_reglocals; i++){
                    sb.writef("    push r%\n", low_reglocals_to_save[i]);
                }
                // Save R8+ scratch regs
                for(int i = N_REG_ARGS; i < before; i++){
                    sb.writef("    push r%\n", i);
                }
            }

            // Direct call - use qualified name for extern functions
            // For varargs with float args, emit float mask as third argument
            if(id.ref_kind == IdentifierRefKind.EXTERN_FUNC){
                used_funcs[id.name.lexeme] = true;  // Mark as used for dlimport
                str* func_alias = id.name.lexeme in extern_funcs;
                if(func_alias is null){
                    if(auto func_ptr = id.name.lexeme in func_info)
                        goto actually_local;
                    error(expr.expr.token, "Extern function not found in alias table");
                    return -1;
                }
                if(callee_is_varargs && varargs_float_mask != 0){
                    sb.writef("    call function %.% % %\n", *func_alias, id.name.lexeme, total_args, H(varargs_float_mask));
                } else {
                    sb.writef("    call function %.% %\n", *func_alias, id.name.lexeme, total_args);
                }
            } else {
                actually_local:
                // Local function call - verify the function has a definition
                if(auto func_ptr = id.name.lexeme in func_info){
                    CFunction* func = *func_ptr;
                    if(!func.is_definition){
                        // Function declared but not defined, and no library specified
                        import dlib.stringbuilder : StringBuilder;
                        StringBuilder msg;
                        msg.allocator = allocator;
                        msg.write("call to undefined function '");
                        msg.write(id.name.lexeme);
                        msg.write("' - add #pragma library to specify which library it comes from, or provide a definition");
                        error(expr.expr.token, msg.borrow());
                        return -1;
                    }
                }
                called_funcs[id.name.lexeme] = true;  // Track internal call (for inline function generation)
                if(callee_is_varargs && varargs_float_mask != 0){
                    sb.writef("    call function % % %\n", id.name.lexeme, total_args, H(varargs_float_mask));
                } else {
                    sb.writef("    call function % %\n", id.name.lexeme, total_args);
                }
            }

            // Restore registers (for non-varargs; varargs restores after stack cleanup)
            if(!callee_is_varargs){
                // Restore R8+ scratch regs (reverse order)
                for(int i = before - 1; i >= N_REG_ARGS; i--){
                    sb.writef("    pop r%\n", i);
                }
                // Restore low reglocals (reverse order)
                for(int i = n_low_reglocals - 1; i >= 0; i--){
                    sb.writef("    pop r%\n", low_reglocals_to_save[i]);
                }
            }
        } else {
            // Indirect call through register
            int func_reg = regallocator.allocate();
            int err = gen_expression(expr.callee, func_reg);
            if(err) return err;

            if(!callee_is_varargs){
                for(int i = 0; i < n_low_reglocals; i++){
                    sb.writef("    push r%\n", low_reglocals_to_save[i]);
                }
                for(int i = N_REG_ARGS; i < before; i++){
                    sb.writef("    push r%\n", i);
                }
            }

            if(callee_is_varargs && varargs_float_mask != 0){
                sb.writef("    call r% % %\n", func_reg, total_args, H(varargs_float_mask));
            } else {
                sb.writef("    call r% %\n", func_reg, total_args);
            }

            if(!callee_is_varargs){
                for(int i = before - 1; i >= N_REG_ARGS; i--){
                    sb.writef("    pop r%\n", i);
                }
                for(int i = n_low_reglocals - 1; i >= 0; i--){
                    sb.writef("    pop r%\n", low_reglocals_to_save[i]);
                }
            }
        }

        // Caller cleanup: pop stack args
        if(n_stack_args > 0){
            sb.writef("    sub rsp rsp %\n", P(n_stack_args));
        }

        // For varargs calls, restore R8+ registers AFTER stack cleanup
        // (they were saved before varargs were pushed)
        // Also restore low reglocals
        if(callee_is_varargs){
            for(int i = before - 1; i >= N_REG_ARGS; i--){
                sb.writef("    pop r%\n", i);
            }
            for(int i = n_low_reglocals - 1; i >= 0; i--){
                sb.writef("    pop r%\n", low_reglocals_to_save[i]);
            }
        }

        if(target != TARGET_IS_NOTHING){
            if(callee_uses_hidden_ptr){
                // Large struct returns: temp space already allocated, address is at rsp - slots
                sb.writef("    sub r% rsp %\n", target, P(struct_slots_needed));
            } else if(callee_func_type.return_type.is_struct_or_union()){
                // Small/medium struct returns (fit in registers): allocate temp space and store value
                // This ensures we always return a pointer for struct types, consistent with large structs
                size_t struct_size = callee_func_type.return_type.size_of();
                int slots = cast(int)callee_func_type.return_type.stack_slots();

                // Allocate temp space
                sb.writef("    add rsp rsp %\n", P(slots));
                // Get address of temp space
                sb.writef("    sub r% rsp %\n", target, P(slots));

                // Store rout1 (first 8 bytes)
                sb.writef("    write r% rout1\n", target);

                // Store rout2 if needed (9-16 bytes)
                if(struct_size > 8){
                    sb.writef("    add r% r% 8\n", target, target);
                    sb.writef("    write r% rout2\n", target);
                    // Reset target to start of struct
                    sb.writef("    sub r% r% 8\n", target, target);
                }
            } else {
                sb.writef("    move r% rout1\n", target);
                // For sub-32-bit return types (_Bool, char, short), mask to proper size
                // Native functions may leave garbage in upper bits of rax
                // Note: 32-bit writes to eax automatically zero upper 32 bits in AMD64
                CType* ret_type = callee_func_type.return_type;
                if(ret_type && !ret_type.is_float()){
                    size_t ret_size = ret_type.size_of();
                    if(ret_size == 1){
                        // _Bool, char, unsigned char - mask to 8 bits
                        sb.writef("    and r% r% 0xff\n", target, target);
                    } else if(ret_size == 2){
                        // short, unsigned short - mask to 16 bits
                        sb.writef("    and r% r% 0xffff\n", target, target);
                    }
                    // 32-bit and 64-bit don't need masking
                }
            }
        }

        // Note: We don't free the temp struct return space here because:
        // 1. The caller may still need to read from it (e.g., for nested calls)
        // 2. It will be cleaned up when the function returns (rsp = rbp)

        regallocator.reset_to(before);
        return 0;
    }

    int gen_assign(CAssign* expr, int target){
        CExpr* lhs = expr.target.ungroup();

        // Simple variable assignment
        if(CIdentifier* id = lhs.as_identifier()){
            str name = id.name.lexeme;

            // Check for register variable
            if(int* r = name in reglocals){
                CType* var_type = id.expr.type;
                bool is_float_op = var_type && var_type.is_float();
                bool is_float32 = var_type && var_type.is_float32();

                if(expr.op == CTokenType.EQUAL){
                    // Simple assignment
                    int err = gen_expression(expr.value, *r);
                    if(err) return err;
                } else {
                    // Compound assignment (+=, -=, etc.)
                    int before = regallocator.alloced;
                    int rhs_reg = regallocator.allocate();

                    // For float32, convert LHS to double before operation
                    if(is_float32){
                        sb.writef("    ftod r% r%\n", *r, *r);
                    }

                    int err = gen_expression(expr.value, rhs_reg);
                    if(err) return err;

                    switch(expr.op) with (CTokenType){
                        case PLUS_EQUAL:
                            sb.writef("    % r% r% r%\n", is_float_op ? "dadd" : "add", *r, *r, rhs_reg);
                            break;
                        case MINUS_EQUAL:
                            sb.writef("    % r% r% r%\n", is_float_op ? "dsub" : "sub", *r, *r, rhs_reg);
                            break;
                        case STAR_EQUAL:
                            sb.writef("    % r% r% r%\n", is_float_op ? "dmul" : "mul", *r, *r, rhs_reg);
                            break;
                        case SLASH_EQUAL:
                            if(is_float_op)
                                sb.writef("    ddiv r% r% r%\n", *r, *r, rhs_reg);
                            else
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

                    // For float32, convert result back to float32
                    if(is_float32){
                        sb.writef("    dtof r% r%\n", *r, *r);
                    }

                    regallocator.reset_to(before);
                }

                if(target != TARGET_IS_NOTHING && target != *r){
                    sb.writef("    move r% r%\n", target, *r);
                }
                return 0;
            }

            // Check for stack variable
            if(int* offset = name in stacklocals){
                // Check if this is a struct/union assignment
                CType* var_type = id.expr.type;
                if(var_type && var_type.is_struct_or_union()){
                    // Struct/union assignment - use memcpy
                    if(expr.op != CTokenType.EQUAL){
                        error(expr.expr.token, "Compound assignment not supported for structs");
                        return 1;
                    }

                    int before = regallocator.alloced;
                    int dst_reg = regallocator.allocate();
                    int src_reg = regallocator.allocate();

                    // src = address of source struct
                    // IMPORTANT: Evaluate source FIRST, then destination.
                    // This is because evaluating the source (e.g., a function call) may
                    // clobber registers, so we calculate dst address AFTER source is ready.
                    CExpr* val = expr.value.ungroup();
                    if(CIdentifier* src_id = val.as_identifier()){
                        // Source is a variable
                        str src_name = src_id.name.lexeme;
                        if(int* src_offset = src_name in stacklocals){
                            sb.writef("    add r% rbp %\n", src_reg, P(*src_offset));
                        } else {
                            error(expr.expr.token, "Source struct must be a local variable");
                            return 1;
                        }
                    } else if(val.kind == CExprKind.CALL){
                        // Source is a function call that returns a struct
                        // Generate the call - it will return pointer to struct in src_reg
                        int err = gen_expression(val, src_reg);
                        if(err) return err;
                        // src_reg now contains pointer to the returned struct
                    } else if(val.kind == CExprKind.ASSIGN){
                        // Source is another assignment (chained: a = b = c)
                        // Generate the inner assignment, which returns address of its destination
                        int err = gen_expression(val, src_reg);
                        if(err) return err;
                        // src_reg now contains address of the inner assignment's destination
                    } else {
                        // Try gen_struct_address for other expressions (member access, etc.)
                        int err = gen_struct_address(val, src_reg);
                        if(err) return err;
                    }

                    // dst = address of target struct (calculated AFTER source to avoid clobbering)
                    sb.writef("    add r% rbp %\n", dst_reg, P(*offset));

                    // memcpy dst src size
                    size_t struct_size = var_type.size_of();
                    sb.writef("    memcpy r% r% %\n", dst_reg, src_reg, struct_size);

                    // Return address of destination for chained assignment
                    if(target != TARGET_IS_NOTHING){
                        sb.writef("    move r% r%\n", target, dst_reg);
                    }

                    regallocator.reset_to(before);
                    return 0;
                }

                // Regular (non-struct) assignment
                int before = regallocator.alloced;
                int val_reg = regallocator.allocate();

                if(expr.op == CTokenType.EQUAL){
                    // Simple assignment
                    int err = gen_expression(expr.value, val_reg);
                    if(err) return err;
                } else {
                    // Compound assignment - read current value first
                    int cur_reg = regallocator.allocate();
                    sb.writef("    local_read r% %\n", cur_reg, P(*offset));
                    // Convert float32 to double after reading
                    bool is_float_op = var_type && var_type.is_float();
                    if(var_type && var_type.is_float32()){
                        sb.writef("    ftod r% r%\n", cur_reg, cur_reg);
                    }

                    int err = gen_expression(expr.value, val_reg);
                    if(err) return err;

                    // For float types, convert RHS to double if needed
                    // Note: float32 values are already converted to double by gen_expression
                    if(is_float_op){
                        CType* rhs_type = expr.value.type;
                        if(rhs_type && rhs_type.is_integer()){
                            sb.writef("    itod r% r%\n", val_reg, val_reg);
                        }
                        // Don't add ftod for float32 - gen_expression already does this
                    }

                    switch(expr.op) with (CTokenType){
                        case PLUS_EQUAL:
                            sb.writef("    % r% r% r%\n", is_float_op ? "dadd" : "add", val_reg, cur_reg, val_reg);
                            break;
                        case MINUS_EQUAL:
                            sb.writef("    % r% r% r%\n", is_float_op ? "dsub" : "sub", val_reg, cur_reg, val_reg);
                            break;
                        case STAR_EQUAL:
                            sb.writef("    % r% r% r%\n", is_float_op ? "dmul" : "mul", val_reg, cur_reg, val_reg);
                            break;
                        case SLASH_EQUAL:
                            if(is_float_op)
                                sb.writef("    ddiv r% r% r%\n", val_reg, cur_reg, val_reg);
                            else
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

                // Convert double to float32 before storing
                // Note: if expr.value already has type float32 (e.g., from an implicit cast),
                // then gen_expression already converted it, so no need for another dtof
                CType* val_type = expr.value.type;
                bool val_is_float32 = val_type && val_type.is_float32();
                if(var_type && var_type.is_float32() && !val_is_float32){
                    sb.writef("    dtof r% r%\n", val_reg, val_reg);
                }
                sb.writef("    local_write % r%\n", P(*offset), val_reg);

                if(target != TARGET_IS_NOTHING){
                    sb.writef("    move r% r%\n", target, val_reg);
                }
                regallocator.reset_to(before);
                return 0;
            }

            // Check for global variable (including extern)
            if(id.ref_kind == IdentifierRefKind.GLOBAL_VAR ||
               id.ref_kind == IdentifierRefKind.EXTERN_VAR){
                CType* gtype = id.expr.type;
                if(gtype is null){
                    error(expr.expr.token, "Global variable has no type");
                    return 1;
                }

                // Track extern object usage
                if(id.ref_kind == IdentifierRefKind.EXTERN_VAR){
                    used_objs[name] = true;
                }

                // Check if this is a struct/union assignment
                if(gtype.is_struct_or_union()){
                    if(expr.op != CTokenType.EQUAL){
                        error(expr.expr.token, "Compound assignment not supported for structs");
                        return 1;
                    }

                    int before = regallocator.alloced;
                    int dst_reg = regallocator.allocate();
                    int src_reg = regallocator.allocate();

                    // Evaluate source FIRST (may clobber registers)
                    CExpr* val = expr.value.ungroup();
                    if(CIdentifier* src_id = val.as_identifier()){
                        str src_name = src_id.name.lexeme;
                        if(int* src_offset = src_name in stacklocals){
                            sb.writef("    add r% rbp %\n", src_reg, P(*src_offset));
                        } else if(src_id.ref_kind == IdentifierRefKind.GLOBAL_VAR){
                            sb.writef("    move r% var %\n", src_reg, src_name);
                        } else {
                            error(expr.expr.token, "Source struct must be a variable");
                            return 1;
                        }
                    } else if(val.kind == CExprKind.CALL){
                        int err = gen_expression(val, src_reg);
                        if(err) return err;
                    } else if(val.kind == CExprKind.ASSIGN){
                        int err = gen_expression(val, src_reg);
                        if(err) return err;
                    } else {
                        int err = gen_struct_address(val, src_reg);
                        if(err) return err;
                    }

                    // Get address of global destination
                    if(id.ref_kind == IdentifierRefKind.EXTERN_VAR){
                        str* obj_alias = name in extern_objs;
                        if(obj_alias is null){
                            error(expr.expr.token, "Extern variable not found");
                            return -1;
                        }
                        sb.writef("    move r% var %.%\n", dst_reg, *obj_alias, name);
                    } else {
                        sb.writef("    move r% var %\n", dst_reg, name);
                    }

                    // memcpy dst src size
                    size_t struct_size = gtype.size_of();
                    sb.writef("    memcpy r% r% %\n", dst_reg, src_reg, struct_size);

                    if(target != TARGET_IS_NOTHING){
                        sb.writef("    move r% r%\n", target, dst_reg);
                    }

                    regallocator.reset_to(before);
                    return 0;
                }

                int before = regallocator.alloced;
                int addr_reg = regallocator.allocate();
                int val_reg = regallocator.allocate();

                // Get address of global and its size
                if(id.ref_kind == IdentifierRefKind.EXTERN_VAR){
                    str* obj_alias = name in extern_objs;
                    if(obj_alias is null){
                        error(expr.expr.token, "Extern variable not found in alias table");
                        return -1;
                    }
                    sb.writef("    move r% var %.%\n", addr_reg, *obj_alias, name);
                } else {
                    sb.writef("    move r% var %\n", addr_reg, name);
                }
                size_t var_size = gtype.size_of();
                bool is_signed = gtype.is_signed;
                bool is_float_op = gtype.is_float();

                if(expr.op == CTokenType.EQUAL){
                    // Simple assignment
                    int err = gen_expression(expr.value, val_reg);
                    if(err) return err;
                } else {
                    // Compound assignment - read current value first
                    int cur_reg = regallocator.allocate();
                    sb.writef("    % r% r%\n", read_instr_for_size(var_size, is_signed), cur_reg, addr_reg);
                    // Convert float32 to double after reading
                    if(gtype.is_float32()){
                        sb.writef("    ftod r% r%\n", cur_reg, cur_reg);
                    }

                    int err = gen_expression(expr.value, val_reg);
                    if(err) return err;

                    // For float types, convert RHS to double if needed
                    // Note: float32 values are already converted to double by gen_expression
                    if(is_float_op){
                        CType* rhs_type = expr.value.type;
                        if(rhs_type && rhs_type.is_integer()){
                            sb.writef("    itod r% r%\n", val_reg, val_reg);
                        }
                        // Don't add ftod for float32 - gen_expression already does this
                    }

                    switch(expr.op) with (CTokenType){
                        case PLUS_EQUAL:
                            sb.writef("    % r% r% r%\n", is_float_op ? "dadd" : "add", val_reg, cur_reg, val_reg);
                            break;
                        case MINUS_EQUAL:
                            sb.writef("    % r% r% r%\n", is_float_op ? "dsub" : "sub", val_reg, cur_reg, val_reg);
                            break;
                        case STAR_EQUAL:
                            sb.writef("    % r% r% r%\n", is_float_op ? "dmul" : "mul", val_reg, cur_reg, val_reg);
                            break;
                        case SLASH_EQUAL:
                            if(is_float_op)
                                sb.writef("    ddiv r% r% r%\n", val_reg, cur_reg, val_reg);
                            else
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

                // Convert double to float32 before storing
                // Note: if expr.value already has type float32 (e.g., from an implicit cast),
                // then gen_expression already converted it, so no need for another dtof
                CType* val_type = expr.value.type;
                bool val_is_float32 = val_type && val_type.is_float32();
                if(gtype.is_float32() && !val_is_float32){
                    sb.writef("    dtof r% r%\n", val_reg, val_reg);
                }
                sb.writef("    % r% r%\n", write_instr_for_size(var_size), addr_reg, val_reg);

                if(target != TARGET_IS_NOTHING){
                    sb.writef("    move r% r%\n", target, val_reg);
                }
                regallocator.reset_to(before);
                return 0;
            }
            error(expr.expr.token, "Unknown variable");
            return -1;
        }

        // Pointer dereference assignment: *ptr = value
        if(CUnary* deref = lhs.as_unary()){
            if(deref.op == CTokenType.STAR){
                int before = regallocator.alloced;
                int ptr_reg = regallocator.allocate();
                int val_reg = regallocator.allocate();

                int err = gen_expression(deref.operand, ptr_reg);
                if(err) return err;

                err = gen_expression(expr.value, val_reg);
                if(err) return err;

                // Get pointed-to type's size for proper sized write
                CType* ptr_type = deref.operand.type;
                size_t elem_size = (ptr_type && ptr_type.is_pointer()) ? ptr_type.element_size() : 8;
                sb.writef("    % r% r%\n", write_instr_for_size(elem_size), ptr_reg, val_reg);

                if(target != TARGET_IS_NOTHING){
                    sb.writef("    move r% r%\n", target, val_reg);
                }

                regallocator.reset_to(before);
                return 0;
            }
        }

        // Subscript assignment: arr[i] = value (equivalent to *(arr + i) = value)
        if(CSubscript* sub = lhs.as_subscript()){
            int before = regallocator.alloced;
            int ptr_reg = regallocator.allocate();
            int idx_reg = regallocator.allocate();
            int val_reg = regallocator.allocate();

            int err = gen_expression(sub.array, ptr_reg);
            if(err) return err;

            err = gen_expression(sub.index, idx_reg);
            if(err) return err;

            // Scale index by element size
            CType* arr_type = sub.array.type;
            size_t elem_size = arr_type.element_size();
                // ((arr_type.is_pointer() || arr_type.is_array())) ? arr_type.element_size() : 1;
            if(elem_size > 1){
                sb.writef("    mul r% r% %\n", idx_reg, idx_reg, elem_size);
            }
            sb.writef("    add r% r% r%\n", ptr_reg, ptr_reg, idx_reg);

            err = gen_expression(expr.value, val_reg);
            if(err) return err;

            sb.writef("    % r% r%\n", write_instr_for_size(elem_size), ptr_reg, val_reg);

            if(target != TARGET_IS_NOTHING){
                sb.writef("    move r% r%\n", target, val_reg);
            }

            regallocator.reset_to(before);
            return 0;
        }

        // Member access assignment: p.x = value or p->x = value
        if(CMemberAccess* ma = lhs.as_member_access()){
            int before = regallocator.alloced;
            int addr_reg = regallocator.allocate();
            int val_reg = regallocator.allocate();

            // Get the object type
            CType* obj_type = ma.object.type;
            if(obj_type is null){
                error(expr.expr.token, "Cannot determine type of struct expression");
                return 1;
            }

            // For ->, obj_type is a pointer to struct
            CType* struct_type = obj_type;
            if(ma.is_arrow){
                if(!obj_type.is_pointer()){
                    error(expr.expr.token, "'->' requires pointer to struct");
                    return 1;
                }
                struct_type = obj_type.pointed_to;
            }

            if(struct_type is null || !struct_type.is_struct_or_union()){
                error(expr.expr.token, "Member access requires struct/union type");
                return 1;
            }

            // Find the field
            StructField* field = struct_type.get_field(ma.member.lexeme);
            if(field is null){
                error(expr.expr.token, "Unknown struct/union field");
                return 1;
            }

            // Get address of struct/object
            if(ma.is_arrow){
                int err = gen_expression(ma.object, addr_reg);
                if(err) return err;
            } else {
                // For ., we need the address of the struct
                int err = gen_struct_address(ma.object, addr_reg);
                if(err) return err;
            }

            // Add field offset
            if(field.offset > 0){
                sb.writef("    add r% r% %\n", addr_reg, addr_reg, field.offset);
            }

            // Write to field
            size_t field_size = field.type.size_of();
            if(field.type.is_struct_or_union() && needs_memcpy(field_size)){
                // Struct/union field that needs memcpy
                int err = gen_struct_address(expr.value, val_reg);
                if(err) return err;
                sb.writef("    memcpy r% r% %\n", addr_reg, val_reg, field_size);
            } else if(expr.op == CTokenType.EQUAL){
                // Simple assignment: just write the value
                int err = gen_expression(expr.value, val_reg);
                if(err) return err;
                // For float32 fields, convert to float32 before storing
                // Note: if expr.value already has type float32 (e.g., from an implicit cast),
                // then gen_expression already converted it, so no need for another dtof
                CType* val_type = expr.value.type;
                bool val_is_float32 = val_type && val_type.is_float32();
                if(field.type.is_float32() && !val_is_float32){
                    sb.writef("    dtof r% r%\n", val_reg, val_reg);
                }
                sb.writef("    % r% r%\n", write_instr_for_size(field_size), addr_reg, val_reg);
            } else {
                // Compound assignment (+=, -=, etc.)
                int cur_reg = regallocator.allocate();
                bool is_float_op = field.type.is_float();
                bool is_float32 = field.type.is_float32();

                // Read current value (use unsigned read for floats since we don't want sign extension)
                bool use_signed = field.type.is_signed;
                sb.writef("    % r% r%\n", read_instr_for_size(field_size, use_signed), cur_reg, addr_reg);
                // Convert float32 to double for operation
                if(is_float32){
                    sb.writef("    ftod r% r%\n", cur_reg, cur_reg);
                }

                // Generate RHS value
                int err = gen_expression(expr.value, val_reg);
                if(err) return err;

                // Convert RHS to double if needed for float operation
                // Note: float32 values are already converted to double by gen_expression
                if(is_float_op){
                    CType* rhs_type = expr.value.type;
                    if(rhs_type && rhs_type.is_integer()){
                        sb.writef("    itod r% r%\n", val_reg, val_reg);
                    }
                    // Don't add ftod for float32 - gen_expression already does this
                }

                // Perform the operation
                switch(expr.op) with (CTokenType){
                    case PLUS_EQUAL:
                        sb.writef("    % r% r% r%\n", is_float_op ? "dadd" : "add", val_reg, cur_reg, val_reg);
                        break;
                    case MINUS_EQUAL:
                        sb.writef("    % r% r% r%\n", is_float_op ? "dsub" : "sub", val_reg, cur_reg, val_reg);
                        break;
                    case STAR_EQUAL:
                        sb.writef("    % r% r% r%\n", is_float_op ? "dmul" : "mul", val_reg, cur_reg, val_reg);
                        break;
                    case SLASH_EQUAL:
                        if(is_float_op)
                            sb.writef("    ddiv r% r% r%\n", val_reg, cur_reg, val_reg);
                        else
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
                        error(expr.expr.token, "Unhandled compound assignment for member access");
                        return 1;
                }

                // Convert back to float32 if needed
                if(is_float32){
                    sb.writef("    dtof r% r%\n", val_reg, val_reg);
                }

                // Write result back
                sb.writef("    % r% r%\n", write_instr_for_size(field_size), addr_reg, val_reg);
            }

            if(target != TARGET_IS_NOTHING){
                sb.writef("    move r% r%\n", target, val_reg);
            }

            regallocator.reset_to(before);
            return 0;
        }

        error(expr.expr.token, "Invalid assignment target");
        return 1;
    }

    int gen_subscript(CSubscript* expr, int target){
        // array[index] is equivalent to *(array + index)
        int before = regallocator.alloced;
        int arr_reg = regallocator.allocate();
        int idx_reg = regallocator.allocate();

        int err = gen_expression(expr.array, arr_reg);
        if(err) return err;

        err = gen_expression(expr.index, idx_reg);
        if(err) return err;

        // Scale index by element size for proper pointer/array arithmetic
        CType* arr_type = expr.array.type;
        size_t elem_size = arr_type.element_size();
        CType* elem_type = arr_type.element_type();
        bool is_signed = elem_type.is_signed;
        if(elem_size > 1){
            sb.writef("    mul r% r% %\n", idx_reg, idx_reg, elem_size);
        }
        sb.writef("    add r% r% r%\n", arr_reg, arr_reg, idx_reg);

        if(target != TARGET_IS_NOTHING){
            // If element type is an array, don't dereference - arrays decay to pointers
            if(elem_type !is null && elem_type.is_array()){
                sb.writef("    move r% r%\n", target, arr_reg);
            } else {
                sb.writef("    % r% r%\n", read_instr_for_size(elem_size, is_signed), target, arr_reg);
            }
        }

        regallocator.reset_to(before);
        return 0;
    }

    // Generate address of array subscript (for &arr[i])
    int gen_subscript_address(CSubscript* expr, int target){
        int before = regallocator.alloced;
        int arr_reg = regallocator.allocate();
        int idx_reg = regallocator.allocate();

        int err = gen_expression(expr.array, arr_reg);
        if(err) return err;

        err = gen_expression(expr.index, idx_reg);
        if(err) return err;

        // Scale index by element size
        CType* arr_type = expr.array.type;
        size_t elem_size = (arr_type && (arr_type.is_pointer() || arr_type.is_array())) ? arr_type.element_size() : 1;
        if(elem_size > 1){
            sb.writef("    mul r% r% %\n", idx_reg, idx_reg, elem_size);
        }
        sb.writef("    add r% r% r%\n", target, arr_reg, idx_reg);

        regallocator.reset_to(before);
        return 0;
    }

    int gen_member_access(CMemberAccess* expr, int target){
        int before = regallocator.alloced;
        int obj_reg = regallocator.allocate();

        // Get the object type
        CType* obj_type = expr.object.type;
        if(obj_type is null){
            error(expr.expr.token, "Cannot determine type of struct expression");
            return 1;
        }

        // For ->, obj_type is a pointer to struct
        // For ., obj_type is the struct itself
        CType* struct_type = obj_type;
        if(expr.is_arrow){
            if(!obj_type.is_pointer()){
                error(expr.expr.token, "'->' requires pointer to struct");
                return 1;
            }
            struct_type = obj_type.pointed_to;
        }

        if(struct_type is null || !struct_type.is_struct_or_union()){
            error(expr.expr.token, "Member access requires struct/union type");
            return 1;
        }

        // Find the field
        StructField* field = struct_type.get_field(expr.member.lexeme);
        if(field is null){
            error(expr.expr.token, "Unknown struct/union field");
            return 1;
        }

        // Generate code to get address of object
        if(expr.is_arrow){
            // For ->, the expression gives us the pointer directly
            int err = gen_expression(expr.object, obj_reg);
            if(err) return err;
        } else {
            // For ., we need the address of the struct
            // Use gen_struct_address which handles nested member access recursively
            int err = gen_struct_address(expr.object, obj_reg);
            if(err) return err;
        }

        // Add field offset
        if(field.offset > 0){
            sb.writef("    add r% r% %\n", obj_reg, obj_reg, field.offset);
        }

        // For array and struct/union fields, return the address rather than reading
        // (they decay to pointers in most contexts)
        if(target != TARGET_IS_NOTHING){
            if(field.type.is_array() || field.type.is_struct_or_union()){
                // Return address of the array/struct field
                if(target != obj_reg){
                    sb.writef("    move r% r%\n", target, obj_reg);
                }
            } else {
                // Read scalar field value
                size_t field_size = field.type.size_of();
                bool is_signed = field.type.is_signed;
                // For float32 fields, use unsigned read (no sign extend)
                // Note: float32 values stay as float32 bits - callers handle conversion
                if(field.type.is_float32()){
                    sb.writef("    read4 r% r%\n", target, obj_reg);
                } else {
                    sb.writef("    % r% r%\n", read_instr_for_size(field_size, is_signed), target, obj_reg);
                }
            }
        }

        regallocator.reset_to(before);
        return 0;
    }

    int gen_sizeof(CSizeof* expr, int target){
        if(target == TARGET_IS_NOTHING) return 0;

        size_t size = expr.size;
        if(size == 0 && expr.sizeof_expr !is null){
            // sizeof expr - compute size from expression type
            CType* t = expr.sizeof_expr.type;
            size = t.size_of();
        }

        sb.writef("    move r% %\n", target, size);
        return 0;
    }

    int gen_alignof(CAlignof* expr, int target){
        if(target == TARGET_IS_NOTHING) return 0;

        size_t alignment = expr.alignment;
        if(alignment == 0 && expr.alignof_expr !is null){
            // _Alignof(expr) - compute alignment from expression type
            CType* t = expr.alignof_expr.type;
            alignment = t.align_of();
        }

        sb.writef("    move r% %\n", target, alignment);
        return 0;
    }

    int gen_countof(CCountof* expr, int target){
        if(target == TARGET_IS_NOTHING) return 0;

        size_t count = expr.count;
        if(count == 0 && expr.countof_expr !is null){
            // _Countof(expr) - compute count from expression type
            CType* t = expr.countof_expr.type;
            if(t.is_array()){
                count = t.array_size;
            }
            else {
                error(expr.expr.token, "_Countof requires an array expression");
                return 1;
            }
        }

        sb.writef("    move r% %\n", target, count);
        return 0;
    }

    int gen_generic(CGeneric* expr, int target){
        return gen_expression(expr.picked, target);
    }

    int gen_va_arg(CVaArg* expr, int target){
        if(target == TARGET_IS_NOTHING) return 0;

        // va_arg(ap, type): read value from va_list pointer and advance it
        // va_list is a pointer to the varargs on stack
        // We read the value and advance the pointer by the size of the type

        // Get va_list pointer into a register
        int va_reg = regallocator.allocate();
        int err = gen_expression(expr.va_list_expr, va_reg);
        if(err) return err;

        // Read value from va_list (pointer to pointer - need to dereference)
        sb.writef("    read r% r%\n", va_reg, va_reg);  // Load current va_list position
        sb.writef("    read r% r%\n", target, va_reg);  // Load the value

        // Advance va_list by size of type
        size_t type_size = expr.arg_type ? expr.arg_type.size_of() : 8;
        if(type_size < 8) type_size = 8;  // Arguments are at least word-sized

        // We need to write back the advanced pointer
        // But we've already dereferenced... this is tricky
        // For now, just return the value - proper va_arg handling would need
        // to track the va_list variable and update it

        regallocator.reset_to(regallocator.alloced - 1);
        return 0;
    }

    int gen_ternary(CTernary* expr, int target){
        // condition ? if_true : if_false
        int else_label = labelallocator.allocate();
        int after_label = labelallocator.allocate();

        // Generate condition
        int before = regallocator.alloced;
        int cond = (target == TARGET_IS_NOTHING) ? regallocator.allocate() : target;
        int err = gen_expression(expr.condition, cond);
        if(err) return err;

        sb.writef("    cmp r% 0\n", cond);
        sb.writef("    jump eq label L%\n", else_label);  // Jump to else if condition is false

        // Generate true branch
        err = gen_expression(expr.if_true, target);
        if(err) return err;

        sb.writef("    move rip label L%\n", after_label);  // Skip else branch

        // Generate false branch
        sb.writef("  label L%\n", else_label);
        err = gen_expression(expr.if_false, target);
        if(err) return err;

        sb.writef("  label L%\n", after_label);
        regallocator.reset_to(before);
        return 0;
    }

    // Generate code to get the address of a struct expression (for pass-by-value)
    int gen_struct_address(CExpr* e, int target){
        e = e.ungroup();

        // Identifier - get address of struct variable
        if(CIdentifier* id = e.as_identifier()){
            str name = id.name.lexeme;

            final switch(id.ref_kind){
                case IdentifierRefKind.LOCAL_VAR:
                    if(int* offset = name in stacklocals){
                        if(target != TARGET_IS_NOTHING)
                            sb.writef("    add r% rbp %\n", target, P(*offset));
                        return 0;
                    }
                    error(id.name, "Local variable not found on stack");
                    return 1;

                case IdentifierRefKind.GLOBAL_VAR:
                    if(target != TARGET_IS_NOTHING)
                        sb.writef("    move r% var %\n", target, name);
                    return 0;

                case IdentifierRefKind.EXTERN_VAR:
                    used_objs[name] = true;
                    if(target != TARGET_IS_NOTHING){
                        str* obj_alias = name in extern_objs;
                        if(obj_alias is null){
                            error(id.name, "Extern variable not found in alias table");
                            return 1;
                        }
                        sb.writef("    move r% var %.%\n", target, *obj_alias, name);
                    }
                    return 0;

                case IdentifierRefKind.FUNCTION:
                case IdentifierRefKind.EXTERN_FUNC:
                case IdentifierRefKind.ENUM_CONST:
                case IdentifierRefKind.BUILTIN:
                case IdentifierRefKind.UNKNOWN:
                    error(id.name, "Cannot take address of this identifier for struct pass-by-value");
                    return 1;
            }
        }

        // Member access - get address of struct member
        if(CMemberAccess* ma = e.as_member_access()){
            CType* obj_type = ma.object.type;
            if(obj_type is null){
                error(ma.expr.token, "Cannot determine type for member access");
                return 1;
            }

            CType* struct_type = obj_type;
            if(ma.is_arrow){
                if(!obj_type.is_pointer()){
                    error(ma.expr.token, "'->' requires pointer to struct");
                    return 1;
                }
                struct_type = obj_type.pointed_to;
            }

            StructField* field = struct_type.get_field(ma.member.lexeme);
            if(field is null){
                error(ma.expr.token, "Unknown struct field");
                return 1;
            }

            // Get base address
            if(ma.is_arrow){
                int err = gen_expression(ma.object, target);
                if(err) return err;
            }
            else {
                // Recursively get address of the object
                int err = gen_struct_address(ma.object, target);
                if(err) return err;
            }

            // Add field offset
            if(field.offset > 0){
                sb.writef("    add r% r% %\n", target, target, field.offset);
            }
            return 0;
        }

        // Function call returning struct/union - gen_expression returns the address
        if(e.kind == CExprKind.CALL){
            CType* call_type = e.type;
            if(call_type && call_type.is_struct_or_union()){
                return gen_expression(e, target);
            }
        }

        // Compound literal - gen_expression returns the address
        if(e.kind == CExprKind.COMPOUND_LITERAL){
            return gen_expression(e, target);
        }

        // Dereference - address of *ptr is just ptr
        if(CUnary* u = e.as_unary()){
            if(u.op == CTokenType.STAR && u.is_prefix){
                return gen_expression(u.operand, target);
            }
        }

        error(e.token, "Cannot pass this expression as struct/union by value");
        return 1;
    }
}
