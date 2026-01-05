/*
 * Copyright © 2021-2023, David Priver
 */
module dvm.dvm_linked;
import dlib.allocator;
import dlib.box;
import dlib.zstring;
import dlib.table;
import dlib.barray;

import dvm.dvm_defs;

alias IntegerArray = Barray!(uintptr_t);
alias FunctionTable = Table!(str, FunctionInfo);

struct Variable {
    str name;
    uintptr_t value;
}

enum FunctionType: ubyte {
    NULL = 0,
    INTERPRETED = 1,
    NATIVE = 2,
    NATIVE_VARARGS = 3,
}
struct Function {
    union {
        uintptr_t* instructions_;
        void function() native_function_;
        void function(uintptr_t) native_function_a;
        void function(uintptr_t, uintptr_t) native_function_aa;
        void function(uintptr_t, uintptr_t, uintptr_t) native_function_aaa;
        void function(uintptr_t, uintptr_t, uintptr_t, uintptr_t) native_function_aaaa;
        void function(uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t) native_function_aaaaa;
        void function(uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t) native_function_aaaaaa;
        void function(uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t) native_function_aaaaaaa;
        void function(uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t) native_function_aaaaaaaa;
        uintptr_t function() native_function_r;
        uintptr_t function(uintptr_t) native_function_ra;
        uintptr_t function(uintptr_t, uintptr_t) native_function_raa;
        uintptr_t function(uintptr_t, uintptr_t, uintptr_t) native_function_raaa;
        uintptr_t function(uintptr_t, uintptr_t, uintptr_t, uintptr_t) native_function_raaaa;
        uintptr_t function(uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t) native_function_raaaaa;
        uintptr_t function(uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t) native_function_raaaaaa;
        uintptr_t function(uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t) native_function_raaaaaaa;
        uintptr_t function(uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t) native_function_raaaaaaaa;
    }
    FunctionType type;
    ubyte n_args;
    ubyte n_ret;
    // Return types (2 bits per slot): 0=int, 1=float32, 2=float64
    // bits 0-1: first return, bits 2-3: second return, etc.
    ubyte ret_types;
    union {
        // For INTERPRETED functions: instruction count
        uint length;
        // For NATIVE functions: argument type mask (2 bits per arg)
        // 00=int, 01=float32, 10=float64, 11=reserved
        // Supports up to 16 args with type info
        uint arg_types;
    }
    // For NATIVE functions: sizes of struct args that need stack copying
    // null if no struct args, otherwise array of n_args sizes (0 = not a struct)
    ushort* struct_arg_sizes_;

    uintptr_t[] instructions(){
        return instructions_[0..length];
    }

    // Get struct arg sizes slice (may be null)
    ushort[] struct_arg_sizes(){
        if(struct_arg_sizes_ is null) return null;
        return struct_arg_sizes_[0..n_args];
    }
}

struct FunctionInfo {
    str name;
    Function* func;
}

struct LinkedModule {
    str name;
    Box!(str) source_text;
    Box!(uintptr_t[]) bytecode;
    Barray!(ZString) strings;
    Barray!(IntegerArray) arrays;
    FunctionTable functions;
    Box!(Function[]) function_store;
    // storage for the variables
    Box!(uintptr_t[]) variables;
    // table to look variables up by name
    Table!(str, uintptr_t*) variable_table;
    Function* start;

    FunctionInfo
    addr_to_function(uintptr_t* ip){
        foreach(fi; functions.values){
            auto func = fi.func;
            if(func.type != FunctionType.INTERPRETED)
                continue;
            uintptr_t* begin = func.instructions_;
            uintptr_t* end = func.instructions_ + func.length;
            if(ip >= begin && ip < end)
                return fi;
        }
        return FunctionInfo();
    }
    bool
    addr_contained(uintptr_t* ip){
        return ip >= bytecode.data.ptr && ip < (bytecode.data.ptr+bytecode.data.length);
    }

}
