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

alias IntegerArray = Barray!(uintptr_t, VAllocator*);
alias FunctionTable = Table!(str, FunctionInfo, VAllocator*);

struct Variable {
    str name;
    uintptr_t value;
}

enum FunctionType: ubyte {
    NULL = 0,
    INTERPRETED = 1,
    NATIVE = 2,
    // first arg is the interpreter
    // NATIVE_TAKES_INTERPRETER = 2,
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
        uintptr_t function() native_function_r;
        uintptr_t function(uintptr_t) native_function_ra;
        uintptr_t function(uintptr_t, uintptr_t) native_function_raa;
        uintptr_t function(uintptr_t, uintptr_t, uintptr_t) native_function_raaa;
        uintptr_t function(uintptr_t, uintptr_t, uintptr_t, uintptr_t) native_function_raaaa;
    }
    FunctionType type;
    ubyte n_args;
    ubyte n_ret;
    ubyte pad;
    // if D had bitfields I could use the padding byte.
    uint length;

    uintptr_t[] instructions(){
        return instructions_[0..length];
    }
}

static assert(Function.sizeof == 16);

struct FunctionInfo {
    str name;
    Function* func;
}

struct LinkedModule {
    str name;
    Box!(str, VAllocator*) source_text;
    Box!(uintptr_t[], VAllocator*) bytecode;
    Barray!(ZString, VAllocator*) strings;
    Barray!(IntegerArray, VAllocator*) arrays;
    FunctionTable functions;
    Box!(Function[], VAllocator*) function_store;
    // storage for the variables
    Box!(uintptr_t[], VAllocator*) variables;
    // table to look variables up by name
    Table!(str, uintptr_t*, VAllocator*) variable_table;
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
