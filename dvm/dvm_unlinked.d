/*
 * Copyright © 2021-2023, David Priver
 */
module dvm.dvm_unlinked;
import dlib.barray;

import dvm.dvm_defs;
import dvm.dvm_instructions;
import dvm.dvm_args;

struct AbstractInstruction {
    const(char)* first_char;
    str label;
    Instruction instruction;
    enum MAX_ARGS = 5;
    Argument[MAX_ARGS] args;
    int n_args;
}

struct AbstractFunction {
    const(char)* first_char;
    str name;
    int n_args;
    Barray!(AbstractInstruction) instructions;
    bool finished;
}

struct AbstractVariable {
    str name;
    size_t size;                    // number of words
    Barray!(Argument) initializers; // initializer values
    const(char)* first_char;
}

struct AbstractArray {
    uintptr_t id;
    Barray!(Argument) array;
}

struct DlimportFuncSpec {
    str name;
    ubyte n_args;  // For varargs: count of fixed (named) args
    ubyte n_ret;
    bool is_varargs;
    // Argument type mask (2 bits per arg): 00=int, 01=float32, 10=float64, 11=reserved
    uint arg_types;
    // Return type mask (2 bits per ret): 00=int, 01=float32, 10=float64
    uint ret_types;
    // Sizes of struct args that need stack copying (0 = not a struct)
    // Only set for System V x86_64 ABI where large structs are passed by value on stack
    Barray!(ushort) struct_arg_sizes;
}

struct DlimportObjSpec {
    str name;  // Symbol name
}

struct DlimportDecl {
    Barray!str library_paths;  // Multiple paths to try in order
    str alias_name;
    Barray!DlimportFuncSpec funcs;
    Barray!DlimportObjSpec objs;  // External object (variable) imports
}

struct UnlinkedModule{
    str name;
    Barray!(AbstractFunction) functions;
    Barray!(AbstractVariable) variables;
    Barray!(AbstractArray) arrays;
    Barray!(str) imports;
    Barray!(DlimportDecl) dlimports;
}

