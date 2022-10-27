/*
 * Copyright © 2021-2022, David Priver
 */
module dvm.dvm_unlinked;
import dlib.allocator;
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
    Barray!(AbstractInstruction, VAllocator*) instructions;
    bool finished;
}

struct AbstractVariable {
    str name;
    Argument value;
    const(char)* first_char;
}

struct AbstractArray {
    uintptr_t id;
    Barray!(Argument, VAllocator*) array;
}

struct UnlinkedModule{
    str name;
    Barray!(AbstractFunction, VAllocator*) functions;
    Barray!(AbstractVariable, VAllocator*) variables;
    Barray!(AbstractArray,    VAllocator*) arrays;
    Barray!(str,    VAllocator*) imports;
}

