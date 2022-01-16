/*
 * Copyright © 2021-2022, David Priver
 */
import dlib.allocator;
import dlib.barray;

import dvm_defs;
import dvm_instructions;
import dvm_args;
// This is stupid.
import dasm_token;

struct AbstractInstruction {
    const(char)* first_char;
    const(char)[] label;
    Instruction instruction;
    enum MAX_ARGS = 5;
    Argument[MAX_ARGS] args;
    int n_args;
}

struct AbstractFunction {
    const(char)* first_char;
    const(char)[] name;
    int n_args;
    Barray!(AbstractInstruction, VAllocator) instructions;
    bool finished;
}

struct AbstractVariable {
    const(char)[] name;
    Argument value;
    // It should be a source location, not a token.
    Token tok;
}

struct AbstractArray {
    uintptr_t id;
    Barray!(Argument, VAllocator) array;
}

struct UnlinkedModule{
    Barray!(AbstractFunction, VAllocator) functions;
    Barray!(AbstractVariable, VAllocator) variables;
    Barray!(AbstractArray,    VAllocator) arrays;
}

