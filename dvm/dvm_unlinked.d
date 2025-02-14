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
    Argument value;
    const(char)* first_char;
}

struct AbstractArray {
    uintptr_t id;
    Barray!(Argument) array;
}

struct UnlinkedModule{
    str name;
    Barray!(AbstractFunction) functions;
    Barray!(AbstractVariable) variables;
    Barray!(AbstractArray) arrays;
    Barray!(str) imports;
}

