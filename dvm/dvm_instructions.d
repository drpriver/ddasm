/*
 * Copyright © 2021-2023, David Priver
 */
module dvm.dvm_instructions;
import dlib.allocator: Mallocator, MALLOCATOR;
import dlib.table: Table;
import dlib.barray: Barray;
import dvm.dvm_defs;
import dvm.dvm_args: ArgumentKind;

enum Instruction: uintptr_t {
    ABORT,
    NOP,
    READ,
    READ_I,
    LOCAL_READ,
    LOCAL_WRITE,
    LOCAL_WRITE_I,
    WRITE_R,
    WRITE_I,
    MOVE_R,
    MOVE_I,
    CMOVE_R,
    CMOVE_I,
    HALT,
    XOR_I,
    XOR_R,
    OR_I,
    OR_R,
    AND_I,
    AND_R,
    LOGICAL_AND_I,
    LOGICAL_AND_R,
    LOGICAL_OR_I,
    LOGICAL_OR_R,
    ADD_I,
    ADD_R,
    SUB_I,
    SUB_R,
    FADD_I,
    FADD_R,
    MUL_I,
    MUL_R,
    DIV_I,
    DIV_R,
    PUSH_I,
    PUSH_R,
    POP,
    CALL_I,
    CALL_I_NARGS,
    CALL_R,
    CALL_R_NARGS,
    TAIL_CALL_I,
    TAIL_CALL_R,
    RET,
    JUMP_ABS_I,
    JUMP_REL_I,
    JUMP_R,
    CMP_R,
    CMP_I,
    SCMP_R,
    SCMP_I,
    DUMP,
    SHIFTL_I,
    SHIFTL_R,
    SHIFTR_I,
    SHIFTR_R,
    PAUSE,
    MSG,
    DEBUG_OP,
    MEMCPY_I,
    MEMCPY_R,
    BACKTRACE,
    ITOF,
    FTOI,
    NOT,
    NEG,
    BINNEG,
    LEA,
}

struct InstructionInfo {
    Instruction instruction;
    str NAME;
    str asm_name;
    const(ArgumentKind)[] args;
}

private alias AK = ArgumentKind;
private enum IMM = AK.IMMEDIATE | AK.STRING | AK.FUNCTION | AK.LABEL | AK.ARRAY | AK.VARIABLE;
// instructs with the same asm name should have distinct argument
// types so they can be distinguished.
immutable InstructionInfo[Instruction.max+1] INSTRUCTION_INFOS = [
    // IMM is something that can be coerced to a numeric constant
    // at link time.
    InstructionInfo(Instruction.ABORT,            "ABORT",            "abort"),
    InstructionInfo(Instruction.NOP,              "NOP",              "nop"),
    InstructionInfo(Instruction.READ,             "READ",             "read",  [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.READ_I,           "READ_I",           "read",  [AK.REGISTER, IMM]),
    InstructionInfo(Instruction.LOCAL_READ,       "LOCAL_READ",       "local_read", [AK.REGISTER, IMM]),
    InstructionInfo(Instruction.LOCAL_WRITE,      "LOCAL_WRITE",      "local_write", [IMM, AK.REGISTER]),
    InstructionInfo(Instruction.LOCAL_WRITE_I,    "LOCAL_WRITE_I",    "local_write", [IMM, IMM]),
    InstructionInfo(Instruction.WRITE_R,          "WRITE_R",          "write", [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.WRITE_I,          "WRITE_I",          "write", [AK.REGISTER, IMM]),
    InstructionInfo(Instruction.MOVE_R,           "MOVE_R",           "move",  [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.MOVE_I,           "MOVE_I",           "move",  [AK.REGISTER, IMM]),
    InstructionInfo(Instruction.CMOVE_R,          "CMOVE_R",          "cmov", [AK.CMPMODE, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.CMOVE_I,          "CMOVE_I",          "cmov", [AK.CMPMODE, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.HALT,             "HALT",             "halt"),
    InstructionInfo(Instruction.XOR_I,            "XOR_I",            "xor", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.XOR_R,            "XOR_R",            "xor", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.OR_I,             "OR_I",             "or", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.OR_R,             "OR_R",             "or", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.AND_I,            "AND_I",            "and", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.AND_R,            "AND_R",            "and", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.LOGICAL_AND_I,    "LOGICAL_AND_I",    "logical_and", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.LOGICAL_AND_R,    "LOGICAL_AND_R",    "logical_and", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.LOGICAL_OR_I,     "LOGICAL_OR_I",     "logical_or", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.LOGICAL_OR_R,     "LOGICAL_OR_R",     "logical_or", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.ADD_I,            "ADD_I",            "add", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.ADD_R,            "ADD_R",            "add", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.SUB_I,            "SUB_I",            "sub", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.SUB_R,            "SUB_R",            "sub", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.FADD_I,           "FADD_I",           "fadd", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.FADD_R,           "FADD_R",           "fadd", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.MUL_I,            "MUL_I",            "mul", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.MUL_R,            "MUL_R",            "mul", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.DIV_I,            "DIV_I",            "div", [AK.REGISTER, AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.DIV_R,            "DIV_R",            "div", [AK.REGISTER, AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.PUSH_I,           "PUSH_I",           "push", [IMM]),
    InstructionInfo(Instruction.PUSH_R,           "PUSH_R",           "push", [AK.REGISTER]),
    InstructionInfo(Instruction.POP,              "POP",              "pop", [AK.REGISTER]),
    InstructionInfo(Instruction.CALL_I,           "CALL_I",           "call", [IMM]),
    InstructionInfo(Instruction.CALL_I_NARGS,     "CALL_I_NARGS",     "call", [IMM, IMM]),
    InstructionInfo(Instruction.CALL_R,           "CALL_R",           "call", [AK.REGISTER]),
    InstructionInfo(Instruction.CALL_R_NARGS,     "CALL_R_NARGS",     "call", [AK.REGISTER, IMM]),
    InstructionInfo(Instruction.TAIL_CALL_I,      "TAIL_CALL_I",      "tail_call", [IMM]),
    InstructionInfo(Instruction.TAIL_CALL_R,      "TAIL_CALL_R",      "tail_call", [AK.REGISTER]),
    InstructionInfo(Instruction.RET,              "RET",              "ret"),
    InstructionInfo(Instruction.JUMP_ABS_I,       "JUMP_ABS_I",       "jump", [AK.CMPMODE, AK.LABEL]),
    InstructionInfo(Instruction.JUMP_REL_I,       "JUMP_REL_I",       "jump", [AK.CMPMODE, AK.IMMEDIATE]),
    InstructionInfo(Instruction.JUMP_R,           "JUMP_R",           "jump", [AK.CMPMODE, AK.REGISTER]),
    InstructionInfo(Instruction.CMP_R,            "CMP_R",            "cmp", [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.CMP_I,            "CMP_I",            "cmp", [AK.REGISTER, IMM]),
    InstructionInfo(Instruction.SCMP_R,           "SCMP_R",           "scmp", [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.SCMP_I,           "SCMP_I",           "scmp", [AK.REGISTER, IMM]),
    InstructionInfo(Instruction.DUMP,             "DUMP",             "dump"),
    InstructionInfo(Instruction.SHIFTL_I,         "SHIFTL_I",         "shl", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.SHIFTL_R,         "SHIFTL_R",         "shl", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.SHIFTR_I,         "SHIFTR_I",         "shr", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.SHIFTR_R,         "SHIFTR_R",         "shr", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.PAUSE,            "PAUSE",            "pause"),
    InstructionInfo(Instruction.MSG,              "MSG",              "msg", [AK.STRING]),
    InstructionInfo(Instruction.DEBUG_OP,         "DEBUG_OP",         "debug"),
    InstructionInfo(Instruction.MEMCPY_I,         "MEMCPY_I",         "memcpy", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.MEMCPY_R,         "MEMCPY_R",         "memcpy", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.BACKTRACE,        "BACKTRACE",        "bt"),
    InstructionInfo(Instruction.ITOF,             "ITOF",             "itof", [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.FTOI,             "FTOI",             "ftoi", [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.NOT,              "NOT",              "not", [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.NEG,              "NEG",              "neg", [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.BINNEG,           "BINNEG",           "binneg", [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.LEA,              "LEA",              "lea", [AK.REGISTER, AK.REGISTER, IMM, AK.REGISTER, IMM]),
];

Table!(string, Barray!(InstructionInfo))
InstructionTable(){
    static __gshared Table!(string, Barray!(InstructionInfo)) result;
    static __gshared initialized = false;
    if(!initialized){
        result.data.allocator = MALLOCATOR;
        initialized = true;
        foreach(ii; INSTRUCTION_INFOS){
            if(Barray!(InstructionInfo)* arr = ii.asm_name in result){
                arr.push(ii);
            }
            else {
                Barray!(InstructionInfo) arr;
                arr.bdata.allocator = MALLOCATOR;
                arr.push(ii);
                result[ii.asm_name] = arr;
            }
        }
    }
    return result;
}

size_t
instruction_size(Instruction instruction){
    if(instruction > Instruction.max){
        // TODO: log?
        return 1;
    }
    auto info = &INSTRUCTION_INFOS[instruction];
    return info.args.length+1;
}

