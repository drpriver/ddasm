/*
 * Copyright © 2021-2023, David Priver
 */
module dvm.dvm_instructions;
import dlib.allocator: Mallocator, MALLOCATOR;
import dlib.table: Table;
import dlib.barray: Barray;
import dvm.dvm_defs;
import dvm.dvm_args: ArgumentKind;

// DVM requires 64-bit registers to hold double-precision floats
static assert(uintptr_t.sizeof >= 8, "DVM requires a 64-bit platform (registers must hold doubles)");

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
    MUL_I,
    MUL_R,
    DIV_I,
    DIV_R,
    CMP_R,
    CMP_I,
    SCMP_R,
    SCMP_I,
    DADD_I,
    DADD_R,
    DSUB_I,
    DSUB_R,
    DMUL_I,
    DMUL_R,
    DDIV_I,
    DDIV_R,
    DCMP_I,
    DCMP_R,
    DNEG,
    FADD_I,
    FADD_R,
    FSUB_I,
    FSUB_R,
    FMUL_I,
    FMUL_R,
    FDIV_I,
    FDIV_R,
    FCMP_I,
    FCMP_R,
    FNEG,
    PUSH_I,
    PUSH_R,
    POP,
    CALL_I,
    CALL_I_NARGS,
    CALL_I_NARGS_FLOATS,  // For varargs with float arguments
    CALL_R,
    CALL_R_NARGS,
    CALL_R_NARGS_FLOATS,  // For indirect varargs with float arguments
    TAIL_CALL_I,
    TAIL_CALL_R,
    RET,
    JUMP_ABS_I,
    JUMP_REL_I,
    JUMP_R,
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
    MEMZERO_I,
    MEMZERO_R,
    BACKTRACE,
    ITOD,  // integer to double
    ITOF,  // integer to float
    DTOI,  // double to integer
    DTOF,  // double to float
    FTOD,  // float to double
    FTOI,  // float to integer
    NOT,
    NEG,
    BINNEG,
    LEA,
    READ1,
    READ2,
    READ4,
    WRITE1,
    WRITE2,
    WRITE4,
    SREAD1,
    SREAD2,
    SREAD4,
    BREAK,
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
    InstructionInfo(Instruction.MUL_I,            "MUL_I",            "mul", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.MUL_R,            "MUL_R",            "mul", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.DIV_I,            "DIV_I",            "div", [AK.REGISTER, AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.DIV_R,            "DIV_R",            "div", [AK.REGISTER, AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.CMP_R,            "CMP_R",            "cmp", [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.CMP_I,            "CMP_I",            "cmp", [AK.REGISTER, IMM]),
    InstructionInfo(Instruction.SCMP_R,           "SCMP_R",           "scmp", [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.SCMP_I,           "SCMP_I",           "scmp", [AK.REGISTER, IMM]),
    InstructionInfo(Instruction.DADD_I,           "DADD_I",           "dadd", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.DADD_R,           "DADD_R",           "dadd", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.DSUB_I,           "DSUB_I",           "dsub", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.DSUB_R,           "DSUB_R",           "dsub", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.DMUL_I,           "DMUL_I",           "dmul", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.DMUL_R,           "DMUL_R",           "dmul", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.DDIV_I,           "DDIV_I",           "ddiv", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.DDIV_R,           "DDIV_R",           "ddiv", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.DCMP_I,           "DCMP_I",           "dcmp", [AK.REGISTER, IMM]),
    InstructionInfo(Instruction.DCMP_R,           "DCMP_R",           "dcmp", [AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.DNEG,             "DNEG",             "dneg", [AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.FADD_I,           "FADD_I",           "fadd", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.FADD_R,           "FADD_R",           "fadd", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.FSUB_I,           "FSUB_I",           "fsub", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.FSUB_R,           "FSUB_R",           "fsub", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.FMUL_I,           "FMUL_I",           "fmul", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.FMUL_R,           "FMUL_R",           "fmul", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.FDIV_I,           "FDIV_I",           "fdiv", [AK.REGISTER, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.FDIV_R,           "FDIV_R",           "fdiv", [AK.REGISTER, AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.FCMP_I,           "FCMP_I",           "fcmp", [AK.REGISTER, IMM]),
    InstructionInfo(Instruction.FCMP_R,           "FCMP_R",           "fcmp", [AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.FNEG,             "FNEG",             "fneg", [AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.PUSH_I,           "PUSH_I",           "push", [IMM]),
    InstructionInfo(Instruction.PUSH_R,           "PUSH_R",           "push", [AK.REGISTER]),
    InstructionInfo(Instruction.POP,              "POP",              "pop", [AK.REGISTER]),
    InstructionInfo(Instruction.CALL_I,           "CALL_I",           "call", [IMM]),
    InstructionInfo(Instruction.CALL_I_NARGS,     "CALL_I_NARGS",     "call", [IMM, IMM]),
    InstructionInfo(Instruction.CALL_I_NARGS_FLOATS, "CALL_I_NARGS_FLOATS", "call", [IMM, IMM, IMM]),
    InstructionInfo(Instruction.CALL_R,           "CALL_R",           "call", [AK.REGISTER]),
    InstructionInfo(Instruction.CALL_R_NARGS,     "CALL_R_NARGS",     "call", [AK.REGISTER, IMM]),
    InstructionInfo(Instruction.CALL_R_NARGS_FLOATS, "CALL_R_NARGS_FLOATS", "call", [AK.REGISTER, IMM, IMM]),
    InstructionInfo(Instruction.TAIL_CALL_I,      "TAIL_CALL_I",      "tail_call", [IMM]),
    InstructionInfo(Instruction.TAIL_CALL_R,      "TAIL_CALL_R",      "tail_call", [AK.REGISTER]),
    InstructionInfo(Instruction.RET,              "RET",              "ret"),
    InstructionInfo(Instruction.JUMP_ABS_I,       "JUMP_ABS_I",       "jump", [AK.CMPMODE, AK.LABEL]),
    InstructionInfo(Instruction.JUMP_REL_I,       "JUMP_REL_I",       "jump", [AK.CMPMODE, AK.IMMEDIATE]),
    InstructionInfo(Instruction.JUMP_R,           "JUMP_R",           "jump", [AK.CMPMODE, AK.REGISTER]),
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
    InstructionInfo(Instruction.MEMZERO_I,        "MEMZERO_I",        "memzero", [AK.REGISTER, IMM]),
    InstructionInfo(Instruction.MEMZERO_R,        "MEMZERO_R",        "memzero", [AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.BACKTRACE,        "BACKTRACE",        "bt"),
    InstructionInfo(Instruction.ITOD,             "ITOD",             "itod", [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.ITOF,             "ITOF",             "itof", [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.DTOI,             "DTOI",             "dtoi", [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.DTOF,             "DTOF",             "dtof", [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.FTOD,             "FTOD",             "ftod", [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.FTOI,             "FTOI",             "ftoi", [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.NOT,              "NOT",              "not", [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.NEG,              "NEG",              "neg", [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.BINNEG,           "BINNEG",           "binneg", [AK.REGISTER,AK.REGISTER]),
    InstructionInfo(Instruction.LEA,              "LEA",              "lea", [AK.REGISTER, AK.REGISTER, IMM, AK.REGISTER, IMM]),
    InstructionInfo(Instruction.READ1,            "READ1",            "read1", [AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.READ2,            "READ2",            "read2", [AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.READ4,            "READ4",            "read4", [AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.WRITE1,           "WRITE1",           "write1", [AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.WRITE2,           "WRITE2",           "write2", [AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.WRITE4,           "WRITE4",           "write4", [AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.SREAD1,           "SREAD1",           "sread1", [AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.SREAD2,           "SREAD2",           "sread2", [AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.SREAD4,           "SREAD4",           "sread4", [AK.REGISTER, AK.REGISTER]),
    InstructionInfo(Instruction.BREAK,            "BREAK",            "break"),
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
    immutable(InstructionInfo)* info = &INSTRUCTION_INFOS[instruction];
    return info.args.length+1;
}

