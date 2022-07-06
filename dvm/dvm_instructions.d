/*
 * Copyright © 2021-2022, David Priver
 */
module dvm.dvm_instructions;
import dlib.allocator: Mallocator;
import dlib.btable: BTable;
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
    CALL_R,
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

// instructs with the same asm name should have distinct argument
// types so they can be distinguished.
immutable InstructionInfo[Instruction.max+1] INSTRUCTION_INFOS = {
    // this is in an IIFE just for the with
    with(Instruction) with(ArgumentKind){
    // IMM is something that can be coerced to a numeric constant
    // at link time.
    enum IMM = IMMEDIATE | STRING | FUNCTION | LABEL | ARRAY | VARIABLE;
    immutable i = [IMM];
    immutable ii = [IMM, IMM];
    immutable r = [REGISTER];
    immutable rr = [REGISTER, REGISTER];
    immutable ri = [REGISTER, IMM];
    immutable ir = [IMM, REGISTER];
    immutable rrr = [REGISTER, REGISTER, REGISTER];
    immutable rri = [REGISTER, REGISTER, IMM];
    immutable rir = [REGISTER, IMM, REGISTER];
    immutable rii = [REGISTER, IMM, IMM];
    immutable rrrr = [REGISTER, REGISTER, REGISTER, REGISTER];
    immutable rrri = [REGISTER, REGISTER, REGISTER, IMM];
    immutable rriri = [REGISTER, REGISTER, IMM, REGISTER, IMM];
    alias I = InstructionInfo;
    immutable result = cast(immutable)[
        I(ABORT,            "ABORT",            "abort"),
        I(NOP,              "NOP",              "nop"),
        I(READ,             "READ",             "read",  rr),
        I(READ_I,           "READ_I",           "read",  ri),
        I(LOCAL_READ,       "LOCAL_READ",       "local_read", ri),
        I(LOCAL_WRITE,      "LOCAL_WRITE",      "local_write", ir),
        I(LOCAL_WRITE_I,    "LOCAL_WRITE_I",    "local_write", ii),
        I(WRITE_R,          "WRITE_R",          "write", rr),
        I(WRITE_I,          "WRITE_I",          "write", ri),
        I(MOVE_R,           "MOVE_R",           "move",  rr),
        I(MOVE_I,           "MOVE_I",           "move",  ri),
        I(CMOVE_R,          "CMOVE_R",          "cmov", [CMPMODE, REGISTER, REGISTER]),
        I(CMOVE_I,          "CMOVE_I",          "cmov", [CMPMODE, REGISTER, IMM]),
        I(HALT,             "HALT",             "halt"),
        I(XOR_I,            "XOR_I",            "xor", rri),
        I(XOR_R,            "XOR_R",            "xor", rrr),
        I(OR_I,             "OR_I",             "or", rri),
        I(OR_R,             "OR_R",             "or", rrr),
        I(AND_I,            "AND_I",            "and", rri),
        I(AND_R,            "AND_R",            "and", rrr),
        I(LOGICAL_AND_I,    "LOGICAL_AND_I",    "logical_and", rri),
        I(LOGICAL_AND_R,    "LOGICAL_AND_R",    "logical_and", rrr),
        I(LOGICAL_OR_I,     "LOGICAL_OR_I",     "logical_or", rri),
        I(LOGICAL_OR_R,     "LOGICAL_OR_R",     "logical_or", rrr),
        I(ADD_I,            "ADD_I",            "add", rri),
        I(ADD_R,            "ADD_R",            "add", rrr),
        I(SUB_I,            "SUB_I",            "sub", rri),
        I(SUB_R,            "SUB_R",            "sub", rrr),
        I(FADD_I,           "FADD_I",           "fadd", rri),
        I(FADD_R,           "FADD_R",           "fadd", rrr),
        I(MUL_I,            "MUL_I",            "mul", rri),
        I(MUL_R,            "MUL_R",            "mul", rrr),
        I(DIV_I,            "DIV_I",            "div", rrri),
        I(DIV_R,            "DIV_R",            "div", rrrr),
        I(PUSH_I,           "PUSH_I",           "push", i),
        I(PUSH_R,           "PUSH_R",           "push", r),
        I(POP,              "POP",              "pop", r),
        I(CALL_I,           "CALL_I",           "call", i),
        I(CALL_R,           "CALL_R",           "call", r),
        I(TAIL_CALL_I,      "TAIL_CALL_I",      "tail_call", i),
        I(TAIL_CALL_R,      "TAIL_CALL_R",      "tail_call", r),
        I(RET,              "RET",              "ret"),
        I(JUMP_ABS_I,       "JUMP_ABS_I",       "jump", [CMPMODE, LABEL]),
        I(JUMP_REL_I,       "JUMP_REL_I",       "jump", [CMPMODE, IMMEDIATE]),
        I(JUMP_R,           "JUMP_R",           "jump", [CMPMODE, REGISTER]),
        I(CMP_R,            "CMP_R",            "cmp", rr),
        I(CMP_I,            "CMP_I",            "cmp", ri),
        I(SCMP_R,           "SCMP_R",           "scmp", rr),
        I(SCMP_I,           "SCMP_I",           "scmp", ri),
        I(DUMP,             "DUMP",             "dump"),
        I(SHIFTL_I,         "SHIFTL_I",         "shl", rri),
        I(SHIFTL_R,         "SHIFTL_R",         "shl", rrr),
        I(SHIFTR_I,         "SHIFTR_I",         "shr", rri),
        I(SHIFTR_R,         "SHIFTR_R",         "shr", rrr),
        I(PAUSE,            "PAUSE",            "pause"),
        I(MSG,              "MSG",              "msg", [STRING]),
        I(DEBUG_OP,         "DEBUG_OP",         "debug"),
        I(MEMCPY_I,         "MEMCPY_I",         "memcpy", rri),
        I(MEMCPY_R,         "MEMCPY_R",         "memcpy", rrr),
        I(BACKTRACE,        "BACKTRACE",        "bt"),
        I(ITOF,             "ITOF",             "itof", rr),
        I(FTOI,             "FTOI",             "ftoi", rr),
        I(NOT,              "NOT",              "not", rr),
        I(NEG,              "NEG",              "neg", rr),
        I(BINNEG,           "BINNEG",           "binneg", rr),
        I(LEA,              "LEA",              "lea", rriri),
    ];
    foreach(index, res; result)
        assert(index == res.instruction, "mismatch for instruction ");
    return result;
    }
}();

BTable!(string, Barray!(InstructionInfo, Mallocator), Mallocator)
InstructionTable(){
    static __gshared BTable!(string, Barray!(InstructionInfo, Mallocator), Mallocator) result;
    static __gshared initialized = false;
    if(!initialized){
        initialized = true;
        foreach(ii; INSTRUCTION_INFOS){
            if(auto arr = ii.asm_name in result){
                arr.push(ii);
            }
            else {
                Barray!(InstructionInfo, Mallocator) arr;
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

