/*
 * Copyright © 2021-2022, David Priver
 */
module dvm.dvm_args;
import dvm.dvm_defs;
import dvm.dvm_regs : RegisterNames;

// bitflags
enum ArgumentKind: uint {
    UNSET          = 0,
    STRING         = 0b1,
    IMMEDIATE      = 0b10,
    REGISTER       = 0b100,
    CMPMODE        = 0b1000,
    FUNCTION       = 0b10000,
    LABEL          = 0b100000,
    ARRAY          = 0b1000000,
    VARIABLE       = 0b10000000,
    CONSTANT       = 0b100000000,
}

enum CmpMode: uintptr_t {
    EQ = 0,
    NE = 1,
    LT = 2,
    GT = 3,
    GE = 4,
    LE = 5,
    TRUE = 6,
    FALSE = 7,
}

struct CmpModeInfo {
    CmpMode mode;
    const(char)[] NAME;
    const(char)[] name;
}

immutable CmpModeInfo[8] CmpModes = [
    CmpModeInfo(CmpMode.EQ, "EQ", "eq"),
    CmpModeInfo(CmpMode.NE, "NE", "ne"),
    CmpModeInfo(CmpMode.LT, "LT", "lt"),
    CmpModeInfo(CmpMode.GT, "GT", "gt"),
    CmpModeInfo(CmpMode.GE, "GE", "ge"),
    CmpModeInfo(CmpMode.LE, "LE", "le"),
    CmpModeInfo(CmpMode.TRUE, "TRUE", "true"),
    CmpModeInfo(CmpMode.FALSE, "FALSE", "false"),
];


struct Argument {
    ArgumentKind kind;
    const(char)* first_char;
    union {
        uintptr_t array;
        const(char)[] text; // this is borrow and unescaped
        uintptr_t immediate;
        RegisterNames reg;
        CmpMode cmp_mode;
        const(char)[] function_name;
        const(char)[] label_name;
        const(char)[] token;
        const(char)[] variable;
        const(char)[] constant;
    }
}
