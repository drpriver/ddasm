/*
 * Copyright © 2021-2023, David Priver
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
    EMBED          = 0b1000000000,
}

struct EmbedInfo {
    str path;       // file path (unescaped, borrowed)
    size_t offset;  // byte offset into file
    size_t length;  // number of bytes to embed
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
    str NAME;
    str name;
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
        str text; // this is borrow and unescaped
        uintptr_t immediate;
        RegisterNames reg;
        CmpMode cmp_mode;
        str function_name;
        str label_name;
        str token;
        str variable;
        str constant;
        EmbedInfo embed;
    }
}
