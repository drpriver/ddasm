/*
 * Copyright © 2021-2023, David Priver
 */
module dvm.dvm_regs;
import dvm.dvm_defs;

// Configurable number of register arguments
enum N_REG_ARGS = 8;

enum RegisterNames:uintptr_t {
    R0      = 0,  R1  =  1, R2  = 2,  R3 = 3, R4 = 4, R5 = 5, R6 = 6, R7 = 7, R8 = 8, R9 = 9,
    R10     = 10, R11 = 11, R12 = 12, R13 = 13, R14 = 14,
    R15     = 15, R16 = 16, // Return
    // Argument registers: RARG1-RARG8 map to R0-R7
    RARG1   = R0,
    RARG2   = R1,
    RARG3   = R2,
    RARG4   = R3,
    RARG5   = R4,
    RARG6   = R5,
    RARG7   = R6,
    RARG8   = R7,
    RARGMAX = R8,  // One past last arg register
    ROUT1   = R15,
    ROUT2   = R16,
    RJUNK = 17,
    RSP = 18,
    RBP = 19,
    RIP = 20,
    RFLAGS = 21,
    RERROR = 22,
}

// Get the register for a given argument slot (0-indexed)
RegisterNames arg_reg(size_t slot) {
    assert(slot < N_REG_ARGS);
    return cast(RegisterNames)(RegisterNames.R0 + slot);
}

struct RegisterInfo {
    RegisterNames register;
    string NAME;
    string name;
}

immutable RegisterInfo[33] registerinfos = [
    RegisterInfo(RegisterNames.R0,      "R0",       "r0"),
    RegisterInfo(RegisterNames.R1,      "R1",       "r1"),
    RegisterInfo(RegisterNames.R2,      "R2",       "r2"),
    RegisterInfo(RegisterNames.R3,      "R3",       "r3"),
    RegisterInfo(RegisterNames.R4,      "R4",       "r4"),
    RegisterInfo(RegisterNames.R5,      "R5",       "r5"),
    RegisterInfo(RegisterNames.R6,      "R6",       "r6"),
    RegisterInfo(RegisterNames.R7,      "R7",       "r7"),
    RegisterInfo(RegisterNames.R8,      "R8",       "r8"),
    RegisterInfo(RegisterNames.R9,      "R9",       "r9"),
    RegisterInfo(RegisterNames.RARG1,   "RARG1",    "rarg1"),
    RegisterInfo(RegisterNames.RARG2,   "RARG2",    "rarg2"),
    RegisterInfo(RegisterNames.RARG3,   "RARG3",    "rarg3"),
    RegisterInfo(RegisterNames.RARG4,   "RARG4",    "rarg4"),
    RegisterInfo(RegisterNames.RARG5,   "RARG5",    "rarg5"),
    RegisterInfo(RegisterNames.RARG6,   "RARG6",    "rarg6"),
    RegisterInfo(RegisterNames.RARG7,   "RARG7",    "rarg7"),
    RegisterInfo(RegisterNames.RARG8,   "RARG8",    "rarg8"),
    RegisterInfo(RegisterNames.ROUT1,   "ROUT1",    "rout1"),
    RegisterInfo(RegisterNames.ROUT2,   "ROUT2",    "rout2"),
    RegisterInfo(RegisterNames.RJUNK,   "RJUNK",    "rjunk"),
    RegisterInfo(RegisterNames.RSP,     "RSP",      "rsp"),
    RegisterInfo(RegisterNames.RBP,     "RBP",      "rbp"),
    RegisterInfo(RegisterNames.RIP,     "RIP",      "rip"),
    RegisterInfo(RegisterNames.RFLAGS,  "RFLAGS",   "rflags"),
    RegisterInfo(RegisterNames.RERROR,  "RERROR",   "rerror"),
    RegisterInfo(RegisterNames.R10,     "R10",      "r10"),
    RegisterInfo(RegisterNames.R11,     "R11",      "r11"),
    RegisterInfo(RegisterNames.R12,     "R12",      "r12"),
    RegisterInfo(RegisterNames.R13,     "R13",      "r13"),
    RegisterInfo(RegisterNames.R14,     "R14",      "r14"),
    RegisterInfo(RegisterNames.R15,     "R15",      "r15"),
    RegisterInfo(RegisterNames.R16,     "R16",      "r16"),
];

immutable(RegisterInfo)*
get_register_info(uintptr_t value){
    foreach(ref ri; registerinfos){
        if(ri.register == value)
            return &ri;
    }
    return null;
}

immutable(RegisterInfo)*
get_register_info(str name){
    foreach(ref ri; registerinfos){
        if(ri.name == name || ri.NAME == name)
            return &ri;
    }
    return null;
}


