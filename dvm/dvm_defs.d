/*
 * Copyright © 2021-2022, David Priver
 */
module dvm.dvm_defs;
public import dlib.aliases;

version(Fuzz){
    enum Fuzzing = true;
}
else {
    enum Fuzzing = false;
}

enum AsmError: int {
    NO_ERROR = 0,
    PARSE_ERROR,
    LINK_ERROR,
}
