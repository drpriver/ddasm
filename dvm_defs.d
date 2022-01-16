alias uintptr_t = size_t;
alias intptr_t = ptrdiff_t;

version(Fuzz){
    enum Fuzzing = true;
}
else {
    enum Fuzzing = false;
}
