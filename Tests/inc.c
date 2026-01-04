// SKIP: uses macOS-specific libSystem.dylib
#include "inc.h"
#pragma library("libSystem.dylib")
    extern int printf(const char*, ...);
int start(){
    printf("%p\n", &x);
    return 0;
}
int main(){ return 0; }
