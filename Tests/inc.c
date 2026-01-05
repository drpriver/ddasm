#include "inc.h"
#pragma library("libc")
    extern int printf(const char*, ...);
int start(){
    printf("%p\n", &x);
    return 0;
}
int main(){ return 0; }
