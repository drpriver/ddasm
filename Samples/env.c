#pragma library("libc")
#include <stdio.h>
// Shows off fun macros
#define CODE __MIXIN__(__ENV__("CODE"))
#define DEBUG __MIXIN__(__ENV__("DEBUG"))
#define PWD __ENV__("PWD")
int main(){
    // If CODE is not set, this will convert to ("hello");
    CODE("hello");
#if DEBUG
    puts("debug");
#else
    puts("nodebug");
#endif
    puts("User is " __ENV__("USER"));
    puts("Working in " PWD);
    return 0;
}
