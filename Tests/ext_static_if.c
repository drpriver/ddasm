#pragma library("libc")
#include <stdio.h>

enum {DEBUG=1, USE_FLOAT=1};

// Global scope: bare if (static is optional)
if(USE_FLOAT)
    typedef float number_t;
else
    typedef int number_t;

// Global scope: with braces
if(sizeof(void*) == 8){
    typedef long ptr_int_t;
}

// Global scope: static if also works
static if(DEBUG)
void debug_helper(void){ puts("debug_helper called"); }

int main(){
    // Statement scope: static if required
    static if(DEBUG){
        puts("Debug");
    }
    else {
        puts("Not Debug");
    }

    // Test that global typedefs work
    number_t x = 3.14;
    printf("x = %f\n", (double)x);
    printf("sizeof(ptr_int_t) = %zu\n", sizeof(ptr_int_t));

    // Test that global function works
    static if(DEBUG)
        debug_helper();

    return 0;
}
