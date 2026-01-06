// Test <tgmath.h> - Type-generic math (C99)
#ifndef __APPLE__
// This is too hard to parse.
#pragma watch __MATHCALL
#include <tgmath.h>
#endif


int test_tgmath(void) {
    return 0;
}
int main(){
    test_tgmath();
    return 0;
}
