// Test <setjmp.h> - Nonlocal jumps
#pragma library("libc")
int printf(const char*,...);
#include <setjmp.h>

jmp_buf env;

int test_setjmp(void) {
    printf("%zu\n", sizeof env);
    printf("%zu\n", sizeof(jmp_buf));
    return 0;
}

int main(){
    test_setjmp();
    return 0;
}
