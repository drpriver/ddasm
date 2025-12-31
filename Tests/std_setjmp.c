// Test <setjmp.h> - Nonlocal jumps
#include <setjmp.h>

jmp_buf env;

int test_setjmp(void) {
    return 0;
}
