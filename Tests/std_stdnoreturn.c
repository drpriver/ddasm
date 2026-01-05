// Test <stdnoreturn.h> - Noreturn macro (C11)
#pragma library("libc")
#include <stdnoreturn.h>
#include <stdlib.h>

noreturn void die(void) {
    exit(1);
}

int test_noreturn(int x) {
    if (x < 0) die();
    return x;
}
int main(){
    test_noreturn(1);
    return 0;
}
