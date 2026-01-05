// Test <stdio.h> - Input/output
#pragma library("libc")
#include <stdio.h>

int test_stdio(void) {
    printf("hello\n");
    return 0;
}
int main(){
    test_stdio();
    return 0;
}
