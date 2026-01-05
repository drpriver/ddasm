// Test <errno.h> - Errors
#pragma library("libc")
#include <errno.h>

int test_errno(void) {
    return errno;
}
int main(){
    test_errno();
    return 0;
}
