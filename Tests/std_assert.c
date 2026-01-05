// Test <assert.h> - Diagnostics
#pragma library("libc")
#include <assert.h>

static_assert(1, "");
_Static_assert(1, "");
int test_assert(void) {
    assert(1);
    static_assert(1, "");
    _Static_assert(1, "");
    // assert macro is defined
    return 0;
}
int main(){
    test_assert();
    return 0;
}
