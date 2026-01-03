// Test <assert.h> - Diagnostics
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
