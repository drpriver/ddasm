// Test <inttypes.h> - Format conversion of integer types
#include <inttypes.h>

int test_inttypes(void) {
    int64_t x = 42;
    return (int)x;
}
