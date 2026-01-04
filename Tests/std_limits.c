// Test <limits.h> - Sizes of integer types
#include <limits.h>

int test_limits(void) {
    return INT_MAX > 0 ? 1 : 0;
}
int main(){
    test_limits();
    return 0;
}
