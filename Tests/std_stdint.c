// Test <stdint.h> - Integer types
#include <stdint.h>

int64_t test_stdint(int32_t a, int32_t b) {
    return (int64_t)a + (int64_t)b;
}
int main(){
    test_stdint(1, 1);
    return 0;
}
