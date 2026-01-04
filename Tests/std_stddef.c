// Test <stddef.h> - Common definitions
#include <stddef.h>

size_t test_stddef(void) {
    return sizeof(ptrdiff_t);
}
int main(){
    test_stddef();
    return 0;
}
