// Test <stdlib.h> - General utilities
#include <stdlib.h>

int test_stdlib(void) {
    void *p = malloc(100);
    if (p) free(p);
    return abs(-5);
}
int main(){
    test_stdlib();
    return 0;
}
