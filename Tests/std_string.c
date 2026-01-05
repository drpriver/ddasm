// Test <string.h> - String handling
#pragma library("libc")
#include <string.h>

size_t test_string(const char *s) {
    return strlen(s);
}
int main(){
    test_string("hello");
    return 0;
}
