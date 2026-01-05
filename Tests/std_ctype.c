// Test <ctype.h> - Character handling
#pragma library("libc")
#include <ctype.h>

int test_ctype(int c) {
    return isalpha(c) + isdigit(c) + isspace(c);
}
int main(){
    test_ctype(1);
    return 0;
}
