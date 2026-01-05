// Test <wctype.h> - Wide character classification
#pragma library("libc")
#include <wctype.h>

int test_wctype(wint_t c) {
    return iswalpha(c);
}
int main(){
    test_wctype(1);
    return 0;
}
