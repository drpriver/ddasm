// Test <wctype.h> - Wide character classification
#include <wctype.h>

int test_wctype(wint_t c) {
    return iswalpha(c);
}
