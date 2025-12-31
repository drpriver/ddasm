// Test <ctype.h> - Character handling
#include <ctype.h>

int test_ctype(int c) {
    return isalpha(c) + isdigit(c) + isspace(c);
}
