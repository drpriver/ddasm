// Test <stdarg.h> - Variable arguments
#include <stdarg.h>

int sum(int n, ...) {
    va_list ap;
    va_start(ap, n);
    int x = va_arg(ap, int);
    va_end(ap);
    return x;
}
int main(){ return 0; }
