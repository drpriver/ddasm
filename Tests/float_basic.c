// Basic float operations test
#pragma library("libc");
#include <stdio.h>
double add(double a, double b) { return a + b; }
double sub(double a, double b) { return a - b; }
double mul(double a, double b) { return a * b; }
double div_f(double a, double b) { return a / b; }
double neg(double x) { return -x; }

int cmp_lt(double a, double b) { return a < b; }
int cmp_le(double a, double b) { return a <= b; }
int cmp_gt(double a, double b) { return a > b; }
int cmp_ge(double a, double b) { return a >= b; }
int cmp_eq(double a, double b) { return a == b; }
int cmp_ne(double a, double b) { return a != b; }

int to_int(double x) { return (int)x; }
double from_int(int x) { return (double)x; }

#ifdef __DDASM__
void abort(void){
    __dasm { abort; }
}
#else
#include <stdlib.h>
#endif

#define assert_eq(lhs, rhs) do { \
    if(lhs != rhs){ \
        printf("%s:%d: assertion failed\n", __FILE__, __LINE__); \
        printf("  %s = %g\n", #lhs, (double)lhs); \
        printf("  %s = %g\n", #rhs, (double)rhs); \
        abort(); \
    } \
}while(0)

int main() {
    double x = 3.14159;
    assert_eq(x, 3.14159);
    double y = 2.71828;
    assert_eq(y, 2.71828);
    float f = 1.5f;  // float suffix
    assert_eq(f, 1.5f);
    
    double sum = add(x, y);
    assert_eq(1.+2., 3.);
    assert_eq(sum, 5.85987);
    double diff = sub(x, y);
    assert_eq(diff, 0.42330999999999985);
    double prod = mul(x, y);
    assert_eq(prod, 8.539721265199999);
    double quot = div_f(x, y);
    assert_eq(quot, 1.1557271509925393);
    double n = neg(x);
    assert_eq(n, -3.14159);
    
    int lt = cmp_lt(x, y);
    if(lt != 0) abort();
    int gt = cmp_gt(x, y);
    if(gt != 1) abort();
    
    int i = to_int(x);
    if(i != 3) abort();
    double d = from_int(42);
    assert_eq(d, 42.);
    
    return 0;
}
