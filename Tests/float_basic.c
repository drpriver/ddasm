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

float addf(float a, float b) { return a + b; }
float subf(float a, float b) { return a - b; }
float mulf(float a, float b) { return a * b; }
float div_ff(float a, float b) { return a / b; }
float negf(float x) { return -x; }

int cmp_ltf(float a, float b) { return a < b; }
int cmp_lef(float a, float b) { return a <= b; }
int cmp_gtf(float a, float b) { return a > b; }
int cmp_gef(float a, float b) { return a >= b; }
int cmp_eqf(float a, float b) { return a == b; }
int cmp_nef(float a, float b) { return a != b; }

int to_intf(float x) { return (int)x; }
float from_intf(int x) { return (float)x; }


#ifdef __DDASM__
void abort(void){
    __dasm { dump; abort; }
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

#define assert_eqf(lhs, rhs) do { \
    if(lhs != rhs){ \
        printf("%s:%d: assertion failed\n", __FILE__, __LINE__); \
        printf("  %s = %g\n", #lhs, (float)lhs); \
        printf("  %s = %g\n", #rhs, (float)rhs); \
        abort(); \
    } \
}while(0)

void testd(void){
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
    int ge = cmp_ge(x, y);
    if(ge != 1) abort();
    int le = cmp_le(x, y);
    if(le != 0) abort();
    int eq = cmp_eq(x, y);
    if(eq != 0) abort();
    eq = cmp_eq(x, x);
    if(eq != 1) abort();

    int i = to_int(x);
    if(i != 3) abort();
    double d = from_int(42);
    assert_eq(d, 42.);
}

void testf(void){
    float x = 3.14159f;
    assert_eqf(x, 3.14159f);
    float y = 2.71828f;
    assert_eqf(y, 2.71828f);
    float f = 1.5f;  // float suffix
    assert_eqf(f, 1.5f);

    float sum = addf(x, y);
    assert_eqf(1.f+2.f, 3.f);
    assert_eqf(sum, 5.85987f);
    float diff = subf(x, y);
    assert_eqf(diff, 0.42331004f);
    float prod = mulf(x, y);
    assert_eqf(prod, 8.5397215f);
    float quot = div_ff(x, y);
    assert_eqf(quot, 1.1557271f);
    float n = negf(x);
    assert_eqf(n, -3.14159f);

    int lt = cmp_ltf(x, y);
    if(lt != 0) abort();
    int gt = cmp_gtf(x, y);
    if(gt != 1) abort();
    int ge = cmp_gef(x, y);
    if(ge != 1) abort();
    int le = cmp_lef(x, y);
    if(le != 0) abort();
    int eq = cmp_eqf(x, y);
    if(eq != 0) abort();
    eq = cmp_eqf(x, x);
    if(eq != 1) abort();

    int i = to_intf(x);
    if(i != 3) abort();
    float d = from_intf(42);
    assert_eqf(d, 42.);
}


int main() {
    testd();
    testf();
    return 0;
}
