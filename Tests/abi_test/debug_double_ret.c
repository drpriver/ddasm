// Debug simple double return
#pragma library("libc")
#pragma library("libm")

int printf(const char*, ...);
double sqrt(double x);

int main(void) {
    double x = 25.0;
    printf("x = %f\n", x);

    double result = sqrt(x);
    printf("sqrt(x) = %f (expected 5.0)\n", result);

    return 0;
}
