// Debug - test simplest double return
#pragma library("libc")
int printf(const char*, ...);

#pragma library("test_double")
double return_25(void);
double add_doubles(double a, double b);

int main(void) {
    printf("Calling return_25()...\n");
    double result = return_25();
    printf("return_25() = %f (expected 25.0)\n", result);

    printf("\nCalling add_doubles(3.0, 4.0)...\n");
    double result2 = add_doubles(3.0, 4.0);
    printf("add_doubles(3.0, 4.0) = %f (expected 7.0)\n", result2);

    return 0;
}
