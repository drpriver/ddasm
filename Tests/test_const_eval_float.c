#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

// Test that float constants are evaluated at compile time

int main() {
    // Test basic float constant folding
    double a = 1.0 + 2.0;           // Should fold to 3.0
    double b = 10.0 - 3.0;          // Should fold to 7.0
    double c = 2.5 * 4.0;           // Should fold to 10.0
    double d = 15.0 / 3.0;          // Should fold to 5.0

    printf("1.0 + 2.0 = %f\n", a);
    printf("10.0 - 3.0 = %f\n", b);
    printf("2.5 * 4.0 = %f\n", c);
    printf("15.0 / 3.0 = %f\n", d);

    // Test float negation
    double e = -5.5;                // Should be -5.5
    double f = --5.5;               // Should be 5.5 (double negation)
    printf("-5.5 = %f\n", e);
    printf("--5.5 = %f\n", f);

    // Test mixed expressions
    double g = 1.0 + 2.0 * 3.0;     // Should fold to 7.0
    double h = (1.0 + 2.0) * 3.0;   // Should fold to 9.0
    printf("1.0 + 2.0 * 3.0 = %f\n", g);
    printf("(1.0 + 2.0) * 3.0 = %f\n", h);

    // Test float comparisons
    int cmp1 = 1.0 < 2.0;           // Should be 1
    int cmp2 = 3.0 > 2.0;           // Should be 1
    int cmp3 = 2.0 == 2.0;          // Should be 1
    int cmp4 = 1.5 != 2.5;          // Should be 1
    int cmp5 = 1.0 <= 1.0;          // Should be 1
    int cmp6 = 2.0 >= 3.0;          // Should be 0

    printf("1.0 < 2.0 = %d\n", cmp1);
    printf("3.0 > 2.0 = %d\n", cmp2);
    printf("2.0 == 2.0 = %d\n", cmp3);
    printf("1.5 != 2.5 = %d\n", cmp4);
    printf("1.0 <= 1.0 = %d\n", cmp5);
    printf("2.0 >= 3.0 = %d\n", cmp6);

    // Test float in ternary
    double t = 1.0 ? 10.5 : 20.5;   // Should be 10.5
    printf("1.0 ? 10.5 : 20.5 = %f\n", t);

    // Test cast from int to float
    double i = (double)5;           // Should be 5.0
    printf("(double)5 = %f\n", i);

    // Test cast from float to int
    int j = (int)3.7;               // Should be 3
    printf("(int)3.7 = %d\n", j);

    return 0;
}
