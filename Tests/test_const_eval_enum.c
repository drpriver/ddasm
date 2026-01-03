#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

// Test that enum constants are evaluated at compile time

enum Values {
    ZERO = 0,
    ONE = 1,
    TWO = 2,
    TEN = 10,
    HUNDRED = 100,
    NEG = -5
};

int main() {
    // Test basic enum constant usage
    printf("ZERO = %d\n", ZERO);
    printf("ONE = %d\n", ONE);
    printf("TEN = %d\n", TEN);
    printf("NEG = %d\n", NEG);

    // Test enum in expressions (should be constant folded)
    int a = ONE + TWO;          // Should fold to 3
    int b = TEN * TWO;          // Should fold to 20
    int c = HUNDRED - TEN;      // Should fold to 90
    int d = HUNDRED / TEN;      // Should fold to 10
    int e = TEN + ONE * TWO;    // Should fold to 12
    int f = (TEN + TWO) * ONE;  // Should fold to 12
    int g = NEG + TEN;          // Should fold to 5
    int h = -NEG;               // Should fold to 5

    printf("ONE + TWO = %d\n", a);
    printf("TEN * TWO = %d\n", b);
    printf("HUNDRED - TEN = %d\n", c);
    printf("HUNDRED / TEN = %d\n", d);
    printf("TEN + ONE * TWO = %d\n", e);
    printf("(TEN + TWO) * ONE = %d\n", f);
    printf("NEG + TEN = %d\n", g);
    printf("-NEG = %d\n", h);

    // Test enum in comparisons
    int cmp1 = ONE < TEN;       // Should be 1
    int cmp2 = TEN > HUNDRED;   // Should be 0
    int cmp3 = ONE == ONE;      // Should be 1
    int cmp4 = ZERO != ONE;     // Should be 1

    printf("ONE < TEN = %d\n", cmp1);
    printf("TEN > HUNDRED = %d\n", cmp2);
    printf("ONE == ONE = %d\n", cmp3);
    printf("ZERO != ONE = %d\n", cmp4);

    // Test enum in ternary
    int t = ONE ? TEN : ZERO;   // Should be 10
    printf("ONE ? TEN : ZERO = %d\n", t);

    return 0;
}
