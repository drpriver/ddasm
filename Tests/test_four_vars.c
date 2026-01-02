#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

int main() {
    int a = 1;
    int b = 2;
    int c = 3;
    int d = 4;
    // Only 4 variables - should stay in registers

    printf("sum of 1-4 = %d\n", a + b + c + d);

    return 0;
}
