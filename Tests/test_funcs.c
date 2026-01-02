#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

int add(int a, int b) {
    return a + b;
}

int factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}

void start() {
    int result = add(10, 20);
    printf("add(10, 20) = %d\n", result);

    int fact5 = factorial(5);
    printf("factorial(5) = %d\n", fact5);
}
