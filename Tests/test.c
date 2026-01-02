#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

int add(int a, int b) {
    return a + b;
}

void start() {
    int x = add(10, 20);
    printf("Result: %d\n", x);
}
