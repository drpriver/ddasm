#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

int main() {
    int x = 42;
    int* p = &x;  // Forces x to stack
    printf("x = %d\n", x);
    return 0;
}
