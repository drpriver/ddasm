#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

int main() {
    int x = 42;
    int* p = &x;
    printf("x = %d\n", x);
    printf("*p = %d\n", *p);
    *p = 100;
    printf("After *p = 100: x = %d\n", x);
    return 0;
}
