#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

int main() {
    int x = 42;
    int* p = &x;
    *p = 100;
    printf("x = %d\n", x);
    return 0;
}
