#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

struct Big {
    int a;
    int b;
    int c;
    int d;
};

int main() {
    struct Big s1;
    struct Big s2;
    s1.a = 1;
    s1.b = 2;
    s1.c = 3;
    s1.d = 4;
    s2 = s1;  // struct assignment - 16 bytes
    printf("s2: a=%d b=%d c=%d d=%d\n", s2.a, s2.b, s2.c, s2.d);
    return 0;
}
