#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

struct BigStruct {
    int a;
    int b;
    int c;
    int d;
    int e;  // 20 bytes - too big for registers
};

struct BigStruct make_big(int a, int b, int c, int d) {
    struct BigStruct s;
    s.a = a;
    s.b = b;
    s.c = c;
    s.d = d;
    s.e = 99;
    return s;
}

int main() {
    struct BigStruct s1;
    s1 = make_big(1, 2, 3, 4);
    printf("s1: a=%d b=%d c=%d d=%d e=%d\n", s1.a, s1.b, s1.c, s1.d, s1.e);
    return 0;
}
