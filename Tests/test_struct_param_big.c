#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

struct Big {
    int a;
    int b;
    int c;
    int d;
    int e;  // 20 bytes
};

void print_big(struct Big s) {
    printf("Big: a=%d b=%d c=%d d=%d e=%d\n", s.a, s.b, s.c, s.d, s.e);
}

int main() {
    struct Big b1;
    b1.a = 1;
    b1.b = 2;
    b1.c = 3;
    b1.d = 4;
    b1.e = 5;
    print_big(b1);
    return 0;
}
