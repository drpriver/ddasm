#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

struct Big {
    int a;
    int b;
    int c;
    int d;
};

void modify_struct(struct Big s) {
    s.a = 999;
    s.b = 888;
    printf("Inside function: a=%d b=%d c=%d d=%d\n", s.a, s.b, s.c, s.d);
}

int main() {
    struct Big b1;
    b1.a = 1;
    b1.b = 2;
    b1.c = 3;
    b1.d = 4;

    printf("Before call: a=%d b=%d c=%d d=%d\n", b1.a, b1.b, b1.c, b1.d);
    modify_struct(b1);
    printf("After call: a=%d b=%d c=%d d=%d\n", b1.a, b1.b, b1.c, b1.d);

    return 0;
}
