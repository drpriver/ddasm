#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

// Simple typedef
typedef int MyInt;
typedef int* IntPtr;

// Struct typedef
typedef struct {
    int x;
    int y;
} Point;

// Struct typedef with named struct
typedef struct Vec2 {
    int a;
    int b;
} Vector2;

int main() {
    MyInt x;
    x = 42;
    printf("x = %d\n", x);

    int val;
    val = 100;
    IntPtr p;
    p = &val;
    printf("*p = %d\n", *p);

    Point pt;
    pt.x = 10;
    pt.y = 20;
    printf("pt = (%d, %d)\n", pt.x, pt.y);

    Vector2 v;
    v.a = 5;
    v.b = 6;
    printf("v = (%d, %d)\n", v.a, v.b);

    // Can also use struct Vec2 since it was named
    struct Vec2 v2;
    v2.a = 7;
    v2.b = 8;
    printf("v2 = (%d, %d)\n", v2.a, v2.b);

    return 0;
}
