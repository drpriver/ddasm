#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

struct Point {
    int x;
    int y;
};

struct Point make_point(int x, int y) {
    struct Point p;
    p.x = x;
    p.y = y;
    return p;
}

void print_point(struct Point p) {
    printf("Point: x=%d y=%d\n", p.x, p.y);
}

int main() {
    // Direct use without intermediate variable
    print_point(make_point(100, 200));

    // Multiple calls
    struct Point a;
    struct Point b;
    a = make_point(1, 2);
    b = make_point(3, 4);
    printf("a: %d,%d  b: %d,%d\n", a.x, a.y, b.x, b.y);

    return 0;
}
