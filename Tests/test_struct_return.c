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

int main() {
    struct Point p1;
    p1 = make_point(42, 99);
    printf("p1: x=%d y=%d\n", p1.x, p1.y);
    return 0;
}
