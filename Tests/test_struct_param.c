#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

struct Point {
    int x;
    int y;
};

void print_point(struct Point p) {
    printf("Point: x=%d y=%d\n", p.x, p.y);
}

int main() {
    struct Point p1;
    p1.x = 100;
    p1.y = 200;
    print_point(p1);
    return 0;
}
