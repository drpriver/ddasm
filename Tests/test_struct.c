#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

struct Point {
    int x;
    int y;
};

int main() {
    struct Point p;
    p.x = 10;
    p.y = 20;
    printf("p.x=%d p.y=%d\n", p.x, p.y);
    return 0;
}
