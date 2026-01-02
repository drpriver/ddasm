#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

struct Point {
    int x;
    int y;
};

int main() {
    struct Point p1;
    struct Point p2;
    p1.x = 10;
    p1.y = 20;
    p2 = p1;  // struct assignment
    printf("p2.x=%d p2.y=%d\n", p2.x, p2.y);
    return 0;
}
