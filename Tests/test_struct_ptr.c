#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

struct Point {
    int x;
    int y;
};

int main() {
    struct Point p;
    struct Point* ptr;
    ptr = &p;
    ptr->x = 100;
    ptr->y = 200;
    printf("ptr->x=%d ptr->y=%d\n", ptr->x, ptr->y);
    printf("p.x=%d p.y=%d\n", p.x, p.y);
    return 0;
}
