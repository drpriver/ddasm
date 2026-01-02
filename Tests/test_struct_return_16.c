#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

struct Rect {
    int x;
    int y;
    int w;
    int h;
};

struct Rect make_rect(int x, int y, int w, int h) {
    struct Rect r;
    r.x = x;
    r.y = y;
    r.w = w;
    r.h = h;
    return r;
}

int main() {
    struct Rect r1;
    r1 = make_rect(10, 20, 100, 200);
    printf("r1: x=%d y=%d w=%d h=%d\n", r1.x, r1.y, r1.w, r1.h);
    return 0;
}
