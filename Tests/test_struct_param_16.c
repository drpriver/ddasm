#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

struct Rect {
    int x;
    int y;
    int w;
    int h;
};

void print_rect(struct Rect r) {
    printf("Rect: x=%d y=%d w=%d h=%d\n", r.x, r.y, r.w, r.h);
}

int main() {
    struct Rect r1;
    r1.x = 10;
    r1.y = 20;
    r1.w = 100;
    r1.h = 200;
    print_rect(r1);
    return 0;
}
