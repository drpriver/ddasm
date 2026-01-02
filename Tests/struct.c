#pragma library("libc.so.6")
    extern int printf(char*, ...);

struct Point {
    int x; int y;
};
void start(){
    struct Point p;
    p.x = 1;
    p.y = 2;
    struct Point* pp = &p;
    printf("&p: %p\n", &p);
    printf("pp: %p\n", pp);
    printf("&p.x: %p\n", &p.x);
    printf("&p.y: %p\n", &p.y);
    printf("&pp->x: %p\n", &pp->x);
    printf("&pp->y: %p\n", &pp->y);
}
