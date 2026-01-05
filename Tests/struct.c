#pragma library("libc")
    extern int printf(char*, ...);

struct Point {
    int x; int y;
};
struct Point2 {
    int x, y;
} p2 = {3, 4};
int test(){
    struct Point p;
    p.x = 1;
    p.y = 2;
    if(p.x != 1) return __LINE__;
    if(p.y != 2) return __LINE__;
    if(p2.x != 3) return __LINE__;
    if(p2.y != 4) return __LINE__;
    struct Point3 {
        int x, y;
    } p3 = {5, 6};
    if(p3.x != 5) return __LINE__;
    if(p3.y != 6) return __LINE__;
    struct Point* pp = &p;
    printf("&p: %p\n", &p);
    printf("pp: %p\n", pp);
    printf("&p.x: %p\n", &p.x);
    printf("&p.y: %p\n", &p.y);
    printf("&pp->x: %p\n", &pp->x);
    printf("&pp->y: %p\n", &pp->y);
    return 0;
}
int main(){ return test(); }
