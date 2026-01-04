#pragma library("libc.so.6")
    extern int printf(char*, ...);

struct Point {
    int x; int y;
};
struct Compound {
    struct Point p1;
    struct Point p2;
};
void start(){
    struct Compound c;
    c.p1.x = 1;
    c.p1.y = 2;
    c.p2.x = 3;
    c.p2.y = 4;
    struct Compound* pc = &c;
    struct Point* p1p = &c.p1;
    struct Point* p1pc = &pc->p1;
    int* x = &pc->p1.x;
    int* x2 = &p1p->x;
    int* x3 = &p1pc->x;
    printf("pc: %p\n", pc);
    printf("p1p: %p\n", p1p);
    printf("p1pc: %p\n", p1pc);
    printf("x: %p\n", x);
    printf("x2: %p\n", x2);
    printf("x3: %p\n", x3);
}
int main(){ return 0; }
