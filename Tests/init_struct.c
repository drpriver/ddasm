// Test (6.7.9) struct initializers

#ifdef __DDASM__
#define _lineno(x) #x
#define __lineno(x) _lineno(x)
#define lineno __lineno(__LINE__)
#define abort() __dasm { \
    msg __FILE__":" lineno ": Failed" ; \
    dump; \
    abort }
#else
void abort(void);
#endif

struct Point {
    int x;
    int y;
};

struct Points3 {
    struct Point points[3];
};

int test_struct_init(void) {
    struct Point p = {10, 20};
    if(p.x != 10) abort();
    if(p.y != 20) abort();
    return p.x + p.y;
}

int test_struct_init2(void) {
    struct Points3 p = {10, 20};
    if(p.points[0].x != 10) abort();
    if(p.points[0].y != 20) abort();
    if(p.points[1].x != 0) abort();
    if(p.points[2].x != 0) abort();
    return p.points[0].x + p.points[0].y;
}

int test_struct_partial(void) {
    struct Point p = {5};  // y is zero-initialized
    if(p.x != 5) abort();
    if(p.y != 0) abort();
    return p.x + p.y;
}
int main(){
    if(test_struct_init() != 30) return __LINE__;
    if(test_struct_partial() != 5) return __LINE__;
    if(test_struct_init2() != 30) return __LINE__;
    return 0;
}
