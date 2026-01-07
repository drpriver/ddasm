#ifdef __DDASM__
#define _lineno(x) #x
#define __lineno(x) _lineno(x)
#define lineno __lineno(__LINE__)
#define assert(cond) if(!(cond)) __dasm { \
    msg __FILE__":" lineno ": Failed " #cond; \
    dump; \
    abort }
#else
void abort(void);
#define assert(cond) if(!(cond)) abort()
#endif

int abs(int x) {
    return x > 0 ? x : -x;
}

int max(int a, int b) {
    return a > b ? a : b;
}

int nested(int x) {
    return x > 10 ? 100 : x > 5 ? 50 : 0;
}
typedef struct {
    unsigned char r, g, b, a;
} Color;
void foo(Color c){
    assert(c.r == 5);
    assert(c.g == 6);
    assert(c.b == 7);
    assert(c.a == 8);
}
void struct_ternary(void){
    // Codegen error
    foo((1 == 0)? (Color){1,2,3,4}:(Color){5,6,7,8});
}

int main(){ 
    struct_ternary();
    assert(max(1, 2) == 2);
    assert(abs(-3) == 3);
    assert(nested(9) == 50);

    return 0; 
}
