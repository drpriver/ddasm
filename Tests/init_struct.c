// Test (6.7.9) struct initializers

#ifdef __DDASM__
void abort(void){__dasm{dump;abort;}}
#else
void abort(void);
#endif

struct Point {
    int x;
    int y;
};

int test_struct_init(void) {
    struct Point p = {10, 20};
    if(p.x != 10) abort();
    if(p.y != 20) abort();
    return p.x + p.y;
}

int test_struct_partial(void) {
    struct Point p = {5};  // y is zero-initialized
    if(p.x != 5) abort();
    if(p.y != 0) abort();
    return p.x + p.y;
}
int main(){
    test_struct_init();
    test_struct_partial();
    return 0;
}
