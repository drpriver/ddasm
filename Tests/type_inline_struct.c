// Test inline struct definitions

// Simple inline struct member
struct Simple {
    struct { int x; } inner;
};

int test_simple(void) {
    struct Simple s;
    s.inner.x = 10;
    return s.inner.x;
}
int main(){
    test_simple();
    return 0;
}
