// Test (6.7.9) struct initializers

struct Point {
    int x;
    int y;
};

int test_struct_init(void) {
    struct Point p = {10, 20};
    return p.x + p.y;
}

int test_struct_partial(void) {
    struct Point p = {5};  // y is zero-initialized
    return p.x + p.y;
}
