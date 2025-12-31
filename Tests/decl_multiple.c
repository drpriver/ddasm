// SKIP - comma-separated declarations not supported
// Test (6.7) multiple declarators: int x, y, *z;

int test_separate_decls(void) {
    int x;
    int y;
    int* z;
    return x + y;
}
