// Test (6.5.4) cast-expression

int test_basic_cast(void) {
    int i = 42;
    char c = (char)i;
    long l = (long)i;
    return c + (int)l;
}

void* test_pointer_cast(int* p) {
    void* v = (void*)p;
    int* back = (int*)v;
    return v;
}

int test_nested_cast(int x) {
    return (int)(char)(int)x;
}

int test_cast_in_expr(int x, int y) {
    return (int)x + (int)y;
}
