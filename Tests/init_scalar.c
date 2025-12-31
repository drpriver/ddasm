// Test (6.7.9) scalar initializers

int global_no_init;
int global_init = 42;
int global_expr = 10 + 20;

int test_local_no_init(void) {
    int x;
    x = 5;
    return x;
}

int test_local_init(void) {
    int x = 100;
    return x;
}

int test_local_expr_init(void) {
    int a = 10;
    int b = a + 5;
    return b;
}

int test_ptr_init(void) {
    int x = 42;
    int* p = &x;
    return *p;
}

int test_char_init(void) {
    char c = 'A';
    char d = 65;
    return c + d;
}
