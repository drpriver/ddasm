// Test (6.7) basic declarations

int global_int;
int global_init = 42;

int test_local_decl(void) {
    int x;
    int y = 10;
    return y;
}

int test_multiple_stmts(void) {
    int a;
    int b;
    int c;
    a = 1;
    b = 2;
    c = 3;
    return a + b + c;
}
