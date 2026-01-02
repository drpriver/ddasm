#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

union IntOrFloat {
    int i;
    int f;  // Using int instead of float since we don't support float yet
};

void test_union() {
    union IntOrFloat u;
    u.i = 42;
    printf("As int: %d\n", u.i);
    u.f = 123;
    printf("As 'float': %d\n", u.f);
}

int main() {
    test_union();
    return 0;
}
