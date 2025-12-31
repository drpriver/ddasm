// Test (6.7) multiple declarators: int x, y, *z;

int test_multiple_ints(void) {
    int x, y, z;
    x = 1;
    y = 2;
    z = 3;
    return x + y + z;
}

int test_mixed_pointers(void) {
    int a, *b, **c;
    int val = 42;
    a = 10;
    b = &val;
    c = &b;
    return a + *b + **c;  // 10 + 42 + 42 = 94
}

int test_with_initializers(void) {
    int x = 1, y = 2, z = 3;
    return x + y + z;  // 6
}

int test_partial_init(void) {
    int a = 10, b, c = 30;
    b = 20;
    return a + b + c;  // 60
}

int test_arrays_and_scalars(void) {
    int x, arr[3], y;
    x = 1;
    arr[0] = 10;
    arr[1] = 20;
    arr[2] = 30;
    y = 2;
    return x + arr[0] + arr[1] + arr[2] + y;  // 63
}
