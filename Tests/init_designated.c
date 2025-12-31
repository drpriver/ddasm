// SKIP - designated initializers not implemented
// Test (6.7.9) designated initializers

/*
struct Point {
    int x;
    int y;
};

int test_designated_struct(void) {
    struct Point p = {.y = 20, .x = 10};
    return p.x + p.y;
}

int test_designated_array(void) {
    int arr[10] = {[5] = 50, [9] = 90};
    return arr[5] + arr[9];
}
*/
