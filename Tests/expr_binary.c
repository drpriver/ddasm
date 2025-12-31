// Test (6.5.5-6.5.14) binary operators and precedence

int test_multiplicative(int x, int y) {
    int a = x * y;
    int b = x / y;
    int c = x % y;
    return a + b + c;
}

int test_additive(int x, int y) {
    int a = x + y;
    int b = x - y;
    return a + b;
}

int test_shift(int x) {
    int left = x << 2;
    int right = x >> 1;
    return left + right;
}

int test_relational(int x, int y) {
    int a = x < y;
    int b = x > y;
    int c = x <= y;
    int d = x >= y;
    return a + b + c + d;
}

int test_equality(int x, int y) {
    int a = x == y;
    int b = x != y;
    return a + b;
}

int test_bitwise_and(int x, int y) {
    return x & y;
}

int test_bitwise_xor(int x, int y) {
    return x ^ y;
}

int test_bitwise_or(int x, int y) {
    return x | y;
}

int test_logical_and(int x, int y) {
    return x && y;
}

int test_logical_or(int x, int y) {
    return x || y;
}

// Test precedence: && binds tighter than ||
int test_precedence_logical(int a, int b, int c) {
    return a || b && c;  // a || (b && c)
}

// Test precedence: & binds tighter than ^, which binds tighter than |
int test_precedence_bitwise(int a, int b, int c) {
    return a | b ^ c & 0xFF;  // a | (b ^ (c & 0xFF))
}

// Test precedence: comparison binds tighter than equality
int test_precedence_compare(int a, int b, int c) {
    return a == b < c;  // a == (b < c)
}
