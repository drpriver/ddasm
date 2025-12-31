// Test (6.5.3) unary-expression
// ++/--, &, *, +, -, ~, !, sizeof

int test_prefix_incr(int x) {
    int a = ++x;
    int b = --x;
    return a + b;
}

int test_address_deref(int* p) {
    int val = *p;
    int* ptr = &val;
    *ptr = 100;
    return val;
}

int test_unary_minus(int x) {
    int neg = -x;
    int neg2 = -(-x);
    return neg + neg2;
}

int test_bitwise_not(int x) {
    return ~x;
}

int test_logical_not(int x) {
    int a = !x;
    int b = !0;
    int c = !1;
    return a + b + c;
}

int test_sizeof_type(void) {
    int s1 = sizeof(int);
    int s2 = sizeof(char);
    int s3 = sizeof(int*);
    return s1 + s2 + s3;
}

int test_sizeof_expr(int x) {
    int s = sizeof x;
    return s;
}
