// Test (6.5.16) assignment-expression
// =, +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=

int test_simple_assign(void) {
    int x;
    x = 42;
    return x;
}

int test_compound_add_sub(int x) {
    x += 10;
    x -= 5;
    return x;
}

int test_compound_mul_div_mod(int x) {
    x *= 2;
    x /= 3;
    x %= 7;
    return x;
}

int test_compound_bitwise(int x) {
    x &= 0xFF;
    x |= 0x100;
    x ^= 0x55;
    return x;
}

int test_compound_shift(int x) {
    x <<= 2;
    x >>= 1;
    return x;
}

// Assignment is right-associative
int test_chain_assign(void) {
    int a;
    int b;
    int c;
    a = b = c = 10;
    return a + b + c;
}

// Assignment in expression context
int test_assign_in_expr(int* p) {
    int x;
    *p = x = 5;
    return x;
}
int main(){
    int x = 0;
    test_simple_assign();
    test_compound_add_sub(1);
    test_compound_mul_div_mod(1);
    test_compound_bitwise(1);
    test_compound_shift(1);
    test_chain_assign();
    test_assign_in_expr(&x);
    return 0;
}
