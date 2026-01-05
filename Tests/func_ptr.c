// Test function pointers
#ifdef __DDASM__
void abort(void){__dasm{dump;abort;}}
#else
void abort(void);
#endif

// Simple functions to use as targets
int add(int a, int b) {
    return a + b;
}

int sub(int a, int b) {
    return a - b;
}

int mul(int a, int b) {
    return a * b;
}

int identity(int x) {
    return x;
}

// === Test 1: Basic function pointer variable ===
int test_basic_funcptr() {
    int (*fp)(int, int) = add;
    int result = fp(3, 4);
    if (result != 7) abort();
    return 0;
}

// === Test 2: Function pointer assignment ===
int test_funcptr_assignment() {
    int (*fp)(int, int);
    fp = add;
    if (fp(2, 3) != 5) abort();
    fp = sub;
    if (fp(10, 4) != 6) abort();
    fp = mul;
    if (fp(3, 5) != 15) abort();
    return 0;
}

// === Test 3: Function pointer as parameter ===
int apply(int (*f)(int, int), int x, int y) {
    return f(x, y);
}

int test_funcptr_param() {
    if (apply(add, 5, 3) != 8) abort();
    if (apply(sub, 10, 7) != 3) abort();
    if (apply(mul, 4, 6) != 24) abort();
    return 0;
}

// === Test 4: Function pointer returned from function ===
int (*get_operation(int op))(int, int) {
    if (op == 0) return add;
    if (op == 1) return sub;
    return mul;
}

int test_funcptr_return() {
    int (*fp)(int, int) = get_operation(0);
    if(!fp) abort();
    if (fp(2, 3) != 5) abort();
    fp = get_operation(1);
    if(!fp) abort();
    if (fp(10, 3) != 7) abort();
    fp = get_operation(2);
    if(!fp) abort();
    if (fp(4, 5) != 20) abort();
    return 0;
}

// === Test 5: Null function pointer ===
int test_null_funcptr() {
    int (*fp)(int, int) = 0;
    if (fp != 0) abort();
    fp = add;
    if (fp == 0) abort();
    return 0;
}

// === Test 6: Function pointer comparison ===
int test_funcptr_compare() {
    int (*fp1)(int, int) = add;
    int (*fp2)(int, int) = add;
    int (*fp3)(int, int) = sub;
    if (fp1 != fp2) abort();
    if (fp1 == fp3) abort();
    return 0;
}

// === Test 7: Array of function pointers ===
int test_funcptr_array() {
    int (*ops[3])(int, int) = {add, sub, mul};
    if (ops[0](2, 3) != 5) abort();
    if (ops[1](10, 4) != 6) abort();
    if (ops[2](3, 4) != 12) abort();
    return 0;
}

// === Test 8: Typedef for function pointer ===
typedef int (*binop_t)(int, int);

int test_funcptr_typedef() {
    binop_t op = add;
    if (op(6, 7) != 13) abort();
    op = mul;
    if (op(5, 5) != 25) abort();
    return 0;
}

// === Test 9: Struct containing function pointer ===
struct Operation {
    int (*func)(int, int);
    int operand;
};

int test_funcptr_struct() {
    struct Operation op;
    op.func = add;
    op.operand = 10;
    if (op.func(op.operand, 5) != 15) abort();
    return 0;
}

// === Test 10: Chained function calls ===
int test_funcptr_chain() {
    int (*fp)(int) = identity;
    int result = fp(fp(fp(42)));
    if (result != 42) abort();
    return 0;
}

int main() {
    test_basic_funcptr();
    test_funcptr_assignment();
    test_funcptr_param();
    test_funcptr_return();
    test_null_funcptr();
    test_funcptr_compare();
    test_funcptr_array();
    test_funcptr_typedef();
    test_funcptr_struct();
    test_funcptr_chain();
    return 0;
}
