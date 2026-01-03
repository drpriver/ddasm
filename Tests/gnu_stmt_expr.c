// Test GNU statement expressions ({ ... })

int test_basic() {
    // Basic statement expression returning a value
    int x = ({ 42; });
    if (x != 42) return 1;
    return 0;
}

int test_multiple_stmts() {
    // Multiple statements, last one is the value
    int result = ({
        int a = 10;
        int b = 20;
        a + b;
    });
    if (result != 30) return 1;
    return 0;
}

int test_side_effects() {
    // Statement expression with side effects
    int counter = 0;
    int val = ({
        counter = counter + 1;
        counter = counter + 1;
        counter * 10;
    });
    if (counter != 2) return 1;
    if (val != 20) return 2;
    return 0;
}

int test_in_expression() {
    // Statement expression used in larger expression
    int x = 5 + ({ 3; }) * 2;
    if (x != 11) return 1;  // 5 + (3 * 2) = 11
    return 0;
}

int test_nested() {
    // Nested statement expressions
    int x = ({
        int inner = ({ 5; });
        inner * 2;
    });
    if (x != 10) return 1;
    return 0;
}

int test_void_context() {
    // Statement expression used for side effects only
    int x = 0;
    ({
        x = 42;
    });
    if (x != 42) return 1;
    return 0;
}

int test_conditional() {
    // Statement expression in conditional
    int x = 10;
    if (({ x > 5; })) {
        return 0;
    }
    return 1;
}

int test_with_comma() {
    // Statement expression combined with comma operator (like assert macro)
    int x = ((void)0, ({ 42; }));
    if (x != 42) return 1;
    return 0;
}

int main() {
    if (test_basic() != 0) return 1;
    if (test_multiple_stmts() != 0) return 2;
    if (test_side_effects() != 0) return 3;
    if (test_in_expression() != 0) return 4;
    if (test_nested() != 0) return 5;
    if (test_void_context() != 0) return 6;
    if (test_conditional() != 0) return 7;
    if (test_with_comma() != 0) return 8;
    return 0;
}
