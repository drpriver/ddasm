// Test (6.8.3) expression-statement

int global_counter;

void test_expr_stmt(void) {
    global_counter = 0;
    global_counter++;
    global_counter += 10;
}

int test_call_stmt(void) {
    test_expr_stmt();
    return global_counter;
}

int test_empty_stmt(void) {
    ;
    ;;
    return 0;
}

int test_side_effects(int* p) {
    *p = 42;
    (*p)++;
    return *p;
}
