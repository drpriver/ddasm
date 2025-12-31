// Test (6.8.5.1) while statement

int test_while_simple(int n) {
    int sum = 0;
    int i = 0;
    while (i < n) {
        sum += i;
        i++;
    }
    return sum;
}

int test_while_condition(int x) {
    while (x > 0) {
        x--;
    }
    return x;
}

int test_while_no_braces(int n) {
    int i = 0;
    while (i < n)
        i++;
    return i;
}

int test_while_break(int n) {
    int i = 0;
    while (1) {
        if (i >= n) {
            break;
        }
        i++;
    }
    return i;
}

int test_while_continue(int n) {
    int sum = 0;
    int i = 0;
    while (i < n) {
        i++;
        if (i % 2 == 0) {
            continue;
        }
        sum += i;
    }
    return sum;
}
