// Test (6.8.5.3) for statement

int test_for_simple(int n) {
    int sum = 0;
    int i;
    for (i = 0; i < n; i++) {
        sum += i;
    }
    return sum;
}

int test_for_no_init(int n) {
    int i = 0;
    int sum = 0;
    for (; i < n; i++) {
        sum += i;
    }
    return sum;
}

int test_for_no_cond(void) {
    int i = 0;
    for (i = 0; ; i++) {
        if (i >= 5) {
            break;
        }
    }
    return i;
}

int test_for_no_incr(int n) {
    int i;
    for (i = 0; i < n; ) {
        i++;
    }
    return i;
}

int test_for_no_braces(int n) {
    int sum = 0;
    int i;
    for (i = 0; i < n; i++)
        sum += i;
    return sum;
}

int test_for_nested(int m, int n) {
    int sum = 0;
    int i;
    int j;
    for (i = 0; i < m; i++) {
        for (j = 0; j < n; j++) {
            sum++;
        }
    }
    return sum;
}

int test_for_break(int n) {
    int i;
    for (i = 0; i < n; i++) {
        if (i == 5) {
            break;
        }
    }
    return i;
}

int test_for_continue(int n) {
    int sum = 0;
    int i;
    for (i = 0; i < n; i++) {
        if (i % 2 == 0) {
            continue;
        }
        sum += i;
    }
    return sum;
}
int main(){
    test_for_simple(1);
    test_for_no_init(1);
    test_for_no_cond();
    test_for_no_incr(1);
    test_for_no_braces(1);
    test_for_nested(1, 1);
    test_for_break(1);
    test_for_continue(1);
    return 0;
}
