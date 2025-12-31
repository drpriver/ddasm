// Test (6.8.5.2) do-while statement

int test_do_while(int n) {
    int i = 0;
    do {
        i++;
    } while (i < n);
    return i;
}

int test_do_while_once(void) {
    // do-while always executes at least once
    int x = 0;
    do {
        x = 42;
    } while (0);  // False condition - executes body once
    return x;
}

int test_do_while_break(int n) {
    int i = 0;
    do {
        i++;
        if (i == 5) break;
    } while (i < n);
    return i;
}

int test_do_while_continue(void) {
    int sum = 0;
    int i = 0;
    do {
        i++;
        if (i % 2 == 0) continue;  // Skip even numbers
        sum = sum + i;
    } while (i < 10);
    return sum;  // 1 + 3 + 5 + 7 + 9 = 25
}

int test_nested_do_while(void) {
    int result = 0;
    int i = 0;
    do {
        int j = 0;
        do {
            result++;
            j++;
        } while (j < 3);
        i++;
    } while (i < 2);
    return result;  // 2 * 3 = 6
}
