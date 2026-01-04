// Test (6.8.6) jump statements: break, continue, return

int test_return_void(void) {
    return 0;
}

int test_return_expr(int x) {
    return x + 1;
}

int test_early_return(int x) {
    if (x < 0) {
        return -1;
    }
    return x;
}

int test_break_in_for(void) {
    int i;
    for (i = 0; i < 100; i++) {
        if (i == 10) {
            break;
        }
    }
    return i;
}

int test_break_in_while(void) {
    int i = 0;
    while (i < 100) {
        if (i == 10) {
            break;
        }
        i++;
    }
    return i;
}

int test_continue_in_for(void) {
    int sum = 0;
    int i;
    for (i = 0; i < 10; i++) {
        if (i % 2 == 0) {
            continue;
        }
        sum += i;
    }
    return sum;
}

int test_continue_in_while(int n) {
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
int main(){
    test_return_void();
    test_return_expr(1);
    test_early_return(1);
    test_break_in_for();
    test_break_in_while();
    test_continue_in_for();
    test_continue_in_while(1);
    return 0;
}
