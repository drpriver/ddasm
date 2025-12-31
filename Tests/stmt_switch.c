// Test (6.8.4.2) switch statement

int test_switch_basic(int x) {
    int result;
    switch (x) {
        case 1:
            result = 10;
            break;
        case 2:
            result = 20;
            break;
        case 3:
            result = 30;
            break;
        default:
            result = 0;
            break;
    }
    return result;
}

int test_switch_no_default(int x) {
    int result = -1;
    switch (x) {
        case 1:
            result = 100;
            break;
        case 2:
            result = 200;
            break;
    }
    return result;
}

int test_switch_fallthrough(int x) {
    int result = 0;
    switch (x) {
        case 1:
        case 2:
        case 3:
            result = 123;  // All three cases get this value
            break;
        default:
            result = -1;
            break;
    }
    return result;
}

int test_switch_default_middle(int x) {
    int result;
    switch (x) {
        case 1:
            result = 10;
            break;
        default:
            result = 99;
            break;
        case 2:
            result = 20;
            break;
    }
    return result;
}

int test_switch_expressions(int x) {
    int result;
    switch (x + 1) {
        case 5:
            result = 50;
            break;
        case 10:
            result = 100;
            break;
        default:
            result = 0;
            break;
    }
    return result;
}
