// Test (6.8.4.1) if statement

int test_if_simple(int x) {
    if (x > 0) {
        return 1;
    }
    return 0;
}

int test_if_else(int x) {
    if (x > 0) {
        return 1;
    } else {
        return -1;
    }
}

int test_if_else_if(int x) {
    if (x > 0) {
        return 1;
    } else if (x < 0) {
        return -1;
    } else {
        return 0;
    }
}

int test_if_no_braces(int x) {
    if (x > 0)
        return 1;
    else
        return -1;
}

int test_nested_if(int x, int y) {
    if (x > 0) {
        if (y > 0) {
            return 1;
        } else {
            return 2;
        }
    }
    return 0;
}

// Dangling else
int test_dangling_else(int a, int b) {
    if (a)
        if (b)
            return 1;
        else
            return 2;
    return 0;
}
int main(){
    test_if_simple(1);
    test_if_else(1);
    test_if_else_if(1);
    test_if_no_braces(1);
    test_nested_if(1, 1);
    test_dangling_else(1, 1);
    return 0;
}
