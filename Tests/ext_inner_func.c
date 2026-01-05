#pragma library("libc")
#include <stdio.h>

// Test 1: Basic inner function with typedef
int test_basic() {
    typedef int myint;

    myint inner(myint x) {
        printf("inner: %d\n", x);
        return x-41;
    }

    return inner(42);
}

// Test 2: Multiple inner functions
int test_multiple() {
    int first(int x) {
        printf("first: %d\n", x);
        return x+2;
    }

    int second(int x) {
        printf("second: %d\n", x);
        return x+3;
    }

    int x = first(1);
    x += second(2);
    return x;
}

// Test 3: typeof(outer_var) should work
int test_typeof() {
    int outer_var = 100;

    int inner(typeof(outer_var) x) {
        printf("typeof works: %d\n", x);
        return x;
    }

    return inner(outer_var);
}

// Test 4: Recursive inner function
void test_recursive() {
    void countdown(int n) {
        if(n > 0) {
            printf("countdown: %d\n", n);
            countdown(n - 1);
        }
    }

    countdown(3);
}

// Test 5: Nested inner functions
int test_nested() {
    int level1(int x) {
        int level2(int y) {
            printf("%d: nested: %d\n", __LINE__, y);
            return y;
        }
        return level2(x * 3);
    }
    {
        int level1(int x) {
            int level2(int y) {
                printf("%d: nested: %d\n", __LINE__, y);
                return y;
            }
            return level2(x * 2);
        }
        level1(5);
    }

    return level1(5);
}

int test_enum(){
    enum {BAR = 3};
    int nested(){
        return BAR;
    }
    return nested();
}

int main() {
    printf("=== Test basic ===\n");
    if(test_basic() != 1) return __LINE__;

    printf("=== Test multiple ===\n");
    if(test_multiple() != 8) return __LINE__;

    printf("=== Test typeof ===\n");
    if(test_typeof() != 100) return __LINE__;

    printf("=== Test recursive ===\n");
    test_recursive();

    printf("=== Test nested ===\n");
    if(test_nested() != 15) return __LINE__;

    if(test_enum() != 3) return __LINE__;

    return 0;
}
