#pragma library("libc")
#include <stdio.h>
#include <stdlib.h>

// Test 1: Basic function literal assigned to function pointer
void test_basic() {
    int (*add)(int, int) = int(int a, int b) { return a + b; };
    int result = add(3, 4);
    if(result != 7) {
        printf("FAIL test_basic: expected 7, got %d\n", result);
        exit(1);
    }
    printf("PASS test_basic\n");
}

// Test 2: Function literal with void return
void test_void_return() {
    int called = 0;
    void (*greet)(int*) = void(int* p) { *p = 42; };
    greet(&called);
    if(called != 42) {
        printf("FAIL test_void_return: expected 42, got %d\n", called);
        exit(1);
    }
    printf("PASS test_void_return\n");
}

// Test 3: Function literal returning pointer
void test_pointer_return() {
    int value = 100;
    int* (*get_ptr)(int*) = int*(int* p) { return p; };
    int* result = get_ptr(&value);
    if(*result != 100) {
        printf("FAIL test_pointer_return: expected 100, got %d\n", *result);
        exit(1);
    }
    printf("PASS test_pointer_return\n");
}

// Test 4: Function literal with no parameters
void test_no_params() {
    int (*get_42)(void) = int(void) { return 42; };
    int result = get_42();
    if(result != 42) {
        printf("FAIL test_no_params: expected 42, got %d\n", result);
        exit(1);
    }
    printf("PASS test_no_params\n");
}

// Test 5: Multiple function literals
void test_multiple() {
    int (*add)(int, int) = int(int a, int b) { return a + b; };
    int (*mul)(int, int) = int(int a, int b) { return a * b; };

    int sum = add(5, 3);
    int product = mul(5, 3);

    if(sum != 8 || product != 15) {
        printf("FAIL test_multiple: expected 8 and 15, got %d and %d\n", sum, product);
        exit(1);
    }
    printf("PASS test_multiple\n");
}

// Helper for test_as_argument
int apply(int (*f)(int), int x) {
    return f(x);
}

// Test 6: Function literal passed as argument
void test_as_argument() {
    int result = apply(int(int x) { return x * 2; }, 21);
    if(result != 42) {
        printf("FAIL test_as_argument: expected 42, got %d\n", result);
        exit(1);
    }
    printf("PASS test_as_argument\n");
}

// Test 7: Typedef return type
typedef int myint;
void test_typedef_return() {
    myint (*get_val)(void) = myint(void) { return 99; };
    myint result = get_val();
    if(result != 99) {
        printf("FAIL test_typedef_return: expected 99, got %d\n", result);
        exit(1);
    }
    printf("PASS test_typedef_return\n");
}

int main() {
    test_basic();
    test_void_return();
    test_pointer_return();
    test_no_params();
    test_multiple();
    test_as_argument();
    test_typedef_return();

    printf("All function literal tests passed!\n");
    return 0;
}
