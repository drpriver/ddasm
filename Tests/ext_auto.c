#pragma library("libc")
#include <stdio.h>
#include <stdlib.h>

// Test 1: Basic auto inference
void test_auto_int() {
    auto x = 42;
    if (x != 42) {
        printf("FAIL %s: expected 42, got %d\n", __func__, x);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 2: auto with pointer
void test_auto_pointer() {
    int y = 100;
    auto p = &y;
    if (*p != 100) {
        printf("FAIL %s: expected 100, got %d\n", __func__, *p);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 3: auto with string literal
void test_auto_string() {
    auto s = "hello";
    if (s[0] != 'h') {
        printf("FAIL %s: expected 'h', got '%c'\n", __func__, s[0]);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 4: __auto_type (GCC extension)
void test_auto_type() {
    __auto_type x = 123;
    __auto_type y = 3.14;
    if (x != 123) {
        printf("FAIL %s: expected 123, got %d\n", __func__, x);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 5: const inference extension
void test_const_inference() {
    const msg = "world";
    if (msg[0] != 'w') {
        printf("FAIL %s: expected 'w', got '%c'\n", __func__, msg[0]);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 6: auto with expression
void test_auto_expression() {
    int a = 10, b = 20;
    auto sum = a + b;
    if (sum != 30) {
        printf("FAIL %s: expected 30, got %d\n", __func__, sum);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 7: old-style auto (storage class with explicit type)
void test_old_auto() {
    auto int x = 99;
    if (x != 99) {
        printf("FAIL %s: expected 99, got %d\n", __func__, x);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 8: auto with function return value
int get_value() { return 777; }

void test_auto_func_return() {
    auto result = get_value();
    if (result != 777) {
        printf("FAIL %s: expected 777, got %d\n", __func__, result);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 9: const inference with int
void test_const_int() {
    const val = 42;
    if (val != 42) {
        printf("FAIL %s: expected 42, got %d\n", __func__, val);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 10: static auto
void test_static_auto() {
    static auto counter = 0;
    counter++;
    if (counter < 1) {
        printf("FAIL %s: counter should be >= 1\n", __func__);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 11: const auto
void test_const_auto() {
    const auto x = 123;
    if (x != 123) {
        printf("FAIL %s: expected 123, got %d\n", __func__, x);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 12: static const auto
void test_static_const_auto() {
    static const auto val = 999;
    if (val != 999) {
        printf("FAIL %s: expected 999, got %d\n", __func__, val);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

int main() {
    test_auto_int();
    test_auto_pointer();
    test_auto_string();
    test_auto_type();
    test_const_inference();
    test_auto_expression();
    test_old_auto();
    test_auto_func_return();
    test_const_int();
    test_static_auto();
    test_const_auto();
    test_static_const_auto();
    printf("All auto type inference tests passed!\n");
    return 0;
}
