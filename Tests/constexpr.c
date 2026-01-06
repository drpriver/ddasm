#pragma library("libc")
#include <stdio.h>
#include <stdlib.h>

// Test 1: Basic constexpr inference
void test_constexpr_int() {
    constexpr x = 42;
    if (x != 42) {
        printf("FAIL %s: expected 42, got %d\n", __func__, x);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 2: constexpr with explicit type
void test_constexpr_explicit() {
    constexpr int x = 100;
    if (x != 100) {
        printf("FAIL %s: expected 100, got %d\n", __func__, x);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 3: constexpr with pointer
void test_constexpr_pointer() {
    int y = 50;
    constexpr p = &y;
    if (*p != 50) {
        printf("FAIL %s: expected 50, got %d\n", __func__, *p);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 4: constexpr with string literal
void test_constexpr_string() {
    constexpr s = "hello";
    if (s[0] != 'h') {
        printf("FAIL %s: expected 'h', got '%c'\n", __func__, s[0]);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 5: static constexpr
void test_static_constexpr() {
    static constexpr x = 999;
    if (x != 999) {
        printf("FAIL %s: expected 999, got %d\n", __func__, x);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 6: constexpr static (reversed order)
void test_constexpr_static() {
    constexpr static y = 888;
    if (y != 888) {
        printf("FAIL %s: expected 888, got %d\n", __func__, y);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 7: constexpr with expression
void test_constexpr_expr() {
    constexpr a = 10;
    constexpr b = 20;
    constexpr sum = a + b;
    if (sum != 30) {
        printf("FAIL %s: expected 30, got %d\n", __func__, sum);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 8: constexpr long
void test_constexpr_long() {
    constexpr long x = 1000000000L;
    if (x != 1000000000L) {
        printf("FAIL %s: expected 1000000000, got %ld\n", __func__, x);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 9: constexpr char
void test_constexpr_char() {
    constexpr char c = 'A';
    if (c != 'A') {
        printf("FAIL %s: expected 'A', got '%c'\n", __func__, c);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 10: constexpr unsigned
void test_constexpr_unsigned() {
    constexpr unsigned x = 400000000U;
    if (x != 400000000U) {
        printf("FAIL %s: expected 400000000, got %u\n", __func__, x);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

int main() {
    test_constexpr_int();
    test_constexpr_explicit();
    test_constexpr_pointer();
    test_constexpr_string();
    test_static_constexpr();
    test_constexpr_static();
    test_constexpr_expr();
    test_constexpr_long();
    test_constexpr_char();
    test_constexpr_unsigned();
    printf("All constexpr tests passed!\n");
    return 0;
}
