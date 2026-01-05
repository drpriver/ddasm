#pragma library("libc")
#include <stdio.h>
#include <stdlib.h>

// Test 1: Basic UFCS
int double_it(int x) {
    return x * 2;
}

void test_basic() {
    int x = 21;
    int result = x.double_it();
    if (result != 42) {
        printf("FAIL %s: expected 42, got %d\n", __func__, result);
        exit(1);
    }
    result = (33).double_it();
    if (result != 66) {
        printf("FAIL %s: expected 42, got %d\n", __func__, result);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 2: UFCS with additional args
int add(int x, int y) {
    return x + y;
}

void test_with_args() {
    int x = 10;
    int result = x.add(32);
    if (result != 42) {
        printf("FAIL %s: expected 42, got %d\n", __func__, result);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 3: Auto-ref (function takes pointer)
void increment(int* p) {
    (*p)++;
}

void test_autoref() {
    int x = 41;
    x.increment();
    if (x != 42) {
        printf("FAIL %s: expected 42, got %d\n", __func__, x);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 4: UFCS with struct
struct Point { int x; int y; };

int point_sum(struct Point p) {
    return p.x + p.y;
}

void test_struct() {
    struct Point p = {20, 22};
    int result = p.point_sum();
    if (result != 42) {
        printf("FAIL %s: expected 42, got %d\n", __func__, result);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 5: Auto-ref with struct pointer
void point_scale(struct Point* p, int factor) {
    p->x *= factor;
    p->y *= factor;
}

void test_struct_autoref() {
    struct Point p = {3, 4};
    p.point_scale(2);
    if (p.x != 6 || p.y != 8) {
        printf("FAIL %s: expected (6, 8), got (%d, %d)\n", __func__, p.x, p.y);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

// Test 6: Chained UFCS
int triple(int x) {
    return x * 3;
}

void test_chained() {
    int x = 7;
    int result = x.double_it().triple();
    if (result != 42) {
        printf("FAIL %s: expected 42, got %d\n", __func__, result);
        exit(1);
    }
    printf("PASS %s\n", __func__);
}

void readme_example(){

    int double_it(int x){
        return x * 2;
    }
    int x = 3;
    int y = x.double_it();
    // literal requires parens or else it gets parsed as a double.
    int z = (3).double_it();
    if(y != 6) {
        printf("%s:%d FAIL: %s: expected 6, got %d\n", __FILE__, __LINE__, __func__, y);
        exit(1);
    }
    if(z != 6) {
        printf("%s:%d FAIL: %s: expected 6, got %d\n", __FILE__, __LINE__, __func__, y);
        exit(1);
    }

    struct DynamicArray {
        int* data;
        size_t count, capacity;
    };
    void oom(){
        exit(1);
    }
    void da_append(struct DynamicArray* da, int v){
        // Test (*ptr).member syntax - should work now
        size_t c = (*da).count;
        (void)c;
        if(da.count >= da.capacity){
            size_t capacity = da.capacity?da.capacity*2:1;
            void* data = realloc(da.data, da.capacity * sizeof *da.data);
            if(!data) oom();
            da.data = data;
            da.capacity = capacity;
        }
        da.data[da.count++] = v;
    }

    struct DynamicArray da = {0};
    da.da_append(1);
    // pointer works too
    (&da).da_append(2);
    if(da.capacity != 2){
        printf("%s:%d FAIL: %s: expected 2, got %zu\n", __FILE__, __LINE__, __func__, da.capacity);
        exit(1);
    }
    if(da.count != 2){
        printf("%s:%d FAIL: %s: expected 2, got %zu\n", __FILE__, __LINE__, __func__, da.count);
        exit(1);
    }
    for(size_t i = 0; i < da.count; i++){
        printf("%zu] %d\n", i, da.data[i]);
        if(da.data[i] != i + 1){
            printf("%s:%d FAIL: %s: expected %zu, got %d\n", __FILE__, __LINE__, __func__, i+1, da.data[i]);
            exit(1);
        }
    }
    free(da.data);
}

int main() {
    test_basic();
    test_with_args();
    test_autoref();
    test_struct();
    test_struct_autoref();
    test_chained();
    readme_example();
    printf("All UFCS tests passed!\n");
    return 0;
}
