// Test (6.7.11) designated initializers per C2y spec
// EXPECT: 858

struct Point {
    int x;
    int y;
};

int test_designated_struct(void) {
    struct Point p = {.y = 20, .x = 10};
    return p.x + p.y;  // 30
}

int test_designated_array(void) {
    int arr[10] = {[5] = 50, [9] = 90};
    return arr[5] + arr[9];  // 140
}

int test_mixed_init(void) {
    int arr[5] = {1, 2, [4] = 5};
    return arr[0] + arr[1] + arr[4];  // 8
}

int test_empty_init(void) {
    struct Point p = {};
    return p.x + p.y;  // 0
}

int test_positional_after_designated(void) {
    // After [2] = 100, next positional continues at index 3
    int arr[5] = {[2] = 100, 101, 102};
    return arr[2] + arr[3] + arr[4];  // 303
}

// Large struct - 24 bytes (> 8 bytes)
struct Large {
    int a;
    int b;
    int c;
    int d;
    int e;
    int f;
};

int test_large_struct(void) {
    // Initialize large struct with designators
    struct Large s = {.f = 60, .c = 30, .a = 10};
    return s.a + s.b + s.c + s.d + s.e + s.f;  // 10 + 0 + 30 + 0 + 0 + 60 = 100
}

// Struct containing a struct - total 12 bytes
struct Outer {
    struct Point p;
    int z;
};

int test_nested_struct(void) {
    // Initialize with nested struct init list (positional)
    struct Outer o = {{1, 2}, 3};
    return o.p.x + o.p.y + o.z;  // 1 + 2 + 3 = 6
}

int test_nested_struct_designated(void) {
    // Using designator for nested struct field
    struct Outer o = {.p = {10, 20}, .z = 30};
    return o.p.x + o.p.y + o.z;  // 10 + 20 + 30 = 60
}

// Struct with long fields - 32 bytes total (8 bytes per field)
struct LargeLong {
    long a;
    long b;
    long c;
    long d;
};

int test_large_long_struct(void) {
    struct LargeLong s = {.d = 40, .b = 20, .a = 10};
    return (int)(s.a + s.b + s.c + s.d);  // 10 + 20 + 0 + 40 = 70
}

// Test initializing with struct values > 8 bytes (memcpy needed)
struct Big {
    long a;
    long b;
};  // 16 bytes

struct Bigger {
    struct Big x;
    struct Big y;
};  // 32 bytes

int test_big_struct_copy(void) {
    struct Big b = {1, 2};
    struct Bigger bb = {b, b};  // Copy 16-byte struct twice
    return (int)(bb.x.a + bb.x.b + bb.y.a + bb.y.b);  // 1 + 2 + 1 + 2 = 6
}

// Test 6-byte struct (needs memcpy, not write8)
struct S6 {
    short a, b, c;  // 6 bytes - no matching write instruction
};

struct OuterS6 {
    struct S6 inner;  // 6 bytes at offset 0
    short z;          // at offset 6
};  // total 8 bytes

int test_6byte_struct(void) {
    struct S6 s = {1, 2, 3};
    struct OuterS6 o = {{0,0,0}, 99};
    o.inner = s;  // Must use memcpy, not write (which would corrupt z)
    return (o.inner.a + o.inner.b + o.inner.c + o.z);  // 1+2+3+99 = 105
}

int test_struct_copy_init(void) {
    struct Point p = {10, 20};
    struct Point p2 = p;  // Initialize from another struct (not init list)
    return p2.x + p2.y;  // 10 + 20 = 30
}

// TODO: Array of structs needs struct array subscript support
// struct Point pts[3] = {{1, 2}, {3, 4}, {5, 6}};  // Not yet supported

int main(void) {
    int result = 0;
    result += test_designated_struct();          // 30
    result += test_designated_array();           // 140
    result += test_mixed_init();                 // 8
    result += test_empty_init();                 // 0
    result += test_positional_after_designated();// 303
    result += test_large_struct();               // 100
    result += test_nested_struct();              // 6
    result += test_nested_struct_designated();   // 60
    result += test_large_long_struct();          // 70
    result += test_big_struct_copy();            // 6
    result += test_6byte_struct();               // 105
    result += test_struct_copy_init();           // 30
    return result;  // Expected: 828 + 30 = 858
}
