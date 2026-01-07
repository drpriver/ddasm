// Test (6.7.11) designated initializers per C2y spec
// EXPECT: 942

#ifdef __DDASM__
#define _lineno(x) #x
#define __lineno(x) _lineno(x)
#define lineno __lineno(__LINE__)
#define assert(cond) if(!(cond)) __dasm { \
    msg __FILE__":" lineno ": Failed " #cond; \
    dump; \
    abort }
#else
void abort(void);
#define assert(cond) if(!(cond)) abort()
#endif
struct Point {
    int x;
    int y;
};

void test_designated_struct(void) {
    struct Point p = {.y = 20, .x = 10};
    assert(p.x + p.y == 30);
}

void test_designated_array(void) {
    int arr[10] = {[5] = 50, [9] = 90};
    assert(arr[5] + arr[9] == 140);  // 140
}

void test_mixed_init(void) {
    int arr[5] = {1, 2, [4] = 5};
    assert(arr[0] + arr[1] + arr[4] == 8);  // 8
}

void test_empty_init(void) {
    struct Point p = {};
    assert(p.x + p.y == 0);  // 0
}

void test_positional_after_designated(void) {
    // After [2] = 100, next positional continues at index 3
    int arr[5] = {[2] = 100, 101, 102};
    assert(arr[0] + arr[1] + arr[2] + arr[3] + arr[4] == 303);  // 303
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

void test_large_struct(void) {
    // Initialize large struct with designators
    struct Large s = {.f = 60, .c = 30, .a = 10};
    assert(s.a + s.b + s.c + s.d + s.e + s.f == 100);  // 10 + 0 + 30 + 0 + 0 + 60 = 100
}

// Struct containing a struct - total 12 bytes
struct Outer {
    struct Point p;
    int z;
};

void test_nested_struct(void) {
    // Initialize with nested struct init list (positional)
    struct Outer o = {{1, 2}, 3};
    assert(o.p.x + o.p.y + o.z == 6);  // 1 + 2 + 3 = 6
}

void test_nested_struct_designated(void) {
    // Using designator for nested struct field
    struct Outer o = {.p = {10, 20}, .z = 30};
    assert(o.p.x + o.p.y + o.z == 60);  // 10 + 20 + 30 = 60
}

// Struct with long fields - 32 bytes total (8 bytes per field)
struct LargeLong {
    long a;
    long b;
    long c;
    long d;
};

void test_large_long_struct(void) {
    struct LargeLong s = {.d = 40, .b = 20, .a = 10};
    assert((int)(s.a + s.b + s.c + s.d) == 70);  // 10 + 20 + 0 + 40 = 70
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

void test_big_struct_copy(void) {
    struct Big b = {1, 2};
    struct Bigger bb = {b, b};  // Copy 16-byte struct twice
    assert(b.a == 1);
    assert(b.b == 2);
    assert(bb.x.a == 1);
    assert(bb.x.b == 2);
    assert(bb.y.a == 1);
    assert(bb.y.b == 2);
    assert((int)(bb.x.a + bb.x.b + bb.y.a + bb.y.b) == 6);  // 1 + 2 + 1 + 2 = 6
}

// Test 6-byte struct (needs memcpy, not write8)
struct S6 {
    short a, b, c;  // 6 bytes - no matching write instruction
};

struct OuterS6 {
    struct S6 inner;  // 6 bytes at offset 0
    short z;          // at offset 6
};  // total 8 bytes

void test_6byte_struct(void) {
    struct S6 s = {1, 2, 3};
    struct OuterS6 o = {{0,0,0}, 99};
    o.inner = s;  // Must use memcpy, not write (which would corrupt z)
    assert((o.inner.a + o.inner.b + o.inner.c + o.z) == 105);  // 1+2+3+99 = 105
}

void test_struct_copy_init(void) {
    struct Point p = {10, 20};
    struct Point p2 = p;  // Initialize from another struct (not init list)
    assert((p2.x + p2.y) == 30);  // 10 + 20 = 30
}

void test_chained_struct_assign(void) {
    struct Point a = {1, 2};
    struct Point b, c;
    c = b = a;  // Chained assignment
    assert(a.x + b.x + c.x + a.y + b.y + c.y == 9);  // 1+1+1+2+2+2 = 9
}

// Test chained struct designators: .field.subfield
void test_chained_struct_designator(void) {
    // Initialize nested struct fields directly
    struct Outer o = {.p.x = 5, .p.y = 15, .z = 25};
    assert(o.p.x + o.p.y + o.z == 45);  // 5 + 15 + 25 = 45
}

// Test chained array designators: [i].field
void test_chained_array_designator(void) {
    // Array of structs with chained designators
    struct Point arr[3] = {[0].x = 1, [0].y = 2, [2].x = 5, [2].y = 6};
    assert(arr[0].x == 1);
    assert(arr[0].y == 2);
    assert(arr[1].x == 0);  // uninitialized
    assert(arr[1].y == 0);
    assert(arr[2].x == 5);
    assert(arr[2].y == 6);
    assert(arr[0].x + arr[0].y + arr[1].x + arr[1].y + arr[2].x + arr[2].y == 14);
}

// Test multi-dimensional array designators
void test_multidim_array_designator(void) {
    int arr[2][3] = {[0][1] = 10, [1][2] = 20};
    assert(arr[0][0] == 0);
    assert(arr[0][1] == 10);
    assert(arr[0][2] == 0);
    assert(arr[1][0] == 0);
    assert(arr[1][1] == 0);
    assert(arr[1][2] == 20);
    assert(arr[0][1] + arr[1][2] == 30);
}

// Test global array of structs
struct Point pts[3] = {{1, 2}, {3, 4}, {5, 6}};

void test_global_array_of_structs(void) {
    assert(pts[0].x == 1);
    assert(pts[0].y == 2);
    assert(pts[1].x == 3);
    assert(pts[1].y == 4);
    assert(pts[2].x == 5);
    assert(pts[2].y == 6);
    assert(pts[0].x + pts[0].y + pts[1].x + pts[1].y + pts[2].x + pts[2].y == 21);
}

// Weird but valid: scalar with braces
void test_scalar_with_braces(void) {
    int x = {42};
    assert(x == 42);
    int y = {{{{99}}}};  // Extra braces are valid
    assert(y == 99);
}

// Weird but valid: over-braced array elements
void test_overbraced_array(void) {
    int arr[3] = {{1}, {2}, {3}};
    assert(arr[0] == 1);
    assert(arr[1] == 2);
    assert(arr[2] == 3);
}

// Weird but valid: designator overwrites previous value
void test_designator_overwrite(void) {
    int arr[3] = {[0] = 1, [0] = 99, [1] = 2};
    assert(arr[0] == 99);  // Second [0] wins
    assert(arr[1] == 2);
    assert(arr[2] == 0);
}

// Weird but valid: out of order designators
void test_out_of_order_designators(void) {
    int arr[4] = {[3] = 40, [1] = 20, [0] = 10, [2] = 30};
    assert(arr[0] == 10);
    assert(arr[1] == 20);
    assert(arr[2] == 30);
    assert(arr[3] == 40);
}

// Weird but valid: trailing comma
void test_trailing_comma(void) {
    int arr[3] = {1, 2, 3,};  // Trailing comma is valid
    assert(arr[0] + arr[1] + arr[2] == 6);
    struct Point p = {10, 20,};
    assert(p.x + p.y == 30);
}

// Weird but valid: nested empty initializers
void test_nested_empty(void) {
    struct Outer o = {{}, 5};
    assert(o.p.x == 0);
    assert(o.p.y == 0);
    assert(o.z == 5);
}

// Weird but valid: partial struct init (rest zero)
void test_partial_struct(void) {
    struct Large s = {.a = 1};  // Only a, rest are zero
    assert(s.a == 1);
    assert(s.b == 0);
    assert(s.c == 0);
    assert(s.d == 0);
    assert(s.e == 0);
    assert(s.f == 0);
}

// Weird but valid: designator then positional continues from there
void test_designator_resets_position(void) {
    // After [1] = 10, positional continues at index 2
    int arr[5] = {1, [1] = 10, 20, 30};
    assert(arr[0] == 1);
    assert(arr[1] == 10);
    assert(arr[2] == 20);
    assert(arr[3] == 30);
    assert(arr[4] == 0);
}

// Weird but valid: same field designated multiple times
void test_struct_field_overwrite(void) {
    struct Point p = {.x = 1, .y = 2, .x = 99};
    assert(p.x == 99);  // Last .x wins
    assert(p.y == 2);
}

// Weird but valid: compound literal in initializer
void test_compound_literal_init(void) {
    struct Point p = (struct Point){100, 200};
    assert(p.x == 100);
    assert(p.y == 200);
}

// Weird but valid: zero-length partial init zeros everything
void test_single_zero_init(void) {
    int arr[5] = {0};  // All elements become 0
    assert(arr[0] == 0);
    assert(arr[1] == 0);
    assert(arr[2] == 0);
    assert(arr[3] == 0);
    assert(arr[4] == 0);
}

// Weird but valid: array size from initializer
void test_array_size_from_init(void) {
    int arr[] = {1, 2, 3, 4, 5};
    assert(arr[0] == 1);
    assert(arr[4] == 5);
    // sizeof(arr) / sizeof(arr[0]) == 5
}

// Weird but valid: struct in struct with flat init
void test_flat_nested_init(void) {
    // Flat initializer fills nested struct
    struct Outer o = {1, 2, 3};  // p.x=1, p.y=2, z=3
    assert(o.p.x == 1);
    assert(o.p.y == 2);
    assert(o.z == 3);
}

void test_excess_elements(void){
    struct Point p = {1, 2, 3};
    assert(p.x == 1);
    assert(p.y == 2);
}
void test_scalar_init_aos(void){
    enum {MAX=3};
    struct Outer m[MAX] = { 0 };
    assert(m[0].p.x == 0);
    assert(m[1].p.x == 0);
    assert(m[2].p.x == 0);
}

int main(void) {
    int result = 0;
    test_designated_struct();
    test_designated_array();
    test_mixed_init();
    test_empty_init();
    test_positional_after_designated();
    test_large_struct();
    test_nested_struct();
    test_nested_struct_designated();
    test_large_long_struct();
    test_big_struct_copy();
    test_6byte_struct();
    test_struct_copy_init();
    test_chained_struct_assign();
    test_chained_struct_designator();
    test_chained_array_designator();
    test_multidim_array_designator();
    test_global_array_of_structs();
    // Weird but valid tests
    test_scalar_with_braces();
    test_overbraced_array();
    test_designator_overwrite();
    test_out_of_order_designators();
    test_trailing_comma();
    test_nested_empty();
    test_partial_struct();
    test_designator_resets_position();
    test_struct_field_overwrite();
    test_compound_literal_init();
    test_single_zero_init();
    test_array_size_from_init();
    test_flat_nested_init();
    test_excess_elements();
    test_scalar_init_aos();
    return 0;
}
