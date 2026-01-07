// ABI Test - Tests native calling convention for structs
// This file is compiled by ddasm and calls into the native abi_test library

#pragma library("libc")
int printf(const char*, ...);
#ifdef __DDASM__
void abort(void) { __dasm{ dump; abort; } }
#else
void abort(void);
#endif

#pragma library("abi_test")
#include "abi_test.h"

// ============================================================================
// Test helpers
// ============================================================================

int tests_run = 0;
int tests_passed = 0;

int float_eq(float a, float b) {
    return a == b;
    float diff = a - b;
    if (diff < 0.0f) diff = 0.0f - diff;  // abs
    int result = diff < 0.01f;
    return result;
}
int double_eq(double a, double b) {
    return a == b;
    double diff = a - b;
    if (diff < 0.0) diff = 0.0 - diff;  // abs
    int result = diff < 0.01;
    return result;
}

void inc_tests_run(void) {
    tests_run = tests_run + 1;
}
void inc_tests_passed(void) {
    tests_passed = tests_passed + 1;
}

#define TEST(name) do {\
    inc_tests_run(); \
    printf("  %-40s ", name); \
}while(0)

#define PASS() do {\
    inc_tests_passed(); \
    printf("[PASS]\n"); \
}while(0)

#define FAIL(fmt, ...) \
    printf("[FAIL] " fmt "\n" __VA_OPT__(,) __VA_ARGS__)

#define CHECK_INT(expected, actual) do {\
    if ((expected) == (actual)) { PASS(); } \
    else { FAIL("expected %d, got %d", (expected), (actual));  abort(); } \
}while(0)

#define CHECK_FLOAT(expected, actual)  do {\
    if (float_eq((expected), (actual))) { PASS(); } \
    else { FAIL("expected %f, got %f", (double)(expected), (double)(actual)); abort(); } \
}while(0)

#define CHECK_DOUBLE(expected, actual) do { \
    if (double_eq((expected), (actual))) { PASS(); } \
    else { FAIL("expected %f, got %f", (expected), (actual));  abort(); } \
}while(0)

// ============================================================================
// Tests
// ============================================================================

void test_basic_types(void){
    printf("\n=== Basic Types ===\n");
    {
        TEST("Bool return true");
        _Bool b = IsKeyDown(87);
        CHECK_INT(1, b);
        TEST("Bool return false");
        b = IsKeyDown(2);
        CHECK_INT(0, b);
        TEST("Bool return false in if");
        if(IsKeyDown(2))
            FAIL("key is down");
        else PASS();
        TEST("Bool return true in if");
        if(IsKeyDown(87))
            PASS();
        else FAIL("Key is not down");
    }
}

void test_small_struct_pass(void) {
    printf("\n=== Small Struct Pass by Value ===\n");

    // IntPair (8 bytes, integer register)
    {
        TEST("IntPair pass");
        IntPair p = {10, 20};
        int result = sum_int_pair(p);
        CHECK_INT(30, result);
    }
    // Vec2 (8 bytes, XMM register)
    {
        TEST("Vec2 id");
        Vec2 v = {3.0f, 4.0f};
        Vec2 v2 = vec2_id(v);
        if(v.x != v2.x)
            abort();
        if(v.y != v2.y)
            abort();
        PASS();
    }

    // Vec2 (8 bytes, XMM register)
    {
        TEST("Vec2 pass");
        Vec2 v = {3.0f, 4.0f};
        float result = vec2_length_sq(v);
        CHECK_FLOAT(25.0f, result);
    }

    // Vec3 (12 bytes, two XMM registers)
    {
        TEST("Vec3 pass");
        Vec3 v = {1.0f, 2.0f, 2.0f};
        float result = vec3_length_sq(v);
        CHECK_FLOAT(9.0f, result);
    }

    // Vec4 (16 bytes, two XMM registers)
    {
        TEST("Vec4 dot product");
        Vec4 a = {1.0f, 2.0f, 3.0f, 4.0f};
        Vec4 b = {2.0f, 3.0f, 4.0f, 5.0f};
        float result = vec4_dot(a, b);
        CHECK_FLOAT(40.0f, result);  // 2+6+12+20
    }

    // IntFloat (8 bytes, mixed)
    {
        TEST("IntFloat pass");
        IntFloat p = {10, 5.5f};
        float result = int_float_sum(p);
        CHECK_FLOAT(15.5f, result);
    }

    // MixedSmall (16 bytes, mixed)
    {
        TEST("MixedSmall pass");
        MixedSmall m = {1.0f, 2.0f, 3, 4};
        float result = mixed_small_sum(m);
        CHECK_FLOAT(10.0f, result);
    }

    // Color (4 bytes)
    {
        TEST("Color pass");
        Color c = {0xFF, 0x80, 0x40, 0x20};
        int result = color_to_int(c);
        CHECK_INT(0xFF804020, result);
    }
}

void test_multiple_struct_args(void) {
    printf("\n=== Multiple Struct Arguments ===\n");

    // Vec2 dot
    {
        TEST("Vec2 dot (two struct args)");
        Vec2 a = {1.0f, 2.0f};
        Vec2 b = {3.0f, 4.0f};
        float result = vec2_dot(a, b);
        CHECK_FLOAT(11.0f, result);  // 3+8
    }

    // Vec3 dot
    {
        TEST("Vec3 dot (two struct args)");
        Vec3 a = {1.0f, 2.0f, 3.0f};
        Vec3 b = {4.0f, 5.0f, 6.0f};
        float result = vec3_dot(a, b);
        CHECK_FLOAT(32.0f, result);  // 4+10+18
    }

    // Vec3 cross
    {
        TEST("Vec3 cross (returns struct)");
        Vec3 a = {1.0f, 0.0f, 0.0f};
        Vec3 b = {0.0f, 1.0f, 0.0f};
        Vec3 result = vec3_cross(a, b);
        CHECK_FLOAT(0.0f, result.x);
        // Additional checks would go here
    }
}

void test_mixed_scalar_struct(void) {
    printf("\n=== Mixed Scalar and Struct Args ===\n");

    {
        TEST("Vec2 + scalar");
        Vec2 v = {3.0f, 4.0f};
        float result = vec2_scale_sum(v, 2.0f);
        CHECK_FLOAT(14.0f, result);  // (3+4)*2
    }

    {
        TEST("Vec3 + scalar");
        Vec3 v = {1.0f, 2.0f, 3.0f};
        float result = vec3_scale_sum(v, 3.0f);
        CHECK_FLOAT(18.0f, result);  // (1+2+3)*3
    }

    {
        TEST("IntPair + int scalar");
        IntPair p = {5, 7};
        int result = int_pair_weighted(p, 3);
        CHECK_INT(36, result);  // (5+7)*3
    }
}

void test_large_struct_pass(void) {
    printf("\n=== Large Struct Pass by Value ===\n");

    // Mat2x4 (32 bytes)
    {
        TEST("Mat2x4 pass (32 bytes)");
        Mat2x4 m;
        m.m[0] = 1.0f; m.m[1] = 2.0f; m.m[2] = 3.0f; m.m[3] = 4.0f;
        m.m[4] = 5.0f; m.m[5] = 6.0f; m.m[6] = 7.0f; m.m[7] = 8.0f;
        float result = mat2x4_sum(m);
        CHECK_FLOAT(36.0f, result);
    }

    // Mat4x4 (64 bytes)
    {
        TEST("Mat4x4 pass (64 bytes)");
        Mat4x4 m;
        int i;
        for (i = 0; i < 16; i++) m.m[i] = 0.0f;
        m.m[0] = 1.0f; m.m[5] = 2.0f; m.m[10] = 3.0f; m.m[15] = 4.0f;
        float result = mat4x4_trace(m);
        CHECK_FLOAT(10.0f, result);
    }

    // Vec3d (24 bytes - doubles)
    {
        TEST("Vec3d pass (24 bytes, doubles)");
        Vec3d v = {3.0, 4.0, 0.0};
        double result = vec3d_length_sq(v);
        CHECK_DOUBLE(25.0, result);
    }
}

void test_small_struct_return(void) {
    printf("\n=== Small Struct Return by Value ===\n");

    // IntPair
    {
        TEST("IntPair return");
        IntPair p = make_int_pair(42, 58);
        if (p.a == 42 && p.b == 58) { PASS(); }
        else { FAIL("expected {42,58}, got {%d,%d}", p.a, p.b); }
    }

    // Vec2
    {
        TEST("Vec2 return");
        Vec2 v = make_vec2(1.5f, 2.5f);
        if (float_eq(v.x, 1.5f) && float_eq(v.y, 2.5f)) { PASS(); }
        else { FAIL("expected {1.5,2.5}, got {%f,%f}", (double)v.x, (double)v.y); }
    }

    // Vec3
    {
        TEST("Vec3 return");
        Vec3 v = make_vec3(1.0f, 2.0f, 3.0f);
        if (float_eq(v.x, 1.0f) && float_eq(v.y, 2.0f) && float_eq(v.z, 3.0f)) { PASS(); }
        else { FAIL("expected {1,2,3}, got {%f,%f,%f}", (double)v.x, (double)v.y, (double)v.z); }
    }

    // Vec4
    {
        TEST("Vec4 return");
        Vec4 v = make_vec4(1.0f, 2.0f, 3.0f, 4.0f);
        if (float_eq(v.x, 1.0f) && float_eq(v.y, 2.0f) && float_eq(v.z, 3.0f) && float_eq(v.w, 4.0f)) { PASS(); }
        else { FAIL("got {%f,%f,%f,%f}", (double)v.x, (double)v.y, (double)v.z, (double)v.w); }
    }

    // IntFloat
    {
        TEST("IntFloat return");
        IntFloat p = make_int_float(123, 4.5f);
        if (p.id == 123 && float_eq(p.value, 4.5f)) { PASS(); }
        else { FAIL("expected {123,4.5}, got {%d,%f}", p.id, (double)p.value); }
    }

    // MixedSmall
    {
        TEST("MixedSmall return");
        MixedSmall m = make_mixed_small(1.0f, 2.0f, 3, 4);
        if (float_eq(m.x, 1.0f) && float_eq(m.y, 2.0f) && m.id == 3 && m.flags == 4) { PASS(); }
        else { FAIL("got {%f,%f,%d,%d}", (double)m.x, (double)m.y, m.id, m.flags); }
    }

    // Color
    {
        TEST("Color return");
        Color c = make_color(255, 128, 64, 32);
        if (c.r == 255 && c.g == 128 && c.b == 64 && c.a == 32) { PASS(); }
        else { FAIL("got {%d,%d,%d,%d}", c.r, c.g, c.b, c.a); }
    }
}

void test_large_struct_return(void) {
    printf("\n=== Large Struct Return by Value ===\n");

    // Mat2x4 (32 bytes)
    {
        TEST("Mat2x4 return (32 bytes)");
        Mat2x4 m = make_mat2x4_identity();
        if (float_eq(m.m[0], 1.0f) && float_eq(m.m[3], 1.0f) && float_eq(m.m[1], 0.0f)) { PASS(); }
        else { FAIL("identity matrix not correct"); }
    }

    // Mat4x4 (64 bytes)
    {
        TEST("Mat4x4 return (64 bytes)");
        Mat4x4 m = make_mat4x4_identity();
        if (float_eq(m.m[0], 1.0f) && float_eq(m.m[5], 1.0f) &&
            float_eq(m.m[10], 1.0f) && float_eq(m.m[15], 1.0f) &&
            float_eq(m.m[1], 0.0f)) { PASS(); }
        else { FAIL("identity matrix not correct"); }
    }

    // Vec3d (24 bytes)
    {
        TEST("Vec3d return (24 bytes)");
        Vec3d v = make_vec3d(1.5, 2.5, 3.5);
        if (double_eq(v.x, 1.5) && double_eq(v.y, 2.5) && double_eq(v.z, 3.5)) { PASS(); }
        else { FAIL("got {%f,%f,%f}", v.x, v.y, v.z); }
    }
}

void test_struct_operations(void) {
    printf("\n=== Struct Operations (pass and return) ===\n");

    // Vec2 add
    {
        TEST("Vec2 add");
        Vec2 a = {1.0f, 2.0f};
        Vec2 b = {3.0f, 4.0f};
        Vec2 r = vec2_add(a, b);
        if (float_eq(r.x, 4.0f) && float_eq(r.y, 6.0f)) { PASS(); }
        else { FAIL("got {%f,%f}", (double)r.x, (double)r.y); }
    }

    // Vec3 add
    {
        TEST("Vec3 add");
        Vec3 a = {1.0f, 2.0f, 3.0f};
        Vec3 b = {4.0f, 5.0f, 6.0f};
        Vec3 r = vec3_add(a, b);
        if (float_eq(r.x, 5.0f) && float_eq(r.y, 7.0f) && float_eq(r.z, 9.0f)) { PASS(); }
        else { FAIL("got {%f,%f,%f}", (double)r.x, (double)r.y, (double)r.z); }
    }

    // Vec4 add
    {
        TEST("Vec4 add");
        Vec4 a = {1.0f, 2.0f, 3.0f, 4.0f};
        Vec4 b = {5.0f, 6.0f, 7.0f, 8.0f};
        Vec4 r = vec4_add(a, b);
        if (float_eq(r.x, 6.0f) && float_eq(r.y, 8.0f) && float_eq(r.z, 10.0f) && float_eq(r.w, 12.0f)) { PASS(); }
        else { FAIL("got {%f,%f,%f,%f}", (double)r.x, (double)r.y, (double)r.z, (double)r.w); }
    }

    // Vec2 scale
    {
        TEST("Vec2 scale");
        Vec2 v = {2.0f, 3.0f};
        Vec2 r = vec2_scale(v, 2.0f);
        if (float_eq(r.x, 4.0f) && float_eq(r.y, 6.0f)) { PASS(); }
        else { FAIL("got {%f,%f}", (double)r.x, (double)r.y); }
    }

    // Vec3 scale
    {
        TEST("Vec3 scale");
        Vec3 v = {1.0f, 2.0f, 3.0f};
        Vec3 r = vec3_scale(v, 2.0f);
        if (float_eq(r.x, 2.0f) && float_eq(r.y, 4.0f) && float_eq(r.z, 6.0f)) { PASS(); }
        else { FAIL("got {%f,%f,%f}", (double)r.x, (double)r.y, (double)r.z); }
    }
}

void test_complex_scenarios(void) {
    printf("\n=== Complex Scenarios ===\n");

    // Vec3 normalize
    {
        TEST("Vec3 normalize");
        Vec3 v = {3.0f, 0.0f, 4.0f};  // length = 5
        Vec3 n = vec3_normalize(v);
        if (float_eq(n.x, 0.6f) && float_eq(n.y, 0.0f) && float_eq(n.z, 0.8f)) { PASS(); }
        else { FAIL("got {%f,%f,%f}", (double)n.x, (double)n.y, (double)n.z); }
    }

    // Process chain
    {
        TEST("Process chain (multiple calls)");
        Vec3 start = {3.0f, 0.0f, 4.0f};
        float result = process_chain(start, 2.0f);
        // scaled = {6,0,8}, normalized = {0.6,0,0.8}, length_sq = 1.0
        CHECK_FLOAT(1.0f, result);
    }
}

void test_args(void){
    {
        TEST("Passing a lot of args (float)");
        Vec3 a = {1, 2, 3};
        Vec3 b = {6, 5, -4};
        Vec3 c = {12, 0, 1};
        Vec3 d = {0, 0, 0};
        Vec3 e = vec3_4_max(a, b, c, d);
        if(float_eq(e.x, 12.f) && float_eq(e.y, 5.f) && float_eq(e.z, 3.f)){
            PASS();
        }
        else {
            FAIL("got %f,%f,%f\n", (double)e.x, (double)e.y, (double)e.z);
        }
    }
    {
        TEST("Passing a lot of args (double)");
        Vec3d a = {1, 2, 3};
        Vec3d b = {6, 5, -4};
        Vec3d c = {12, 0, 1};
        Vec3d d = {0, 0, 0};
        Vec3d e = vec3d_4_max(a, b, c, d);
        if(double_eq(e.x, 12.) && double_eq(e.y, 5.) && double_eq(e.z, 3.)){
            PASS();
        }
        else {
            FAIL("got %f,%f,%f\n", e.x, e.y, e.z);
        }
    }
    {
        TEST("Passing a lot of args (int)");
        Vec3i a = {1, 2, 3};
        Vec3i b = {6, 5, -4};
        Vec3i c = {12, 0, 1};
        Vec3i d = {0, 0, 0};
        Vec3i e = vec3i_4_max(a, b, c, d);
        if(e.x== 12 && e.y==5 && e.z== 3){
            PASS();
        }
        else {
            FAIL("got %d,%d,%d\n", e.x, e.y, e.z);
        }
    }
    {
        TEST("Passing a lot of args (int, unpacked)");
        Vec3i a = {1, 2, 3};
        Vec3i b = {6, 5, -4};
        Vec3i c = {12, 0, 1};
        Vec3i d = {0, 0, 0};
        Vec3i e = vec3i_4_max_unpacked(a.x, a.y, a.z, b.x, b.y, b.z, c.x, c.y, c.z, d.x, d.y, d.z);
        if(e.x== 12 && e.y==5 && e.z== 3){
            PASS();
        }
        else {
            FAIL("got %d,%d,%d\n", e.x, e.y, e.z);
        }
    }
}

// ============================================================================
// Main
// ============================================================================

int main(void) {
    printf("ABI Test Suite\n");
    printf("==============\n");

    test_basic_types();
    test_small_struct_pass();
    test_multiple_struct_args();
    test_mixed_scalar_struct();
    test_large_struct_pass();
    test_small_struct_return();
    test_large_struct_return();
    test_struct_operations();
    test_complex_scenarios();
    test_args();

    printf("\n==============\n");
    printf("Results: %d/%d tests passed\n", tests_passed, tests_run);

    if (tests_passed != tests_run) {
        abort();
    }

    return 0;
}
