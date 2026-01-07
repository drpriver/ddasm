#ifndef ABI_TEST_H
#define ABI_TEST_H

#include <stdint.h>

// ============================================================================
// Small structs (fit in registers)
// ============================================================================

// 8 bytes - fits in one register
typedef struct {
    int32_t a, b;
} IntPair;

// 8 bytes - two floats, fits in one XMM register
typedef struct {
    float x, y;
} Vec2;

// 12 bytes - three floats, fits in two XMM registers
typedef struct {
    float x, y, z;
} Vec3;

typedef struct {
    int x, y, z;
} Vec3i;

// 16 bytes - four floats, fits in two XMM registers
typedef struct {
    float x, y, z, w;
} Vec4;

// 8 bytes - mixed int and float
typedef struct {
    int32_t id;
    float value;
} IntFloat;

// 16 bytes - mixed types
typedef struct {
    float x, y;
    int32_t id;
    int32_t flags;
} MixedSmall;

// 4 bytes - packed color
typedef struct {
    uint8_t r, g, b, a;
} Color;

// ============================================================================
// Large structs (passed by hidden pointer)
// ============================================================================

// 32 bytes
typedef struct {
    float m[8];
} Mat2x4;

// 64 bytes
typedef struct {
    float m[16];
} Mat4x4;

// 24 bytes - just over the 16-byte threshold
typedef struct {
    double x, y, z;
} Vec3d;

// 40 bytes
typedef struct {
    int32_t id;
    char name[32];
    float value;
} LargeMixed;

// ============================================================================
// Test functions - Pass by value
// ============================================================================

// Small struct pass
Vec2 vec2_id(Vec2 v);
int32_t sum_int_pair(IntPair p);
float vec2_length_sq(Vec2 v);
float vec3_length_sq(Vec3 v);
float vec4_dot(Vec4 a, Vec4 b);
float int_float_sum(IntFloat p);
float mixed_small_sum(MixedSmall m);
int32_t color_to_int(Color c);

// Multiple small struct args
float vec2_dot(Vec2 a, Vec2 b);
float vec3_dot(Vec3 a, Vec3 b);
Vec3 vec3_cross(Vec3 a, Vec3 b);

// Mixed scalar and struct args
float vec2_scale_sum(Vec2 v, float scale);
float vec3_scale_sum(Vec3 v, float scale);
int32_t int_pair_weighted(IntPair p, int32_t weight);

// Large struct pass
float mat2x4_sum(Mat2x4 m);
float mat4x4_trace(Mat4x4 m);
double vec3d_length_sq(Vec3d v);
int32_t large_mixed_hash(LargeMixed m);

// ============================================================================
// Test functions - Return by value
// ============================================================================

// Small struct return
IntPair make_int_pair(int32_t a, int32_t b);
Vec2 make_vec2(float x, float y);
Vec3 make_vec3(float x, float y, float z);
Vec4 make_vec4(float x, float y, float z, float w);
IntFloat make_int_float(int32_t id, float value);
MixedSmall make_mixed_small(float x, float y, int32_t id, int32_t flags);
Color make_color(uint8_t r, uint8_t g, uint8_t b, uint8_t a);

// Large struct return
Mat2x4 make_mat2x4_identity(void);
Mat4x4 make_mat4x4_identity(void);
Vec3d make_vec3d(double x, double y, double z);
LargeMixed make_large_mixed(int32_t id, const char* name, float value);

// Operations that return structs
Vec2 vec2_add(Vec2 a, Vec2 b);
Vec3 vec3_add(Vec3 a, Vec3 b);
Vec4 vec4_add(Vec4 a, Vec4 b);
Vec2 vec2_scale(Vec2 v, float s);
Vec3 vec3_scale(Vec3 v, float s);
Mat4x4 mat4x4_multiply(Mat4x4 a, Mat4x4 b);

// ============================================================================
// Complex scenarios
// ============================================================================

// Chain of struct operations
Vec3 vec3_normalize(Vec3 v);
float process_chain(Vec3 start, float scale);

// Callback with struct args/returns (if supported)
typedef Vec2 (*Vec2Transform)(Vec2 v);
Vec2 apply_transform(Vec2 v, Vec2Transform fn);

_Bool IsKeyDown(int);

// Lots of args
Vec3 vec3_4_max(Vec3 a, Vec3 b, Vec3 c, Vec3 d);
Vec3d vec3d_4_max(Vec3d a, Vec3d b, Vec3d c, Vec3d d);
Vec3i vec3i_4_max(Vec3i a, Vec3i b, Vec3i c, Vec3i d);
Vec3i vec3i_4_max_unpacked(int ax, int ay, int az, int bx, int by, int bz, int cx, int cy, int cz, int dx, int dy, int dz);

#endif // ABI_TEST_H
