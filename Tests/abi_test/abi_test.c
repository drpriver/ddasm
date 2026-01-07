#include "abi_test.h"
#include <string.h>
#include <math.h>
#include <stdio.h>

// ============================================================================
// Small struct pass by value
// ============================================================================

int32_t sum_int_pair(IntPair p) {
    return p.a + p.b;
}

float vec2_length_sq(Vec2 v) {
    return v.x * v.x + v.y * v.y;
}
Vec2 vec2_id(Vec2 v) {
    return v;
}

float vec3_length_sq(Vec3 v) {
    return v.x * v.x + v.y * v.y + v.z * v.z;
}

float vec4_dot(Vec4 a, Vec4 b) {
    return a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w;
}

float int_float_sum(IntFloat p) {
    return (float)p.id + p.value;
}

float mixed_small_sum(MixedSmall m) {
    return m.x + m.y + (float)m.id + (float)m.flags;
}

int32_t color_to_int(Color c) {
    return (c.r << 24) | (c.g << 16) | (c.b << 8) | c.a;
}

// Multiple small struct args
float vec2_dot(Vec2 a, Vec2 b) {
    return a.x * b.x + a.y * b.y;
}

float vec3_dot(Vec3 a, Vec3 b) {
    return a.x * b.x + a.y * b.y + a.z * b.z;
}

Vec3 vec3_cross(Vec3 a, Vec3 b) {
    Vec3 result;
    result.x = a.y * b.z - a.z * b.y;
    result.y = a.z * b.x - a.x * b.z;
    result.z = a.x * b.y - a.y * b.x;
    return result;
}

// Mixed scalar and struct args
float vec2_scale_sum(Vec2 v, float scale) {
    return (v.x + v.y) * scale;
}

float vec3_scale_sum(Vec3 v, float scale) {
    return (v.x + v.y + v.z) * scale;
}

int32_t int_pair_weighted(IntPair p, int32_t weight) {
    return (p.a + p.b) * weight;
}

// Large struct pass by value
float mat2x4_sum(Mat2x4 m) {
    float sum = 0;
    for (int i = 0; i < 8; i++) {
        sum += m.m[i];
    }
    return sum;
}

float mat4x4_trace(Mat4x4 m) {
    return m.m[0] + m.m[5] + m.m[10] + m.m[15];
}

double vec3d_length_sq(Vec3d v) {
    return v.x * v.x + v.y * v.y + v.z * v.z;
}

int32_t large_mixed_hash(LargeMixed m) {
    int32_t hash = m.id;
    for (int i = 0; i < 32 && m.name[i]; i++) {
        hash = hash * 31 + m.name[i];
    }
    hash ^= (int32_t)(m.value * 1000);
    return hash;
}

// ============================================================================
// Small struct return by value
// ============================================================================

IntPair make_int_pair(int32_t a, int32_t b) {
    IntPair result;
    result.a = a;
    result.b = b;
    return result;
}

Vec2 make_vec2(float x, float y) {
    Vec2 result;
    result.x = x;
    result.y = y;
    return result;
}

Vec3 make_vec3(float x, float y, float z) {
    Vec3 result;
    result.x = x;
    result.y = y;
    result.z = z;
    return result;
}

Vec4 make_vec4(float x, float y, float z, float w) {
    Vec4 result;
    result.x = x;
    result.y = y;
    result.z = z;
    result.w = w;
    return result;
}

IntFloat make_int_float(int32_t id, float value) {
    IntFloat result;
    result.id = id;
    result.value = value;
    return result;
}

MixedSmall make_mixed_small(float x, float y, int32_t id, int32_t flags) {
    MixedSmall result;
    result.x = x;
    result.y = y;
    result.id = id;
    result.flags = flags;
    return result;
}

Color make_color(uint8_t r, uint8_t g, uint8_t b, uint8_t a) {
    Color result;
    result.r = r;
    result.g = g;
    result.b = b;
    result.a = a;
    return result;
}

// Large struct return by value
Mat2x4 make_mat2x4_identity(void) {
    Mat2x4 result = {0};
    result.m[0] = 1.0f;
    result.m[3] = 1.0f;
    return result;
}

Mat4x4 make_mat4x4_identity(void) {
    Mat4x4 result = {0};
    result.m[0] = 1.0f;
    result.m[5] = 1.0f;
    result.m[10] = 1.0f;
    result.m[15] = 1.0f;
    return result;
}

Vec3d make_vec3d(double x, double y, double z) {
    Vec3d result;
    result.x = x;
    result.y = y;
    result.z = z;
    return result;
}

LargeMixed make_large_mixed(int32_t id, const char* name, float value) {
    LargeMixed result;
    result.id = id;
    strncpy(result.name, name, 31);
    result.name[31] = '\0';
    result.value = value;
    return result;
}

// Operations that return structs
Vec2 vec2_add(Vec2 a, Vec2 b) {
    Vec2 result;
    result.x = a.x + b.x;
    result.y = a.y + b.y;
    return result;
}

Vec3 vec3_add(Vec3 a, Vec3 b) {
    Vec3 result;
    result.x = a.x + b.x;
    result.y = a.y + b.y;
    result.z = a.z + b.z;
    return result;
}

Vec4 vec4_add(Vec4 a, Vec4 b) {
    Vec4 result;
    result.x = a.x + b.x;
    result.y = a.y + b.y;
    result.z = a.z + b.z;
    result.w = a.w + b.w;
    return result;
}

Vec2 vec2_scale(Vec2 v, float s) {
    Vec2 result;
    result.x = v.x * s;
    result.y = v.y * s;
    return result;
}

Vec3 vec3_scale(Vec3 v, float s) {
    Vec3 result;
    result.x = v.x * s;
    result.y = v.y * s;
    result.z = v.z * s;
    return result;
}

Mat4x4 mat4x4_multiply(Mat4x4 a, Mat4x4 b) {
    Mat4x4 result = {0};
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            for (int k = 0; k < 4; k++) {
                result.m[i * 4 + j] += a.m[i * 4 + k] * b.m[k * 4 + j];
            }
        }
    }
    return result;
}

// ============================================================================
// Complex scenarios
// ============================================================================

Vec3 vec3_normalize(Vec3 v) {
    float len = sqrtf(v.x * v.x + v.y * v.y + v.z * v.z);
    Vec3 result;
    if (len > 0.0001f) {
        result.x = v.x / len;
        result.y = v.y / len;
        result.z = v.z / len;
    } else {
        result.x = result.y = result.z = 0.0f;
    }
    return result;
}

float process_chain(Vec3 start, float scale) {
    Vec3 scaled = vec3_scale(start, scale);
    Vec3 normalized = vec3_normalize(scaled);
    return vec3_length_sq(normalized);
}

Vec2 apply_transform(Vec2 v, Vec2Transform fn) {
    return fn(v);
}

_Bool IsKeyDown(int key){
    if(key == 87) return key;
    return 0;
    return key == 87;
}

Vec3 vec3_4_max(Vec3 a, Vec3 b, Vec3 c, Vec3 d){
    Vec3 result = a;
    if(b.x > a.x) a.x = b.x;
    if(c.x > a.x) a.x = c.x;
    if(d.x > a.x) a.x = d.x;
    if(b.y > a.y) a.y = b.y;
    if(c.y > a.y) a.y = c.y;
    if(d.y > a.y) a.y = d.y;
    if(b.z > a.z) a.z = b.z;
    if(c.z > a.z) a.z = c.z;
    if(d.z > a.z) a.z = d.z;
    return a;
}

Vec3d vec3d_4_max(Vec3d a, Vec3d b, Vec3d c, Vec3d d){
    Vec3d result = a;
    if(b.x > a.x) a.x = b.x;
    if(c.x > a.x) a.x = c.x;
    if(d.x > a.x) a.x = d.x;
    if(b.y > a.y) a.y = b.y;
    if(c.y > a.y) a.y = c.y;
    if(d.y > a.y) a.y = d.y;
    if(b.z > a.z) a.z = b.z;
    if(c.z > a.z) a.z = c.z;
    if(d.z > a.z) a.z = d.z;
    return a;
}

Vec3i vec3i_4_max(Vec3i a, Vec3i b, Vec3i c, Vec3i d){
    Vec3i result = a;
    if(b.x > a.x) a.x = b.x;
    if(c.x > a.x) a.x = c.x;
    if(d.x > a.x) a.x = d.x;
    if(b.y > a.y) a.y = b.y;
    if(c.y > a.y) a.y = c.y;
    if(d.y > a.y) a.y = d.y;
    if(b.z > a.z) a.z = b.z;
    if(c.z > a.z) a.z = c.z;
    if(d.z > a.z) a.z = d.z;
    return a;
}

Vec3i vec3i_4_max_unpacked(int ax, int ay, int az, int bx, int by, int bz, int cx, int cy, int cz, int dx, int dy, int dz){
    Vec3i a = {ax, ay, az};
    Vec3i b = {bx, by, bz};
    Vec3i c = {cx, cy, cz};
    Vec3i d = {dx, dy, dz};
    Vec3i result = a;
    if(b.x > a.x) a.x = b.x;
    if(c.x > a.x) a.x = c.x;
    if(d.x > a.x) a.x = d.x;
    if(b.y > a.y) a.y = b.y;
    if(c.y > a.y) a.y = c.y;
    if(d.y > a.y) a.y = d.y;
    if(b.z > a.z) a.z = b.z;
    if(c.z > a.z) a.z = c.z;
    if(d.z > a.z) a.z = d.z;
    return a;
}
