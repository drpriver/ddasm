// Debug - check raw return bits
#pragma library("libc")
#pragma library("abi_test")

int printf(const char*, ...);

typedef struct { double x, y, z; } Vec3d;
double vec3d_length_sq(Vec3d v);

typedef union {
    double d;
    unsigned long u;
} DblBits;

int main(void) {
    Vec3d v;
    v.x = 3.0;
    v.y = 4.0;
    v.z = 0.0;

    double result = vec3d_length_sq(v);

    DblBits bits;
    bits.d = result;

    printf("result as double: %f\n", result);
    printf("result as hex: %lx\n", bits.u);
    printf("expected 25.0 hex: %lx\n", 0x4039000000000000UL);

    return 0;
}
