// Debug Vec3d pass
#pragma library("libc")
#pragma library("abi_test")

int printf(const char*, ...);

typedef struct { double x, y, z; } Vec3d;
double vec3d_length_sq(Vec3d v);

int main(void) {
    Vec3d v;
    v.x = 3.0;
    v.y = 4.0;
    v.z = 0.0;

    printf("Before call: v.x=%f v.y=%f v.z=%f\n", v.x, v.y, v.z);

    double result = vec3d_length_sq(v);

    printf("After call: result=%f\n", result);
    printf("Expected: 25.0 (3*3 + 4*4 + 0*0)\n");

    return 0;
}
