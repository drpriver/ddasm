// Debug Vec3d pass specifically
#pragma library("libc")
int printf(const char*, ...);

#pragma library("abi_test")
typedef struct { double x, y, z; } Vec3d;
double vec3d_length_sq(Vec3d v);

int main(void) {
    Vec3d v;
    v.x = 3.0;
    v.y = 4.0;
    v.z = 0.0;

    printf("v.x = %f, v.y = %f, v.z = %f\n", v.x, v.y, v.z);

    double result = vec3d_length_sq(v);

    printf("vec3d_length_sq = %f (expected 25.0)\n", result);

    return 0;
}
