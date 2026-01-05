// Debug - pass pointer and return double
#pragma library("libc")
#pragma library("abi_test")

int printf(const char*, ...);

// Just pass 3 doubles as separate args
double vec3d_length_sq_args(double x, double y, double z) {
    return x*x + y*y + z*z;
}

// Or try with existing function that takes pointer
typedef struct { double x, y, z; } Vec3d;
double vec3d_length_sq(Vec3d v);

int main(void) {
    // Test 1: Direct double args
    double x = 3.0;
    double y = 4.0;
    double z = 0.0;

    // Test 2: Struct pass
    Vec3d v;
    v.x = 3.0;
    v.y = 4.0;
    v.z = 0.0;

    printf("Calling vec3d_length_sq with struct...\n");
    double result = vec3d_length_sq(v);
    printf("Result = %f (expected 25.0)\n", result);

    return 0;
}
