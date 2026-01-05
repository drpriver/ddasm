#include <stdio.h>
#include "abi_test.h"

int main() {
    Vec3d v = {3.0, 4.0, 0.0};
    printf("v = {%f, %f, %f}\n", v.x, v.y, v.z);
    double result = vec3d_length_sq(v);
    printf("vec3d_length_sq = %f (expected 25.0)\n", result);
    return 0;
}
