// Debug float comparison
#pragma library("libc")

int printf(const char*, ...);

int main(void) {
    float a = 0.0f;
    float b = 0.01f;

    printf("a = %f, b = %f\n", (double)a, (double)b);

    int lt = a < b;
    printf("a < b = %d (expect 1)\n", lt);

    int gt = a > b;
    printf("a > b = %d (expect 0)\n", gt);

    int eq = a == b;
    printf("a == b = %d (expect 0)\n", eq);

    int neq = a != b;
    printf("a != b = %d (expect 1)\n", neq);

    int le = a <= b;
    printf("a <= b = %d (expect 1)\n", le);

    int ge = a >= b;
    printf("a >= b = %d (expect 0)\n", ge);

    // Try with doubles
    double da = 0.0;
    double db = 0.01;
    int dlt = da < db;
    printf("\ndouble: da < db = %d (expect 1)\n", dlt);

    return 0;
}
