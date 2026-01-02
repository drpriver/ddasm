#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

int main() {
    int arr[10];
    int* p1 = arr;
    int* p2 = arr + 5;

    // Pointer subtraction should give element count, not byte count
    long diff = p2 - p1;
    printf("p2 - p1 = %ld (expected 5)\n", diff);

    // Also test with &arr[n]
    int* p3 = arr + 8;
    diff = p3 - p1;
    printf("p3 - p1 = %ld (expected 8)\n", diff);

    // Pointer - integer
    int* p4 = p3 - 3;
    diff = p4 - p1;
    printf("(p3 - 3) - p1 = %ld (expected 5)\n", diff);

    return 0;
}
