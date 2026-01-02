#pragma library("libc.so.6")
extern int printf(char* fmt, ...);
extern void* malloc(long size);

int main() {
    // Allocate an array of 4 ints
    int* arr = malloc(4 * 4);  // 4 ints * 4 bytes each

    // Write values using pointer arithmetic
    *arr = 10;
    *(arr + 1) = 20;
    *(arr + 2) = 30;
    *(arr + 3) = 40;

    // Read using subscript
    printf("arr[0] = %d\n", arr[0]);
    printf("arr[1] = %d\n", arr[1]);
    printf("arr[2] = %d\n", arr[2]);
    printf("arr[3] = %d\n", arr[3]);

    // Test arithmetic
    int* p = arr + 2;
    printf("*(arr+2) = %d\n", *p);

    return 0;
}
