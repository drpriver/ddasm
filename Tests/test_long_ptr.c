#pragma library("libc.so.6")
extern int printf(char* fmt, ...);
extern void* malloc(long size);

int main() {
    // Allocate an array of 4 longs (8 bytes each)
    long* arr = malloc(4 * 8);

    // Write values
    arr[0] = 100;
    arr[1] = 200;
    arr[2] = 300;
    arr[3] = 400;

    // Read using pointer arithmetic
    printf("arr[0] = %ld\n", *arr);
    printf("arr[1] = %ld\n", *(arr + 1));
    printf("arr[2] = %ld\n", *(arr + 2));
    printf("arr[3] = %ld\n", *(arr + 3));

    return 0;
}
