#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

int main() {
    long arr[3];

    arr[0] = 100;
    arr[1] = 200;
    arr[2] = 300;

    printf("arr[0] = %ld\n", arr[0]);
    printf("arr[1] = %ld\n", arr[1]);
    printf("arr[2] = %ld\n", arr[2]);

    long sum = arr[0] + arr[1] + arr[2];
    printf("sum = %ld\n", sum);

    return 0;
}
