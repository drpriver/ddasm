#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

#define SIZE (32)
#define OFFSET (SIZE + 10)
#define FLAGS (0xFF)

int main() {
    printf("SIZE = %d\n", SIZE);
    printf("OFFSET = %d\n", OFFSET);
    printf("FLAGS = 0x%x\n", FLAGS);

    int arr[SIZE];
    arr[0] = 100;
    printf("arr[0] = %d\n", arr[0]);

    return 0;
}
