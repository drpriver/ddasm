#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

#define MAX_SIZE 10
#define FLAGS 0xFF
#define UNUSED

int main() {
    int arr[MAX_SIZE];
    int i;

    UNUSED

    for (i = 0; i < MAX_SIZE; i = i + 1) {
        arr[i] = i * 2;
    }

    printf("MAX_SIZE = %d\n", MAX_SIZE);
    printf("FLAGS = 0x%x\n", FLAGS);
    printf("arr[5] = %d\n", arr[5]);

    return 0;
}
