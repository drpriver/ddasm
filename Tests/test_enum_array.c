#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

enum { SIZE = 5 };

int main() {
    int arr[SIZE];
    int i;
    for (i = 0; i < SIZE; i = i + 1) {
        arr[i] = i * 10;
    }
    for (i = 0; i < SIZE; i = i + 1) {
        printf("arr[%d] = %d\n", i, arr[i]);
    }
    return 0;
}
