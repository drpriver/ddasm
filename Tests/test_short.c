#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

int main() {
    short arr[4];
    arr[0] = 1000;
    arr[1] = 2000;
    arr[2] = 3000;
    arr[3] = -500;
    printf("arr[0]=%d arr[1]=%d arr[2]=%d arr[3]=%d\n", arr[0], arr[1], arr[2], arr[3]);
    return 0;
}
