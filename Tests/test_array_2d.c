int arr[2][3] = {{1, 2, 3}, {4, 5, 6}};

#pragma library("libc")
int printf(const char* fmt, ...);

int main() {
    printf("arr[0][0]=%d arr[0][1]=%d arr[0][2]=%d\n", arr[0][0], arr[0][1], arr[0][2]);
    printf("arr[1][0]=%d arr[1][1]=%d arr[1][2]=%d\n", arr[1][0], arr[1][1], arr[1][2]);
    return 0;
}
