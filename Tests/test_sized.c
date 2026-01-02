#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

int main() {
    int arr[3];
    arr[0] = 100;
    arr[1] = 200;
    arr[2] = 300;
    printf("arr[0]=%d arr[1]=%d arr[2]=%d\n", arr[0], arr[1], arr[2]);

    char str[5] = "abcd";
    str[2] = 'X';
    printf("str[0]=%c str[1]=%c str[2]=%c str[3]=%c\n", str[0], str[1], str[2], str[3]);
    return 0;
}
