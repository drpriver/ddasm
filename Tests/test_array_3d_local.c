#pragma library("libc")
int printf(const char* fmt, ...);

int main() {
    int arr[2][3][4] = {
        {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 10, 11, 12}},
        {{13, 14, 15, 16}, {17, 18, 19, 20}, {21, 22, 23, 24}}
    };
    printf("arr[0][0]: %d %d %d %d\n", arr[0][0][0], arr[0][0][1], arr[0][0][2], arr[0][0][3]);
    printf("arr[0][2]: %d %d %d %d\n", arr[0][2][0], arr[0][2][1], arr[0][2][2], arr[0][2][3]);
    printf("arr[1][0]: %d %d %d %d\n", arr[1][0][0], arr[1][0][1], arr[1][0][2], arr[1][0][3]);
    printf("arr[1][2]: %d %d %d %d\n", arr[1][2][0], arr[1][2][1], arr[1][2][2], arr[1][2][3]);
    return 0;
}
