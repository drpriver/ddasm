#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

int main() {
    int a = 1;
    int b = 2;
    int c = 3;
    int d = 4;
    int e = 5;  // 5th variable - should trigger stack mode
    int f = 6;
    int g = 7;
    int h = 8;

    int sum = a + b + c + d + e + f + g + h;
    printf("sum of 1-8 = %d\n", sum);

    return 0;
}
