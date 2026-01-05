#pragma library("libc")
int printf(const char* fmt, ...);

long add8(long a, long b, long c, long d, long e, long f, long g, long h) {
    return a + b + c + d + e + f + g + h;
}

int main() {
    long result = add8(1, 2, 3, 4, 5, 6, 7, 8);
    printf("sum=%ld\n", result);
    return 0;
}
