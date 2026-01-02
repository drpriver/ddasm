#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

void swap(int* a, int* b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

int main() {
    int x = 10;
    int y = 20;
    printf("Before: x=%d, y=%d\n", x, y);
    swap(&x, &y);
    printf("After:  x=%d, y=%d\n", x, y);
    return 0;
}
