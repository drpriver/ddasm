#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

void start() {
    int a = 5;
    int b = 3;
    int sum = a + b;
    printf("5 + 3 = %d\n", sum);
}
int main(){ return 0; }
