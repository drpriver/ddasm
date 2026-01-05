// SKIP
// tenative definitions
int x, y, z;
// declarations
extern int a, b, c;
#pragma library("libc")
int printf(const char*, ...);
int main(){
    printf("%d, %d, %d\n", x, y, z);
    printf("%p, %p, %p\n", &x, &y, &z);
    printf("%d, %d, %d\n", a, b, c);
    printf("%p, %p, %p\n", &a, &b, &c);
}
// definitions
int a = 0;
int b = 1;
int c = 2;
