// SKIP: extern variables a,b,c not available in libc
int x, y, z;
extern int a, b, c;
#pragma library("libc")
int printf(const char*, ...);
int main(){
    printf("%d, %d, %d\n", x, y, z);
    printf("%p, %p, %p\n", &x, &y, &z);
    printf("%d, %d, %d\n", a, b, c);
    printf("%p, %p, %p\n", &a, &b, &c);
}
