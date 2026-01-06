#pragma library("libc")
// This is the actual printf from libc
extern int printf(char* fmt, ...);
int main(){
    printf("Hello from main()!\n");
    // varargs is supported.
    printf("%d, %d, %s", 1, 2, "number!\n");
    return 0;
}
