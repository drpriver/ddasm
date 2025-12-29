#pragma library("libc.so.6")
extern int printf(char* fmt, ...);
int main(){
    printf("Hello from main()!\n");
    printf("%d, %d, %s", 1, 2, "number!\n");
    return 0;
}
