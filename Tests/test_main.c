#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

int main() {
    printf("Hello from main()!\n");
    return 0;
}
