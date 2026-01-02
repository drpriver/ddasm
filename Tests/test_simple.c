#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

int main() {
    printf("Hello!\n");
    return 0;
}
