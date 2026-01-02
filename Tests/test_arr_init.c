#pragma library("libc.so.6")
extern int printf(char* fmt, ...);
int main() {
    char foo[13] = "hello world\n";
    printf("%s", foo);
    return 0;
}
