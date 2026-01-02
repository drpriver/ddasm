#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

int main() {
    char* s = "Hello";
    printf("String: %s\n", s);
    printf("First char: %c\n", *s);
    printf("Second char: %c\n", *(s + 1));
    printf("Third char: %c\n", s[2]);
    return 0;
}
