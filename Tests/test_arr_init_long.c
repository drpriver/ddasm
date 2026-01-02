#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

int main() {
    // String too long - should truncate
    char foo[5] = "hello world";
    foo[4] = 0;  // Ensure null termination
    printf("truncated: '%s'\n", foo);
    return 0;
}
