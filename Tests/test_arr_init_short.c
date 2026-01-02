#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

int main() {
    // String shorter than array - rest should be zeros
    char foo[10] = "hi";
    printf("foo[0]=%d foo[1]=%d foo[2]=%d foo[3]=%d foo[4]=%d\n",
           foo[0], foo[1], foo[2], foo[3], foo[4]);
    return 0;
}
