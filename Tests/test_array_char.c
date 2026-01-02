#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

int main() {
    char buf[4];

    buf[0] = 'H';
    buf[1] = 'i';
    buf[2] = '!';
    buf[3] = 0;

    printf("buf = %s\n", buf);

    return 0;
}
