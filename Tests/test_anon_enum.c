#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

// Anonymous enum - just defines constants
enum {
    FOO = 10,
    BAR = 20,
    BAZ = 0x30
};

int main() {
    printf("FOO = %d\n", FOO);
    printf("BAR = %d\n", BAR);
    printf("BAZ = %d\n", BAZ);
    return 0;
}
