#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

enum Flags {
    FLAG_NONE = 0x0,
    FLAG_READ = 0x1,
    FLAG_WRITE = 0x2,
    FLAG_EXEC = 0x4,
    FLAG_ALL = 0xFF
};

enum Colors {
    COLOR_RED = 0xFF0000,
    COLOR_GREEN = 0x00FF00,
    COLOR_BLUE = 0x0000FF
};

int main() {
    printf("FLAG_NONE = 0x%x\n", FLAG_NONE);
    printf("FLAG_READ = 0x%x\n", FLAG_READ);
    printf("FLAG_WRITE = 0x%x\n", FLAG_WRITE);
    printf("FLAG_EXEC = 0x%x\n", FLAG_EXEC);
    printf("FLAG_ALL = 0x%x\n", FLAG_ALL);
    printf("COLOR_RED = 0x%x\n", COLOR_RED);
    printf("COLOR_GREEN = 0x%x\n", COLOR_GREEN);
    printf("COLOR_BLUE = 0x%x\n", COLOR_BLUE);
    return 0;
}
