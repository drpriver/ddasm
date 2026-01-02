#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

void start() {
    printf("Custom start()!\n");
}

int main() {
    printf("This is main()\n");
    return 0;
}
