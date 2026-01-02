#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

enum Color {
    RED,
    GREEN,
    BLUE
};

enum Status {
    OK = 0,
    ERROR = -1,
    PENDING = 100
};

int main() {
    printf("RED = %d\n", RED);
    printf("GREEN = %d\n", GREEN);
    printf("BLUE = %d\n", BLUE);

    printf("OK = %d\n", OK);
    printf("ERROR = %d\n", ERROR);
    printf("PENDING = %d\n", PENDING);

    enum Color c;
    c = GREEN;
    printf("c = %d\n", c);

    return 0;
}
