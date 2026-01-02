#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

// Anonymous enum typedef
typedef enum {
    APPLE,
    BANANA,
    CHERRY
} Fruit;

// Named enum typedef
typedef enum Color {
    RED = 1,
    GREEN,
    BLUE
} ColorType;

int main() {
    Fruit f;
    f = BANANA;
    printf("f = %d (BANANA)\n", f);

    ColorType c;
    c = GREEN;
    printf("c = %d (GREEN)\n", c);

    // Can also use enum Color since it was named
    enum Color c2;
    c2 = BLUE;
    printf("c2 = %d (BLUE)\n", c2);

    return 0;
}
