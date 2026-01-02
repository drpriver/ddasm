#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

void start() {
    // Test for loop
    printf("For loop: ");
    for (int i = 0; i < 5; i = i + 1) {
        printf("%d ", i);
    }
    printf("\n");

    // Test while loop
    printf("While loop: ");
    int j = 0;
    while (j < 5) {
        printf("%d ", j);
        j = j + 1;
    }
    printf("\n");

    // Test if/else
    int x = 10;
    if (x > 5) {
        printf("x > 5: true\n");
    } else {
        printf("x > 5: false\n");
    }
}
