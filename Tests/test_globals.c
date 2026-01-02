#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

int counter = 0;
int initialized = 42;

void increment() {
    counter = counter + 1;
}

int main() {
    printf("initialized = %d\n", initialized);
    printf("counter = %d\n", counter);

    increment();
    printf("after increment: counter = %d\n", counter);

    increment();
    increment();
    printf("after 2 more increments: counter = %d\n", counter);

    counter = 100;
    printf("after counter = 100: counter = %d\n", counter);

    return 0;
}
