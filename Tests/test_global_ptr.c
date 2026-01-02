#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

char* message = "Hello from global!";
int value = 123;
int* ptr;

int main() {
    printf("message: %s\n", message);
    printf("value: %d\n", value);

    ptr = &value;
    printf("*ptr: %d\n", *ptr);

    *ptr = 456;
    printf("after *ptr = 456: value = %d\n", value);

    return 0;
}
