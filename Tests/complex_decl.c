// SKIP: unsupported function to ptr decay
// Test file for complex declarator parsing
#pragma library("libc")
int printf(const char*, ...);

// Function pointer
int (*func_ptr)(int, int);

// Pointer to array of 10 ints
int (*ptr_to_array)[10];

// Array of 5 function pointers
int (*func_array[5])(int);

// Simple callback typedef
typedef void (*callback_t)(int);

// Test usage
int add(int a, int b) {
    return a + b;
}

int main() {
    func_ptr = add;
    int result = func_ptr(3, 4);
    printf("Result: %d\n", result);
    return 0;
}
