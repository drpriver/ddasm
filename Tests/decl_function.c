// SKIP: function pointer parameter calls not yet supported
// Test (6.7.6) function declarators

// Forward declaration
int add(int a, int b);
int sub(int, int);

// Function with no params
int get_zero(void) {
    return 0;
}

// Function definitions
int add(int a, int b) {
    return a + b;
}

int sub(int a, int b) {
    return a - b;
}

// Function pointer parameter
int apply(int (*fn)(int, int), int x, int y) {
    return fn(x, y);
}

// Variadic declaration (extern only)
extern int printf(const char*, ...);
int main(){ return 0; }
