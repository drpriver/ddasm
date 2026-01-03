// Test postfix const syntax (type const * instead of const type *)

// Function with postfix const parameter
int read_value(int const *ptr) {
    return *ptr;
}

// Function with void const * parameter
int read_byte(void const *ptr) {
    return *((char const *)ptr);
}

// Multiple levels of const
int read_nested(int const * const *ptr) {
    return **ptr;
}

int main() {
    int x = 42;
    int const *p = &x;

    if (read_value(p) != 42) return 1;
    if (read_value(&x) != 42) return 2;

    char c = 99;
    if (read_byte(&c) != 99) return 3;

    int const *inner = &x;
    int const * const *outer = &inner;
    if (read_nested(outer) != 42) return 4;

    return 0;
}
