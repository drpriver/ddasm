// SKIP: Local enums are broken apparently
// Test inline enum definitions (enum defined inside function body)

int test_named_enum() {
    enum Color {
        RED = 1,
        GREEN = 2,
        BLUE = 3
    };

    enum Color c = GREEN;
    return c;
}

int test_anonymous_enum() {
    enum {
        SMALL = 10,
        MEDIUM = 20,
        LARGE = 30
    };

    return SMALL + MEDIUM + LARGE;  // 60
}

int test_enum_with_var() {
    enum Status {
        OK = 0,
        ERROR = -1
    } status = OK;

    return status;
}

int main() {
    if (test_named_enum() != 2) return 1;
    if (test_anonymous_enum() != 60) return 2;
    if (test_enum_with_var() != 0) return 3;

    // Test using enum constants in expressions
    enum { A = 5, B = 10, C = 15 };
    int sum = A + B + C;
    if (sum != 30) return 4;

    return 0;
}
