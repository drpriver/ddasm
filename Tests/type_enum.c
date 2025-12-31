// Test (6.7.2.2) enum definitions and constants

enum Color {
    RED,
    GREEN,
    BLUE
};

enum Status {
    OK = 0,
    ERROR = -1,
    PENDING = 1
};

enum Flags {
    FLAG_NONE = 0,
    FLAG_READ = 1,
    FLAG_WRITE = 2,
    FLAG_EXEC = 4,
    FLAG_ALL = 7
};

int test_enum_value(void) {
    enum Color c = RED;
    return c;
}

int test_enum_explicit(void) {
    enum Status s = ERROR;
    return s;
}

int test_enum_arithmetic(enum Color c) {
    return c + 1;
}

int test_enum_flags(void) {
    int flags = FLAG_READ | FLAG_WRITE;
    return flags;
}

// Anonymous enum
enum {
    CONST_A = 100,
    CONST_B = 200
};

int test_anon_enum(void) {
    return CONST_A + CONST_B;
}
