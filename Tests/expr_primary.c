// Test (6.5.1) primary-expression
// identifier, constant, string-literal, ( expression )

int global_var;

int test_identifiers(int param) {
    int local = 42;
    global_var = param;
    return local;
}

int test_constants(void) {
    int decimal = 123;
    int hex = 0xFF;
    int octal = 0777;
    char c = 'A';
    return decimal + hex;
}

char* test_strings(void) {
    char* s = "hello world";
    char* empty = "";
    return s;
}

int test_parens(int x) {
    return (x + 1) * (x - 1);
}
