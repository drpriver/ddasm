// Test (6.7.2) basic type-specifiers
// void, char, short, int, long, float, double

void void_func(void) {
    return;
}

char test_char(char c) {
    return c + 1;
}

short test_short(short s) {
    return s * 2;
}

int test_int(int i) {
    return i + i;
}

long test_long(long l) {
    return l;
}

// Note: float/double codegen may not be fully implemented
int test_sizes(void) {
    int c_size = sizeof(char);
    int s_size = sizeof(short);
    int i_size = sizeof(int);
    int l_size = sizeof(long);
    return c_size + s_size + i_size + l_size;
}
int main(){
    test_char(1);
    test_short(1);
    test_int(1);
    test_long(1);
    test_sizes();
    return 0;
}
