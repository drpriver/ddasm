// Test (6.7.2) signed/unsigned variants

unsigned int test_unsigned(unsigned int u) {
    return u + 1;
}

signed int test_signed(signed int s) {
    return s - 1;
}

unsigned char test_uchar(unsigned char c) {
    return c;
}

unsigned short test_ushort(unsigned short s) {
    return s;
}

unsigned long test_ulong(unsigned long l) {
    return l;
}

// Various signed/unsigned orderings
int test_int_unsigned(int unsigned x) {
    return x;
}

int test_unsigned_long_int(unsigned long int x) {
    return (int)x;
}

int test_long_unsigned(long unsigned x) {
    return (int)x;
}
