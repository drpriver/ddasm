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
int main(){
    test_unsigned(1);
    test_signed(1);
    test_uchar(1);
    test_ushort(1);
    test_ulong(1);
    test_int_unsigned(1);
    test_unsigned_long_int(1);
    test_long_unsigned(1);
    return 0;
}
