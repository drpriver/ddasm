#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

union SmallUnion {
    int a;
    int b;
};

union BigUnion {
    int arr[8];  // 32 bytes - too big for registers
    long big;
};

union SmallUnion make_small(int val) {
    union SmallUnion u;
    u.a = val;
    return u;
}

void print_small(union SmallUnion u) {
    printf("SmallUnion: %d\n", u.a);
}

union BigUnion make_big(int first) {
    union BigUnion u;
    u.arr[0] = first;
    u.arr[1] = 100;
    return u;
}

void print_big(union BigUnion u) {
    printf("BigUnion: arr[0]=%d arr[1]=%d\n", u.arr[0], u.arr[1]);
}

int main() {
    // Test small union (fits in register)
    union SmallUnion s;
    s = make_small(42);
    print_small(s);

    // Test big union (uses hidden pointer)
    union BigUnion b;
    b = make_big(999);
    print_big(b);

    return 0;
}
