// Test #embed directive
#pragma library("libc")
int printf(const char*, ...);

#ifdef __DDASM__
void abort(void){__dasm{abort;}}
#else
void abort(void);
#endif
#define assert_eq(lhs, rhs) do {\
    if((lhs) != (rhs)){ \
        printf("%s:%d assertion failed: %s == %s\n", __FILE__, __LINE__, #lhs, #rhs); \
        printf("  %s -> '%c'\n", #lhs, lhs); \
        printf("  %s -> '%c'\n", #rhs, rhs); \
    } \
} while(0)
const unsigned char data[] = {
#embed "test_embed.bin"
};

// With limit
const char limited[] = {
#embed "test_embed.bin" limit(3)
};

// With suffix (null terminator)
const char str[] = {
#embed "test_embed.bin" suffix(, 0)
};

// With prefix and suffix
const char with_prefix[] = {
#if 0
#embed "test_embed.bin" prefix(0xEF, 0xBB, 0xBF,) suffix(, 0)
#endif
};

// Test if_empty with empty limit
const char empty_test[] = {
#embed "test_embed.bin" limit(0) if_empty('E', 'M', 'P', 'T', 'Y')
};

int main() {
    // Test basic embed - should be "HELLO" (5 bytes)
    if(sizeof data != 5) return 1;
    assert_eq(data[0], 'H');
    assert_eq(data[1], 'E');
    assert_eq(data[2], 'L');
    assert_eq(data[3], 'L');
    assert_eq(data[4], 'O');

    // Test limit(3) - should be "HEL" (3 bytes)
    if(sizeof limited != 3) return 2;
    assert_eq(limited[0], 'H');
    assert_eq(limited[1], 'E');
    assert_eq(limited[2], 'L');

    // Test suffix(, 0) - should be "HELLO\0" (6 bytes)
    if(sizeof str != 6) return 3;
    assert_eq(str[0], 'H');
    assert_eq(str[1], 'E');
    assert_eq(str[2], 'L');
    assert_eq(str[3], 'L');
    assert_eq(str[4], 'O');
    assert_eq(str[5], '\0');

    // Test if_empty with limit(0) - should be "EMPTY" (5 bytes)
    if(sizeof empty_test != 5) return 4;
    assert_eq(empty_test[0], 'E');
    assert_eq(empty_test[1], 'M');
    assert_eq(empty_test[2], 'P');
    assert_eq(empty_test[3], 'T');
    assert_eq(empty_test[4], 'Y');

    return 0;
}
