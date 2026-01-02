// Test #embed directive

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
#embed "test_embed.bin" prefix(0xEF, 0xBB, 0xBF,) suffix(, 0)
};

// Test if_empty with empty limit
const char empty_test[] = {
#embed "test_embed.bin" limit(0) if_empty('E', 'M', 'P', 'T', 'Y')
};

int main() {
    return 0;
}
