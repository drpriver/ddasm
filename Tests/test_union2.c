#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

union Data {
    int value;
    char bytes[8];  // 8 bytes to show union takes max size
};

void test_overlap() {
    union Data d;
    d.value = 0x04030201;  // Little endian: bytes will be 01 02 03 04
    printf("value = 0x%x\n", d.value);
    printf("bytes[0] = %d\n", d.bytes[0]);  // Should be 1
    printf("bytes[1] = %d\n", d.bytes[1]);  // Should be 2
    printf("bytes[2] = %d\n", d.bytes[2]);  // Should be 3
    printf("bytes[3] = %d\n", d.bytes[3]);  // Should be 4
}

int main() {
    test_overlap();
    return 0;
}
