// Test logical && and || operators
#pragma library("libc")
int printf(const char*, ...);
void exit(int);

#define assert(x) do { if (!(x)) { printf("%s:%d: FAIL: %s\n", __FILE__, __LINE__, #x); exit(1); } } while(0)

int main() {
    // && tests
    assert(1 && 1);
    assert(!(1 && 0));
    assert(!(0 && 1));
    assert(!(0 && 0));

    // || tests
    assert(1 || 1);
    assert(1 || 0);
    assert(0 || 1);
    assert(!(0 || 0));

    // With non-zero values
    assert(0xf0 && 0x0f);
    assert(0xf0 || 0);

    // Result values
    assert((1 && 1) == 1);
    assert((1 && 0) == 0);
    assert((1 || 0) == 1);
    assert((0 || 0) == 0);

    exit(0);
}
