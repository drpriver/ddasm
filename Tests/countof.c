// Test _Countof operator
#pragma library("libc")
int printf(const char*, ...);
void exit(int);
// typedef unsigned long long size_t;

#define assert(lhs, rhs) do { if (!((lhs) == (rhs))) { printf("%s:%d: FAIL: %s == %s\n", __FILE__, __LINE__, #lhs, #rhs); printf("  %s = %zu\n", #lhs, (lhs)); printf("  %s = %zu\n", #rhs, (rhs)); exit(1); } } while(0)

int main() {
    int arr[10];

    // _Countof without parens on expression
    assert(_Countof arr,  10);

    // _Countof with parens on expression
    assert(_Countof(arr), 10);

    // _Countof on type
    assert(_Countof(int[5]), 5);
    assert(_Countof(char[100]), 100);

    exit(0);
}
