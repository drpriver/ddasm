// Test _Countof operator
#pragma library("libc")
int printf(const char*, ...);
void exit(int);

#define assert(x) do { if (!(x)) { printf("%s:%d: FAIL: %s\n", __FILE__, __LINE__, #x); exit(1); } } while(0)

int main() {
    int arr[10];

    // _Countof without parens on expression
    assert(_Countof arr == 10);

    // _Countof with parens on expression
    assert(_Countof(arr) == 10);

    // _Countof on type
    assert(_Countof(int[5]) == 5);
    assert(_Countof(char[100]) == 100);

    exit(0);
}
