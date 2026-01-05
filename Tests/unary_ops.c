// Test all unary operators
#pragma library("libc")
int printf(const char*, ...);
void exit(int);

#define assert(x) do { if (!(x)) { printf("%s:%d: FAIL: %s\n", __FILE__, __LINE__, #x); exit(1); } } while(0)

int main() {
    int x;

    // Logical NOT
    assert(!0 == 1);
    assert(!1 == 0);
    assert(!42 == 0);

    // Bitwise NOT
    assert(~0 == -1);
    assert((~0xff & 0xff) == 0);

    // Unary minus
    assert(-5 == 0 - 5);
    assert(-(-3) == 3);

    // Unary plus
    assert(+5 == 5);
    assert(+(-3) == -3);

    // Pre-increment
    x = 5;
    assert(++x == 6);
    assert(x == 6);

    // Pre-decrement
    x = 5;
    assert(--x == 4);
    assert(x == 4);

    // Post-increment
    x = 5;
    assert(x++ == 5);
    assert(x == 6);

    // Post-decrement
    x = 5;
    assert(x-- == 5);
    assert(x == 4);

    // Multiple increments in sequence
    x = 0;
    ++x; ++x; ++x;
    assert(x == 3);

    x = 10;
    x--; x--; x--;
    assert(x == 7);

    // Increment/decrement on array elements
    int arr[3] = {10, 20, 30};
    assert(++arr[0] == 11);
    assert(arr[0] == 11);
    assert(arr[1]++ == 20);
    assert(arr[1] == 21);

    // Address-of and dereference
    x = 42;
    int* p = &x;
    assert(*p == 42);
    *p = 100;
    assert(x == 100);

    // Increment through pointer
    x = 5;
    p = &x;
    ++*p;
    assert(x == 6);
    (*p)++;
    assert(x == 7);

    // sizeof
    assert(sizeof(int) == 4);
    assert(sizeof(char) == 1);
    assert(sizeof(long) == 8);
    assert(sizeof x == 4);
    assert(sizeof arr == 12);  // 3 * 4

    // sizeof on expressions (not evaluated)
    x = 10;
    assert(sizeof(x++) == 4);
    assert(x == 10);  // x should NOT have been incremented

    // _Countof
    int arr2[7];
    assert(_Countof arr2 == 7);
    assert(_Countof(int[15]) == 15);

    // Chained unary operators
    x = 5;
    assert(-(-(-x)) == -5);
    assert(!!1 == 1);
    assert(!!0 == 0);
    assert(~~0xff == 0xff);

    // Increment in expressions
    x = 5;
    int y = ++x + 10;
    assert(y == 16);  // 6 + 10
    assert(x == 6);

    x = 5;
    y = x++ + 10;
    assert(y == 15);  // 5 + 10
    assert(x == 6);

    // Pointer increment
    int nums[4] = {1, 2, 3, 4};
    int* q = nums;
    assert(*q == 1);
    q++;
    assert(*q == 2);
    ++q;
    assert(*q == 3);
    assert(*q++ == 3);
    assert(*q == 4);

    // Pointer decrement
    q = &nums[3];
    assert(*q == 4);
    q--;
    assert(*q == 3);
    --q;
    assert(*q == 2);
    assert(*q-- == 2);
    assert(*q == 1);

    // Combining address-of with array indexing
    int val = 99;
    int* pval = &val;
    assert(*&val == 99);
    assert(*&*pval == 99);

    // Negative array index via pointer
    q = &nums[2];
    assert(q[-1] == 2);
    assert(q[-2] == 1);

    exit(0);  // Success
}
