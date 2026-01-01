// Test _Countof operator
// EXPECTED: 25

int main() {
    int arr[10];
    int result = 0;

    // _Countof without parens on expression
    result += _Countof arr;  // 10

    // _Countof with parens on expression
    result += _Countof(arr);  // 10

    // _Countof on type
    result += _Countof(int[5]);  // 5

    return result;  // Expected: 25
}
