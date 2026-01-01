// Test logical && and || operators
// EXPECTED: 6

int main() {
    int result = 0;

    // && tests
    if (1 && 1) result += 1;      // true
    if (1 && 0) result += 100;    // false
    if (0 && 1) result += 100;    // false
    if (0 && 0) result += 100;    // false

    // || tests
    if (1 || 1) result += 1;      // true
    if (1 || 0) result += 1;      // true
    if (0 || 1) result += 1;      // true
    if (0 || 0) result += 100;    // false

    // With non-zero values
    if (0xf0 && 0x0f) result += 1;  // true
    if (0xf0 || 0) result += 1;     // true

    return result;  // Expected: 6
}
