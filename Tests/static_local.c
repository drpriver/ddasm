// Test static local variables

int counter() {
    static int count = 0;
    count++;
    return count;
}

int other_counter() {
    static int count = 0;  // Different static, same name
    count++;
    return count;
}

int test_scoped_static(int call_num) {
    static int x = 10;
    {
        static int x = 20;  // Different static in inner scope
        x++;
        if (x != 20 + call_num) return 1;
    }
    x++;
    if (x != 10 + call_num) return 1;
    return 0;
}

int main() {
    // Test basic static local persistence
    if (counter() != 1) return 1;
    if (counter() != 2) return 2;
    if (counter() != 3) return 3;

    // Test separate static locals with same name in different functions
    if (other_counter() != 1) return 4;
    if (other_counter() != 2) return 5;

    // Original counter should still work independently
    if (counter() != 4) return 6;

    // Test scoped static locals
    if (test_scoped_static(1) != 0) return 7;
    if (test_scoped_static(2) != 0) return 8;  // Call again to verify persistence

    return 0;
}
