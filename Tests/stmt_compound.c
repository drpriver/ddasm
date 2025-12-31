// Test (6.8.2) compound-statement (blocks)

int test_empty_block(void) {
    {}
    return 0;
}

int test_simple_block(void) {
    {
        int x = 5;
        x = x + 1;
    }
    return 0;
}

int test_nested_blocks(void) {
    int a = 1;
    {
        int b = 2;
        {
            int c = 3;
            a = a + b + c;
        }
    }
    return a;
}

int test_block_scope(void) {
    int x = 10;
    {
        int x = 20;
        x = x + 1;
    }
    return x;  // Should return 10
}
