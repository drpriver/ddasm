// Simple function-like macro
#define FOO(x) 0

// Without extra parens - should work
#define TEST1 FOO(a) || 1

// With extra parens - might fail
#define TEST2 (FOO(a)) || 1

#if TEST1
int test1 = 1;
#else
int test1 = 0;
#endif

#if TEST2
int test2 = 1;
#else
int test2 = 0;
#endif

int start() {
    return test1 + test2;
}
int main(){ return 0; }
