// XFAIL: unterminated macro argument list
#define FOO(a, b) (a + b)
int x = FOO(1, 2;  // missing )
