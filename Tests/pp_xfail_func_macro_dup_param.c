// XFAIL: duplicate parameter names in function-like macro
#define BAD(x, x) (x)
int a = BAD(1, 2);
