// XFAIL: consecutive ## operators
#define BAD(a, b) a ## ## b
int a = BAD(x, y);
