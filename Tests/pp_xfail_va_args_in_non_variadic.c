// XFAIL: __VA_ARGS__ in non-variadic macro
#define BAD(x) __VA_ARGS__
int a = BAD(1);
