// XFAIL: macro redefinition with different value (should warn/error in strict mode)
#define FOO 1
#define FOO 2
int a = FOO;
