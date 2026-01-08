// XFAIL: function-like macro call with missing closing paren
#define FOO(x) x+1
int a = FOO(5;
