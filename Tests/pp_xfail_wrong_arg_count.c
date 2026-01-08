// XFAIL: wrong number of arguments to function-like macro
#define ADD(a, b) ((a) + (b))
int x = ADD(1);  // missing second argument
