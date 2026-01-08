// XFAIL: modulo by zero in #if expression
#if 1%0
int a = 1;
#endif
