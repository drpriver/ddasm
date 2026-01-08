// XFAIL: multiple #else in same #if
#if 1
int a = 1;
#else
int b = 2;
#else
int c = 3;
#endif
