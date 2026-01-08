// XFAIL: #elif after #else
#if 0
int a = 1;
#else
int b = 2;
#elif 1
int c = 3;
#endif
