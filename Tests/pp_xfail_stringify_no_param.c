// XFAIL: # not followed by parameter
#define BAD(x) # y
char *s = BAD(hello);
