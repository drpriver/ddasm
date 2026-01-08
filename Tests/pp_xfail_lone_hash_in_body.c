// XFAIL: lone # not followed by parameter in function macro body
#define BAD(x) # not_param
int a = 1;
