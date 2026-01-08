// __LINE__, __FILE__, and related magic macros

int line1 = __LINE__;  // Should be this line number

// __LINE__ in macro
#define GET_LINE __LINE__
int line2 = GET_LINE;  // Line where GET_LINE is used

// __LINE__ stringified
#define STR(x) #x
#define XSTR(x) STR(x)
#define LINE_STR XSTR(__LINE__)
char *s1 = LINE_STR;  // Should be string of line number

// __FILE__
char *file = __FILE__;

// __LINE__ in nested macro
#define OUTER_LINE INNER_LINE
#define INNER_LINE __LINE__
int line3 = OUTER_LINE;

// Multiple __LINE__ on same line
int same_line = __LINE__ + __LINE__;  // Both should be same

// __LINE__ across continuation
int cont_line = __LINE__ \
    + 0;

// #line directive
#line 1000
int fake_line = __LINE__;  // 1000

#line 2000 "fake_file.c"
char *fake_file = __FILE__;  // "fake_file.c"
int fake_line2 = __LINE__;  // 2000

// Reset to automatic
#line 50
int reset_line = __LINE__;  // 50

// __DATE__ and __TIME__ - just verify they exist
// char *date = __DATE__;
// char *time = __TIME__;

// __COUNTER__ (GNU extension) - if supported
#ifdef __COUNTER__
int counter1 = __COUNTER__;  // 0
int counter2 = __COUNTER__;  // 1
int counter3 = __COUNTER__;  // 2
#endif

// __func__ equivalent via macro (not standard but common)
// #define FUNC_NAME __func__

// Multiple macros using __LINE__
#define UNIQUE_NAME_IMPL(prefix, line) prefix ## line
#define UNIQUE_NAME(prefix) UNIQUE_NAME_IMPL(prefix, __LINE__)
int UNIQUE_NAME(var_);  // var_<line number>

// Token paste with __LINE__
#define PASTE_LINE(x) x ## __LINE__
// int PASTE_LINE(var);  // var__LINE__ (## prevents expansion)

// Need indirection to expand __LINE__ first
#define PASTE_LINE2_IMPL(x, l) x ## l
#define PASTE_LINE2(x) PASTE_LINE2_IMPL(x, __LINE__)
// int PASTE_LINE2(var);  // var<line>

int main(){ return 0; }
