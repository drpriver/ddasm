// Macro argument edge cases

// Empty argument - use in context where empty is valid
#define ID(x) (0 x)
int a = ID();  // (0 ) - valid with empty

// Multiple empty arguments - use in safe context
#define TWO(a, b) (0 a b)
int b = TWO(,);  // (0  ) - valid

// Argument with parentheses (not split)
#define PAREN_ARG(x) x
int c = PAREN_ARG((1, 2, 3));  // (1, 2, 3) - comma expr evaluates to 3

// Nested parentheses
#define NESTED(x) (x)
int d = NESTED(((5)));  // (((5)))

// Argument containing macro
#define INNER 42
#define OUTER(x) (x)
int e = OUTER(INNER);  // (42) - arg expanded before substitution

// Argument is another macro call
#define ADD(x, y) ((x) + (y))
#define DOUBLE(x) ((x) * 2)
int f = ADD(DOUBLE(5), 1);  // ((((5) * 2)) + (1))

// Argument containing comma protected by parens
#define FIRST(x, ...) x
int g = FIRST((1, 2), 3);  // (1, 2)

// Unprotected comma splits arguments
// int h = FIRST(1, 2, 3);  // x=1, variadic=2,3

// Stringified argument - single arg only
#define STR(x) #x
char *i = STR(hello);  // "hello"

// Function-like with zero args vs one empty arg
#define ZERO_OR_ONE(...) (0 __VA_ARGS__)
int j1 = ZERO_OR_ONE();  // (0 )
int j2 = ZERO_OR_ONE(+ 5);  // (0 + 5)

// Argument that looks like directive (should be fine)
#define LOOKS_LIKE_DIR(x) x
int k = LOOKS_LIKE_DIR(123);  // 123

// Very long argument
#define LONG(x) x
int l = LONG(1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10);

// Argument with operators
#define OP_ARG(x) (x)
int m = OP_ARG(1 << 2 | 3 & 4);

// Argument with strings
#define STR_ARG(x) x
char *n = STR_ARG("hello");

// Argument with character literals
#define CHAR_ARG(x) x
int o = CHAR_ARG('A');

// Recursive argument structure
#define REC_ARG(x) (x)
int p = REC_ARG(REC_ARG(REC_ARG(5)));

// Whitespace in argument
#define WS_ARG(x) x
int q = WS_ARG(   5   );  // Should normalize to 5

// Comment in argument (becomes whitespace)
#define COMMENT_ARG(x) x
int r = COMMENT_ARG(5 /* comment */ + 1);

int main(){ return 0; }
