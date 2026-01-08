// Tricky/corner-case preprocessor tests

// Empty macro
#define EMPTY
int EMPTY tricky_a EMPTY = EMPTY 1 EMPTY;

// Macro expanding to another macro definition (doesn't define it)
#define NOT_A_DEF #define FOO 1
// NOT_A_DEF  // This would produce literal "#define FOO 1" tokens

// Macro that looks like a function call but isn't
#define NOTFUNC (1 + 2)
int tricky_b = NOTFUNC;  // (1 + 2)

// Macro name same as parameter name
#define X(X) (X)
int tricky_c = X(5);  // (5)

// Parenthesized macro name
#define PAREN 10
int tricky_d = (PAREN);  // (10) - parens not part of macro

// Comment in macro (should be whitespace)
#define WITH_COMMENT tricky_ab
int WITH_COMMENT;

// Multiline macro with backslash
#define MULTILINE 1 + \
                  2 + \
                  3
int tricky_e = MULTILINE;

// Macro producing unbalanced parens/braces (legal)
#define OPEN_PAREN (
#define CLOSE_PAREN )
int tricky_f = OPEN_PAREN 5 CLOSE_PAREN;  // ( 5 )

// Redefining a macro (should warn in strict mode but work)
#define REDEF 1
#undef REDEF
#define REDEF 2
int tricky_g = REDEF;  // 2

// Stringifying a macro that contains quotes
#define HAS_QUOTE say "hi"
#define S(x) #x
char *tricky_h = S(HAS_QUOTE);  // "HAS_QUOTE"

// Token pasting creating a string prefix
#define PREFIX(p, s) p ## s
// char* tricky_i = PREFIX(L, "wide");  // L"wide"

// Macro with same name as its expansion (not self-reference)
#define FOO BAR
#define BAR FOO_VAL
#define FOO_VAL 100
int tricky_j = FOO;  // 100 (FOO->BAR->FOO_VAL->100)

// Function macro with no args vs object macro with parens
#define FUNC() 1
#define OBJ (0)
int tricky_k = FUNC();  // 1
int tricky_l = OBJ;     // (0)

// Distinguishing function-like from object-like
#define MAYBE_FUNC(x) x
int tricky_m = MAYBE_FUNC(10);  // Works: 10
// int tricky_n = MAYBE_FUNC;  // Error or remains MAYBE_FUNC

// Argument spanning multiple lines (with continuation)
#define LONG_ARG(x) x
int tricky_o = LONG_ARG(1 + \
                 2 + \
                 3);

// Hexadecimal and octal in expressions
#if 0x10 == 16
int tricky_p = 1;
#endif

#if 010 == 8
int tricky_q = 2;
#endif

// Macro as part of another token (doesn't expand)
#define AB 99
int pABq = 0;  // AB doesn't expand - it's part of pABq

// L-value macro
#define LVAL arr[0]
int arr[1];
// LVAL = 5;  // Valid if macro expands to l-value

// Sizeof in macro
#define SIZE sizeof(int)
int tricky_r = SIZE;

int main(){ return 0; }
