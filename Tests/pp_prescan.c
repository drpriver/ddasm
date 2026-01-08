// Macro argument prescan edge cases
// Arguments are fully expanded BEFORE substitution (except for # and ##)

#define A 1
#define B 2
#define ADD(x, y) ((x) + (y))

// Normal prescan - args expand first
int a = ADD(A, B);  // ((1) + (2))

// Prescan with nested macro calls
#define DOUBLE(x) ((x) * 2)
int b = ADD(DOUBLE(3), A);  // ((((3) * 2)) + (1))

// Prescan blocked by #
#define STR(x) #x
char *c = STR(A);  // "A" (not "1")

// Prescan blocked by ##
#define PASTE(a, b) a ## b
int PASTE(A, B);  // AB (not 12)

// But XSTR pattern causes expansion then stringify
#define XSTR(x) STR(x)
char *d = XSTR(A);  // "1"

// Prescan with function-like macro as argument
#define F(x) (x + 1)
#define G(y) y
int e = G(F(5));  // (5 + 1)

// Prescan order matters
#define FIRST(a, b) a
#define SECOND(a, b) b
int f = FIRST(A, B);  // 1
int g = SECOND(A, B);  // 2

// Argument used multiple times - each gets expansion
#define USE_TWICE(x) ((x) + (x))
#define INC i++
int i = 0;
// int h = USE_TWICE(INC);  // Be careful - INC expands twice!

// Nested macro in argument position
#define OUTER2(m) m(5)
#define INNER2(x) (x * 2)
int j = OUTER2(INNER2);  // (5 * 2)

// Prescan with self-referential macro
#define SELF2 SELF2
#define TAKE(x) int x
TAKE(SELF2);  // declares int SELF2 (self-ref stops)

// Complex prescan chain
#define L1 L2
#define L2 L3
#define L3 42
#define CHAIN(x) x
int l = CHAIN(L1);  // 42

int main(){ return 0; }
