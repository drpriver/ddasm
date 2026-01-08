// Rescanning behavior tests
// After macro replacement, the result is rescanned for more macros

#define A 1
#define B A
int a = B;  // B->A->1

// Rescan finds function-like macro
#define FUNC(x) ((x))
#define NAME FUNC
int b = NAME(5);  // NAME->FUNC, then FUNC(5)->((5))

// Rescan with paste result
#define PASTE(a, b) a ## b
#define foobar 42
int c = PASTE(foo, bar);  // paste creates foobar, rescan expands to 42

// Rescan doesn't re-expand painted macro
#define PAINTED PAINTED
int PAINTED;  // PAINTED->PAINTED (second is painted, stays as identifier)

// Multiple rescans
#define L1 L2
#define L2 L3
#define L3 L4
#define L4 100
int e = L1;  // L1->L2->L3->L4->100

// Rescan with argument substitution
#define WRAP(x) x
#define INNER 50
int f = WRAP(INNER);  // WRAP substitutes INNER which is already expanded to 50

// Rescan of stringify result (no rescan - it's a string)
#define STR(x) #x
#define VAL 123
char *g = STR(VAL);  // "VAL" (# prevents expansion)

// Rescan order with multiple macros in result
#define MULTI A + B
int h = MULTI;  // A + B -> 1 + A (B->A, A painted) -> 1 + 1

// Actually let me reconsider: A and B are expanded independently
// MULTI -> A + B
// A -> 1
// B -> A -> 1 (B is not A, so A can expand)
// Result: 1 + 1

// Rescan with nested function calls
#define F(x) G(x)
#define G(x) (x)
int i = F(10);  // F(10)->G(10)->(10)

// Rescan after paste creates valid macro name
#define XY 999
#define MAKE(a, b) a ## b
int j = MAKE(X, Y);  // paste creates XY, rescan finds XY->999

// Paste creates function macro name
#define AB(x) (x * 10)
int k = MAKE(A, B)(5);  // paste creates AB, rescan with (5) gives (5 * 10)

// Rescan limit test
#define R1(x) R2(x, x)
#define R2(a, b) R3(a + b)
#define R3(x) (x)
int l = R1(1);  // R1(1)->R2(1,1)->R3(1+1)->(1+1)

// Deep chain
#define D1 D2
#define D2 D3
#define D3 D4
#define D4 D5
#define D5 D6
#define D6 D7
#define D7 D8
#define D8 D9
#define D9 D10
#define D10 42
int m = D1;  // 42

int main(){ return 0; }
