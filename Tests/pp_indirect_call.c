// Test indirect function macro calls
// When an object macro expands to a function macro name, followed by ()

#define INNER(x) ((x) * 2)
#define OUTER INNER

// OUTER expands to INNER, then INNER(5) should be invoked
int a = OUTER(5);  // Expected: ((5) * 2)

// Multi-level indirection
#define L1 L2
#define L2 L3
#define L3(x) ((x) + 100)
int b = L1(1);  // Expected: ((1) + 100)

// Indirect through paste
#define PASTE(a, b) a ## b
#define FUNC(x) (x)
int c = PASTE(FU, NC)(42);  // Expected: (42)

// Object macro to function macro with multiple args
#define ADD(x, y) ((x) + (y))
#define INDIRECT_ADD ADD
int d = INDIRECT_ADD(1, 2);  // Expected: ((1) + (2))

// Chain of indirection
#define A1 A2
#define A2 A3
#define A3 A4
#define A4(x) (x)
int e = A1(99);  // Expected: (99)

// Indirect variadic
#define VA_FUNC(...) (__VA_ARGS__)
#define INDIRECT_VA VA_FUNC
int f = INDIRECT_VA(1 + 2);  // Expected: (1 + 2)

// Self-reference doesn't prevent indirect call
#define SELF_NAME SELF_NAME
#define SELF_NAME_FUNC(x) (x)
// The name SELF_NAME expands to SELF_NAME (painted)
// So SELF_NAME(5) -> SELF_NAME(5) stays

// But this should work:
#define OTHER_NAME WORKS
#define WORKS(x) (x)
int g = OTHER_NAME(10);  // Expected: (10)

int main(){ return 0; }
