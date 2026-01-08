// Test macro recursion prevention (blue paint algorithm)
// These tests verify the preprocessor stops infinite recursion
// We use #define to create "valid" identifiers for the painted results

// Direct self-reference - should stop after one expansion
#define SELF SELF
int SELF;  // Declares variable named SELF

// Mutual/indirect recursion
#define A B
#define B A
int A;  // A->B->A stops, declares 'A'
int B;  // B->A->B stops, declares 'B'

// Three-way indirect recursion
#define X Y
#define Y Z
#define Z X
int X;  // X->Y->Z->X stops

// Self-reference in expansion with other tokens
#define RECURSE (RECURSE + 1)
// Can't use as initializer, just verify it expands
#define USE_RECURSE int recurse_test = RECURSE
// USE_RECURSE;  // Would be: int recurse_test = (RECURSE + 1)

// The classic "file" example
#define file stdio
#define stdio file
int file;  // file->stdio->file stops

// Multiple self-references in one expansion
#define DUP DUP ## DUP
int DUP;  // DUPDIP as identifier

// Test that non-recursive macros still work
#define NORMAL 42
int normal_val = NORMAL;

// Chain that terminates normally (not recursive)
#define C1 C2
#define C2 C3
#define C3 100
int chain_val = C1;  // 100

// Macro calling itself with different args doesn't recurse
#define FACTORIAL(n) ((n) <= 1 ? 1 : (n) * FACTORIAL((n)-1))
// Note: FACTORIAL stays as FACTORIAL in expansion (painted)

int main(){ return 0; }
