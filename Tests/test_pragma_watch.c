// Test #pragma watch
// Expected stderr output (for verification):
// - [watch] #define FOO 42
// - [watch] FOO -> 42
// - [watch] #define MAX(a, b) ...
// - [watch] MAX(1, 2) -> ...
// - [watch] #define BAR 100
// - [watch] #undef BAR
// - (no expand for BAR - filtered)
// - [watch] #define BAR 200
// - [watch] #undef BAR

// Watch a macro before it's defined (all events)
#pragma watch FOO

// Define the watched macro - should log definition
#define FOO 42

// Use the macro - should log expansion
int x = FOO;

// Watch a function-like macro before defining
#pragma watch MAX

#define MAX(a, b) ((a) > (b) ? (a) : (b))

// Use it - should log expansion with arguments
int y = MAX(1, 2);

// Test unwatch
#pragma unwatch FOO

// This should NOT log (unwatched)
int w = FOO;

// Test filtered watch - only define and undef, not expand
#pragma watch(define, undef) BAR
#define BAR 100
int a = BAR;  // should NOT log (expand filtered out)
#undef BAR

// Re-define after undef - should log again
#define BAR 200
int b = BAR;  // should NOT log (expand filtered out)
#undef BAR

int main() {
    return 0;
}
