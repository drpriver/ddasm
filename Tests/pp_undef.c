// #undef edge cases

// Basic undef
#define FOO 1
int a = FOO;  // 1
#undef FOO
// int b = FOO;  // Error or unexpanded

// Redefine after undef
#define BAR 10
int c = BAR;  // 10
#undef BAR
#define BAR 20
int d = BAR;  // 20

// Undef something that wasn't defined
#undef NEVER_DEFINED  // Should be okay, no-op

// Undef and redefine as different type
#define MORPH 100
int e = MORPH;
#undef MORPH
#define MORPH(x) ((x) * 2)
int f = MORPH(5);  // 10

// Undef in middle of expansion (doesn't affect current expansion)
#define EXPAND_THEN_UNDEF 42
int g = EXPAND_THEN_UNDEF;  // 42 (expansion happens before undef below)
#undef EXPAND_THEN_UNDEF

// Chain of undef/define
#define CHAIN_VAL 1
int h1 = CHAIN_VAL;
#undef CHAIN_VAL
#define CHAIN_VAL 2
int h2 = CHAIN_VAL;
#undef CHAIN_VAL
#define CHAIN_VAL 3
int h3 = CHAIN_VAL;

// Undef in conditional
#define COND_DEF 1
#ifdef COND_DEF
int i = 1;
#undef COND_DEF
#endif
#ifndef COND_DEF
int j = 2;
#endif

// Undef function-like macro
#define FUNC_MACRO(x) (x)
int k = FUNC_MACRO(5);
#undef FUNC_MACRO
// int l = FUNC_MACRO(5);  // Error

// Undef with same name as keyword (legal)
#define int INT_TYPE
#undef int
int m = 0;  // int is keyword again

// Undef predefined macro (implementation-defined)
// #undef __LINE__  // Might not be allowed

// Multiple undefs of same thing
#define MULTI_UNDEF 1
#undef MULTI_UNDEF
#undef MULTI_UNDEF  // Second undef of undefined - should be ok
#undef MULTI_UNDEF  // Third

// Undef inside disabled #if
#define KEEP_THIS 1
#if 0
#undef KEEP_THIS  // Skipped
#endif
int n = KEEP_THIS;  // 1 (undef was skipped)

int main(){ return 0; }
