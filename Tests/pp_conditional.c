// Conditional compilation edge cases

// Basic #if with constant
#if 1
int a = 1;
#endif

#if 0
int should_not_exist_1;
#endif

// #if with expression
#if 1 + 1 == 2
int b = 2;
#endif

// Nested conditionals
#if 1
    #if 1
        int c = 3;
    #endif
#endif

// #elif chain
#if 0
int bad1;
#elif 0
int bad2;
#elif 1
int d = 4;
#elif 1
int bad3;  // Should not appear
#endif

// defined() operator
#define FEATURE_X
#if defined(FEATURE_X)
int e = 5;
#endif

#if defined(FEATURE_Y)
int should_not_exist_2;
#endif

// defined without parens
#if defined FEATURE_X
int f = 6;
#endif

// !defined
#if !defined(FEATURE_Y)
int g = 7;
#endif

// Complex defined expressions
#define A_DEF
#define B_DEF
#if defined(A_DEF) && defined(B_DEF)
int h = 8;
#endif

#if defined(A_DEF) || defined(NONEXISTENT)
int i = 9;
#endif

// Arithmetic in #if
#if 2 * 3 + 1 == 7
int j = 10;
#endif

// Bitwise in #if
#if (0xFF & 0x0F) == 0x0F
int k = 11;
#endif

// Comparison operators
#if 5 > 3
int l = 12;
#endif

#if 3 >= 3
int m = 13;
#endif

// Ternary in #if
#if 1 ? 1 : 0
int n = 14;
#endif

// Undefined macro in #if evaluates to 0
#if UNDEFINED_MACRO
int should_not_exist_3;
#endif

#if !UNDEFINED_MACRO
int o = 15;
#endif

// #ifdef / #ifndef
#ifdef FEATURE_X
int p = 16;
#endif

#ifndef FEATURE_Y
int q = 17;
#endif

// Nested #else
#if 0
int bad4;
#else
    #if 0
    int bad5;
    #else
    int r = 18;
    #endif
#endif

// Edge: empty #if body
#if 1
#endif

// Negative numbers
#if -1
int s = 19;  // -1 is non-zero, so true
#endif

// Zero
#if 0
int should_not_exist_4;
#endif

// Character constants in #if
#if 'A' == 65
int t = 20;
#endif

// Macro expansion in #if
#define VALUE 42
#if VALUE == 42
int u = 21;
#endif

#define EXPR (1 + 2)
#if EXPR == 3
int v = 22;
#endif

int main(){ return 0; }
