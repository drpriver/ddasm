// SKIP: type-generic macros not supported
// Test <tgmath.h> - Type-generic math (C99)
#pragma watch(define) __MATHCALL
#pragma watch(define) __MATHDECL
#pragma watch(define) __MATHDECL_1
#pragma watch(define) __MATH_PRECNAME
#include <tgmath.h>


int test_tgmath(void) {
    return 0;
}
