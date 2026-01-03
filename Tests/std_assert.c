// SKIP: need __PRETTY_FUNCTION__
// Test <assert.h> - Diagnostics
#include <assert.h>

# define myassert(expr) ((void) sizeof ((expr) ? 1 : 0), __extension__ ({			\
      if (expr)								\
        ; /* empty */							\
      else								\
        __assert_fail (#expr, __FILE__, __LINE__, __ASSERT_FUNCTION);	\
    }))
static_assert(1, "");
_Static_assert(1, "");
int test_assert(void) {
    assert(1);
    static_assert(1, "");
    _Static_assert(1, "");
    // assert macro is defined
    return 0;
}
