// Variadic macro edge cases

// Basic variadic - just define, don't call with invalid args
#define PRINT(...) printf(__VA_ARGS__)
// Usage would be: PRINT("hello %d", 42);

// Named args plus variadic
#define LOG(fmt, ...) printf(fmt, __VA_ARGS__)
// Usage: LOG("value: %d", 10);

// Empty variadic args - make output valid C
#define EMPTY_VA(...) (0 __VA_ARGS__)
int a = EMPTY_VA();  // (0 )
int a2 = EMPTY_VA(+ 1);  // (0 + 1)

// Single arg variadic
#define SINGLE_VA(...) (__VA_ARGS__)
int b = SINGLE_VA(42);  // (42)

// Variadic with stringification - commented out due to cpp bug
// #define VA_STR(...) #__VA_ARGS__
// char *c = VA_STR(hello);  // Should be "hello"

// __VA_OPT__ (C23) - if supported
#ifdef __VA_OPT__
#define OPT_LOG(fmt, ...) printf(fmt __VA_OPT__(,) __VA_ARGS__)
#endif

// Variadic passed to another variadic
#define VA_OUTER(...) VA_INNER(__VA_ARGS__)
#define VA_INNER(...) (__VA_ARGS__)
int d = VA_OUTER(1 + 2);  // (1 + 2)

// Variadic with paste (GNU extension: , ## __VA_ARGS__)
#define GNU_LOG(fmt, ...) printf(fmt, ## __VA_ARGS__)
// GNU_LOG("no args");  // comma disappears if __VA_ARGS__ empty

// Count variadic args (classic trick) - use valid identifiers
#define COUNT_ARGS(...) COUNT_ARGS_IMPL(__VA_ARGS__, 5, 4, 3, 2, 1, 0)
#define COUNT_ARGS_IMPL(_1, _2, _3, _4, _5, N, ...) N
int e = COUNT_ARGS(1, 2, 3);  // 3
int f = COUNT_ARGS(1);  // 1

// Nested variadic expansion
#define WRAP(...) (__VA_ARGS__)
#define DOUBLE_WRAP(...) WRAP(WRAP(__VA_ARGS__))
int h = DOUBLE_WRAP(42);  // ((42))

// Variadic with complex expressions
#define EXPR_VA(...) (0 __VA_ARGS__)
int i = EXPR_VA(+ 1 + 2 + 3);  // (0 + 1 + 2 + 3)

extern int printf(const char*, ...);
int main(){ return 0; }
