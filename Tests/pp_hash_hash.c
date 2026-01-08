// Interactions between # (stringify) and ## (paste)

#define STR(x) #x
#define PASTE(a, b) a ## b
#define XSTR(x) STR(x)

// Stringify of paste result
#define STRINGIFY_PASTE(a, b) STR(a ## b)
char *a = STRINGIFY_PASTE(foo, bar);  // "foobar"

// Paste then stringify through indirection
#define PASTE_THEN_STR(a, b) XSTR(a ## b)
char *b = PASTE_THEN_STR(hello, world);  // Should this be "helloworld"?

// Multiple paste then stringify
#define MULTI_PASTE_STR(a, b, c) STR(a ## b ## c)
char *c = MULTI_PASTE_STR(x, y, z);  // "xyz"

// Stringify in paste (not really valid, but test behavior)
// #define STR_IN_PASTE(x) # ## x  // Invalid

// Paste creating stringification operator (not valid)
// #define MAKE_HASH a #
// #define TRY_HASH(x) MAKE_HASH x  // Doesn't work

// Adjacent # and ##
#define ADJACENT(a, b) #a ## #b
// This is: stringify(a) paste stringify(b)
// char *d = ADJACENT(foo, bar);  // "foo" ## "bar" -> invalid token?

// Stringify of macro that pastes
#define PASTER(x, y) x ## y
#define STR_PASTER(x, y) STR(PASTER(x, y))
char *e = STR_PASTER(a, b);  // "PASTER(a, b)" (# blocks expansion)

// Via XSTR to force expansion first
#define XSTR_PASTER(x, y) XSTR(PASTER(x, y))
char *f = XSTR_PASTER(a, b);  // "ab"

// Paste with stringified result
#define STR_VAL "hello"
#define PASTE_STR_VAL(prefix) prefix ## STR_VAL
// char *g = PASTE_STR_VAL(L);  // LSTR_VAL or L"hello"?

// Empty paste then stringify
#define EMPTY_PASTE_STR(a, b) STR(a ## b)
char *h = EMPTY_PASTE_STR(, suffix);  // "suffix"
char *i = EMPTY_PASTE_STR(prefix, );  // "prefix"

// Complex: paste creates identifier, which is then stringified
#define MAKE_NAME(a, b) a ## _ ## b
#define STR_NAME(a, b) STR(MAKE_NAME(a, b))
char *j = STR_NAME(foo, bar);  // "MAKE_NAME(foo, bar)"

// Force expansion for stringify
#define XSTR_NAME(a, b) XSTR(MAKE_NAME(a, b))
char *k = XSTR_NAME(foo, bar);  // "foo_bar"

int main(){ return 0; }
