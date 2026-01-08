// Token pasting edge cases

// Basic paste
#define PASTE(a, b) a ## b
int PASTE(foo, bar);  // foobar

// Paste with numbers
#define VAR(n) var_ ## n
int VAR(1);   // var_1
int VAR(42);  // var_42

// Multiple pastes in sequence
#define PASTE3(a, b, c) a ## b ## c
int PASTE3(x, y, z);  // xyz

// Paste with empty argument (placemarker)
#define PASTE_EMPTY(a, b) a ## b
int PASTE_EMPTY(prefix, );  // Should be: prefix
int PASTE_EMPTY(, suffix);  // Should be: suffix

// Paste creating valid token from invalid parts
#define MAKE_ARROW(a, b) a ## b
int *p;
// p MAKE_ARROW(-, >);  // Should create ->

// Paste identifier with number
#define ID_NUM(id, num) id ## num
int ID_NUM(x, 123);  // x123

// Paste with underscores
#define UNDER(a) __ ## a ## __
int UNDER(foo);  // __foo__

// Double paste - ensuring left-to-right
#define DPASTE(a, b, c) a ## b ## c
int DPASTE(a, b, c);  // abc

// Paste in nested macro
#define INNER(x) inner_ ## x
#define OUTER(y) INNER(y)
int OUTER(test);  // Should be: inner_test

// Paste with macro argument that expands
#define EXPAND_PASTE(a, b) a ## b
#define VALUE val
int EXPAND_PASTE(pre_, VALUE);  // pre_VALUE (not pre_val - ## prevents expansion)

// Stringification followed by paste (tricky)
#define STR(x) #x
#define COMPLEX(a, b) STR(a ## b)
char *s = COMPLEX(hello, world);  // "helloworld"

// Paste that would create invalid token (implementation-defined)
// #define BAD_PASTE(a, b) a ## b
// BAD_PASTE(+, -)  // +-  is this valid?

// Nested ## operators
#define NEST_PASTE(a, b, c) a ## _ ## b ## _ ## c
int NEST_PASTE(x, y, z);  // x_y_z

// Paste with keywords
#define KEYWORD_PASTE(a) a ## _t
typedef int KEYWORD_PASTE(my);  // my_t

int main(){ return 0; }
