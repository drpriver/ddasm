// Stringification edge cases

#define STR(x) #x
#define XSTR(x) STR(x)

// Basic stringification
char *a = STR(hello);  // "hello"

// Stringify with spaces
char *b = STR(hello world);  // "hello world"

// Stringify number
char *c = STR(123);  // "123"

// Stringify expression
char *d = STR(1 + 2);  // "1 + 2"

// Stringify with quotes inside (should escape)
char *e = STR(say "hello");  // "say \"hello\""

// Stringify with backslashes (should escape)
char *f = STR(path\to\file);  // "path\\to\\file"

// Stringify macro argument that would expand
#define VALUE 42
char *g = STR(VALUE);   // "VALUE" (# prevents expansion)
char *h = XSTR(VALUE);  // "42" (expands first, then stringify)

// Stringify empty
#define STR_EMPTY(x) #x
char *i = STR_EMPTY();  // ""

// Stringify with newline-like whitespace (multiple spaces)
char *j = STR(a    b);  // "a b" (whitespace should normalize)

// Stringify operators
char *k = STR(a + b * c);  // "a + b * c"

// Stringify with parentheses
char *l = STR((a)(b)(c));  // "(a)(b)(c)"

// Stringify entire macro call
#define CALL(f, x) f(x)
char *m = STR(CALL(func, 1));  // "CALL(func, 1)"

// Double stringify (weird but valid)
#define DSTR(x) STR(STR(x))
char *n = DSTR(test);  // "STR(test)"

// Stringify with comma
char *o = STR(a, b, c);  // "a, b, c"

// Stringify special characters
char *p = STR(!@#$%);  // "!@#$%"

// Stringify with ## in argument
char *q = STR(a ## b);  // "a ## b"

int main(){ return 0; }
