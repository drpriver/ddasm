// Stringification edge cases
#pragma library("libc")
int strcmp(const char*, const char*);
#ifdef __DDASM__
#define FAIL(cond) __dasm {\
    msg __FILE__ ":" XSTR(__LINE__) ":" #cond " failed"; \
    dump; \
    abort; \
}
#else
void abort(void);
#define FAIL(cond) abort()
#endif
#define assert(cond) if(!(cond)) {FAIL(cond);}
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

// Stringify with backslashes (not escaped - only quotes/backslashes in strings are)
char *f = STR(path\to\file);  // "path\to\file"

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

// Stringify with comma (needs variadic)
#define STRV(...) #__VA_ARGS__
char *o = STRV(a, b, c);  // "a, b, c"

// Stringify special characters
char *p = STR(!@#$%);  // "!@#$%"

// Stringify with ## in argument
char *q = STR(a ## b);  // "a ## b"

int main(){ 
    assert(strcmp(a, "hello") == 0);
    assert(strcmp(b, "hello world") == 0);
    assert(strcmp(c, "123") == 0);
    assert(strcmp(d, "1 + 2") == 0);
    assert(strcmp(e, "say \"hello\"") == 0);
    assert(strcmp(f, "path\to\file") == 0);
    assert(strcmp(g, "VALUE") == 0);
    assert(strcmp(h, "42") == 0);
    assert(strcmp(i, "") == 0);
    assert(strcmp(j, "a b") == 0);
    assert(strcmp(k, "a + b * c") == 0);
    assert(strcmp(l, "(a)(b)(c)") == 0);
    assert(strcmp(m, "CALL(func, 1)") == 0);
    assert(strcmp(n, "STR(test)") == 0);
    assert(strcmp(o, "a, b, c") == 0);
    assert(strcmp(p, "!@#$%") == 0);
    assert(strcmp(q, "a ## b") == 0);
    return 0; 
}
