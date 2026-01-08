// Corner cases and obscure preprocessor behavior

// Empty object macro next to identifier
#define EMPTY
int EMPTYfoo;  // Should be: int foo; or int EMPTYfoo?

// Token paste with empty creating valid token
#define PASTE_E(a, b) a ## b
int PASTE_E(var, );  // var
int PASTE_E(, name);  // name

// Macro that produces another #define (doesn't work but test error handling)
// #define META #define X 1
// META  // Should not actually define X

// Macro with trailing backslash (continuation vs literal)
#define TRAILING 42  // No trailing backslash
int corner_a = TRAILING;

// Whitespace sensitivity
#define WS1(x) x
#define WS2( x ) x
int b = WS1(1);
int c = WS2(2);

// Macro redefinition with same definition (allowed)
#define SAME 1
#define SAME 1
int d = SAME;

// Macro redefinition with different definition (warning)
#undef SAME
#define SAME 2
int e = SAME;

// # at start vs in middle
#define HASH_START # not_special
// This is not stringification, # is literal in object macro

// Stringify empty
#define STR(x) #x
char *f = STR();  // ""

// Double ## (invalid)
// #define DOUBLE_HASH(a, b) a ## ## b  // Error

// ## at start or end
// #define START_HASH(a) ## a  // Error
// #define END_HASH(a) a ##    // Error

// Paste creating number
#define MAKE_NUM(a, b) a ## b
int g = MAKE_NUM(1, 23);  // 123

// Paste creating float
double h = MAKE_NUM(3, .14);  // 3.14

// Paste with L prefix for wide string
#define WIDE(s) L ## s
// wchar_t *w = WIDE("hello");  // L"hello"

// Nested stringify
#define S1(x) #x
#define S2(x) S1(x)
#define VAL 42
char *i = S1(S2(VAL));  // "S2(VAL)"
char *j = S2(VAL);      // "42"

// Conditional in macro argument (should work)
#define COND_ARG(x) x
#if 1
int k = COND_ARG(1);
#endif

// Variadic with zero args vs one empty arg
#define VA_TEST(...) __VA_ARGS__ ## _end
// int VA_TEST();  // _end or empty_end?

// Defined operator stringified
#define DEFINED_STR(x) #x
char *l = DEFINED_STR(defined(FOO));  // "defined(FOO)"

// __VA_ARGS__ outside variadic (error)
// #define BAD_VA __VA_ARGS__  // Error

// Recursive stringify (doesn't actually recurse)
#define RSTR(x) #x RSTR(x)
// char *m = RSTR(a);  // "a" RSTR(a) - RSTR painted

// Macro in #error/#warning
#define ERR_MSG "custom error"
// #error ERR_MSG  // Does ERR_MSG expand?

// Computed include (GNU extension)
// #define HEADER "stdio.h"
// #include HEADER

// Pragma in macro
#define PRAGMA_MACRO _Pragma("once")
// PRAGMA_MACRO  // Should emit #pragma once

// Adjacent string literals (concatenation happens later)
#define STR_A "hello"
#define STR_B "world"
char *n = STR_A STR_B;  // "hello" "world" -> "helloworld" in later phase

// Macro producing unbalanced tokens
#define OPEN (
#define CLOSE )
int o = OPEN 5 CLOSE;

// Parenthesis matching across macro boundaries
#define CALL_START (
#define CALL_END )
int corner_p = CALL_START 1 + 2 CALL_END;

int main(){ return 0; }
