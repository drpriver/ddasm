// Stress tests for preprocessor - deep nesting, long chains

// Deep nesting of function-like macros
#define N1(x) N2(x)
#define N2(x) N3(x)
#define N3(x) N4(x)
#define N4(x) N5(x)
#define N5(x) N6(x)
#define N6(x) N7(x)
#define N7(x) N8(x)
#define N8(x) N9(x)
#define N9(x) N10(x)
#define N10(x) (x)
int stress_a = N1(42);  // Should eventually become ((((((((((42))))))))))

// Long expansion chain
#define C1 C2
#define C2 C3
#define C3 C4
#define C4 C5
#define C5 C6
#define C6 C7
#define C7 C8
#define C8 C9
#define C9 C10
#define C10 100
int stress_b = C1;  // 100

// Recursive-looking but actually terminates
#define R1(x) R2(x + 1)
#define R2(x) R3(x + 1)
#define R3(x) R4(x + 1)
#define R4(x) (x)
int stress_c = R1(0);  // (((0 + 1) + 1) + 1)

// Complex token paste chain
#define P1(a, b) a ## b
#define P2(a, b) P1(a, b)
#define P3(a, b) P2(a, b)
int P3(vari, able);  // variable

// Multiple stringification levels
#define S1(x) #x
#define S2(x) S1(x)
#define S3(x) S2(x)
#define VAL 123
char *stress_d = S3(VAL);  // "123"

// Conditional nesting stress
#if 1
    #if 1
        #if 1
            #if 1
                #if 1
                    int stress_e = 5;
                #endif
            #endif
        #endif
    #endif
#endif

// Mixed paste and stringify
#define MIX_PASTE(a, b) a ## _ ## b
#define MIX_STR(x) #x
#define XMIX_STR(x) MIX_STR(x)
#define COMPLEX_MIX(a, b) XMIX_STR(MIX_PASTE(a, b))
char *stress_f = COMPLEX_MIX(hello, world);  // "hello_world"

// Variadic with many args
#define MANY(...) (__VA_ARGS__)
int stress_g = MANY(1);

// Indirect function-like macro invocation
#define INVOKE(m, arg) m(arg)
#define DOUBLE_IT(x) ((x) + (x))
int stress_i = INVOKE(DOUBLE_IT, 7);  // ((7) + (7))

// Edge: macro arg is macro name
#define APPLY(f, x) f(x)
#define INC(n) ((n) + 1)
int stress_j = APPLY(INC, 10);  // ((10) + 1)

// Stringify of paste result
#define SP(a, b) S1(a ## b)
char *stress_k = SP(foo, bar);  // "foobar"

// Comma in macro argument (protected by parens)
#define FIRST_ARG(x, ...) x
int stress_l = FIRST_ARG((1, 2), 3);  // (1, 2) - comma expr

int main(){ return 0; }
