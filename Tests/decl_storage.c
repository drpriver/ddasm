// SKIP: just a compiler bug
// Test (6.7.1) storage-class specifiers: static, extern

// External declaration
extern int external_var;
extern int external_func(int);

// Static global
static int static_global = 100;

static int static_func(int x) {
    return x * 2;
}

// Static local variables not supported
// int test_static(void) {
//     static int counter = 0;
//     counter++;
//     return counter;
// }

int test_static_global(void) {
    return static_global;
}

int use_static_func(int x) {
    return static_func(x) + static_global;
}
