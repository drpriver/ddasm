// Test __MIXIN__

// Basic expansion
int x = __MIXIN__("1 + 2");

// Expand identifier
__MIXIN__("int y = 42;")

// Expand from macro
#define CODE "int z = 100;"
__MIXIN__(CODE)

// Use in expression
int sum = __MIXIN__("10") + __MIXIN__("20");

// Test in #if
#define DEBUG_VAL "1"
#if __MIXIN__(DEBUG_VAL)
int debug_enabled = 1;
#endif

#define ZERO "0"
#if __MIXIN__(ZERO)
int should_not_appear = 1;
#endif

int main() { return 0; }
