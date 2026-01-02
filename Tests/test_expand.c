// Test __EXPAND__

// Basic expansion
int x = __EXPAND__("1 + 2");

// Expand identifier
__EXPAND__("int y = 42;")

// Expand from macro
#define CODE "int z = 100;"
__EXPAND__(CODE)

// Use in expression
int sum = __EXPAND__("10") + __EXPAND__("20");

// Test in #if
#define DEBUG_VAL "1"
#if __EXPAND__(DEBUG_VAL)
int debug_enabled = 1;
#endif

#define ZERO "0"
#if __EXPAND__(ZERO)
int should_not_appear = 1;
#endif

int main() { return 0; }
