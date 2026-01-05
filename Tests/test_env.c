// Test __ENV__

// Basic environment variable (should exist)
const char* home = __ENV__("HOME");
const char* user = __ENV__("USER");
const char* path = __ENV__("PATH");

// Non-existent variable - empty string
const char* missing = __ENV__("DOES_NOT_EXIST_XYZ");

// Non-existent with default
const char* with_default = __ENV__("DOES_NOT_EXIST_XYZ", "fallback_value");

// Test in #if with __EXPAND__
#define ENV_VAL __ENV__("TEST_TRUTHY", "1")
#if __EXPAND__(ENV_VAL)
int env_truthy = 1;
#endif

#define ENV_VAL2 __ENV__("TEST_FALSY", "0")
#if __EXPAND__(ENV_VAL2)
int should_not_appear = 1;
#endif

int main() { return 0; }
