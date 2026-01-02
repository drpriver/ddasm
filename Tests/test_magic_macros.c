// Test magic macros

// __COUNTER__ - auto-incrementing
int a = __COUNTER__;  // 0
int b = __COUNTER__;  // 1
int c = __COUNTER__;  // 2

// __INCLUDE_DEPTH__ - should be 0 at top level
int depth_here = __INCLUDE_DEPTH__;

// Include a file to test depth changes
#pragma include_path push "include_test"
#include "depth_test.h"
#pragma include_path pop

// __BASE_FILE__ - should be this file even after includes
const char* base = __BASE_FILE__;

// __FILE__ vs __BASE_FILE__
const char* current = __FILE__;

// __DIR__ - directory of current file
const char* dir = __DIR__;

// __RANDOM__ - random numbers (will be different each expansion)
int r1 = __RANDOM__;
int r2 = __RANDOM__;
int r3 = __RANDOM__;

int main() { return 0; }
