// Test __COUNTER__(name) - named counter streams

// Global counter (no parens) - should still work
int a = __COUNTER__;  // 0
int b = __COUNTER__;  // 1
int c = __COUNTER__;  // 2

// Named counter - separate stream
int foo0 = __COUNTER__(foo);  // 0
int foo1 = __COUNTER__(foo);  // 1
int foo2 = __COUNTER__(foo);  // 2

// Different name - different stream
int bar0 = __COUNTER__(bar);  // 0
int bar1 = __COUNTER__(bar);  // 1

// Back to foo - continues from 2
int foo3 = __COUNTER__(foo);  // 3

// Global counter continues
int d = __COUNTER__;  // 3

// Another name
int baz0 = __COUNTER__(baz);  // 0

int main() { return 0; }
