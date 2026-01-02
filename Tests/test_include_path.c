// Test #pragma include_path

// Show initial include paths
#pragma include_path reveal

// This would fail without the push:
#pragma include_path push "include_test"
#include "myheader.h"
#pragma include_path pop

// Verify the header was included
#pragma eval defined(FROM_MYHEADER)

// Test pop warning
#pragma include_path pop

int main() { return 0; }
