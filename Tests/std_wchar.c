// Test <wchar.h> - Wide character handling
#include <wchar.h>

size_t test_wchar(const wchar_t *s) {
    return wcslen(s);
}
