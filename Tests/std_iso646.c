// Test <iso646.h> - Alternative spellings
#include <iso646.h>

int test_iso646(int a, int b) {
    if (a and b) return 1;
    if (a or b) return 2;
    if (not a) return 3;
    return 0;
}
int main(){
    test_iso646(1, 1);
    return 0;
}
