// Test <stdbool.h> - Boolean type
#include <stdbool.h>

bool test_bool(bool a, bool b) {
    return a && b;
}
int main(){
    test_bool(1, 1);
    return 0;
}
