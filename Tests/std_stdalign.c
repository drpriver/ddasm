// Test <stdalign.h> - Alignment (C11)
#include <stdalign.h>

struct S {
    char c;
    int x;
    double d;
};

int test_alignof_type(void) {
    return alignof(int);  // 4
}

int test_alignof_struct(void) {
    return alignof(struct S);  // 8 (double alignment)
}

int test_alignof_expr(void) {
    int x;
    return _Alignof(x);  // 4 (GNU extension)
}
int main(){
    test_alignof_type();
    test_alignof_struct();
    test_alignof_expr();
    return 0;
}
