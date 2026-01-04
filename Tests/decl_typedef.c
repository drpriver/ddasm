// Test (6.7.7) typedef declarations

typedef int Int;
typedef int* IntPtr;
typedef unsigned long size_t;

typedef struct Point {
    int x;
    int y;
} Point;

typedef enum Color {
    RED,
    GREEN,
    BLUE
} Color;

Int test_typedef_basic(Int x) {
    Int y = x + 1;
    return y;
}

int test_typedef_ptr(IntPtr p) {
    return *p;
}

int test_typedef_struct(Point p) {
    return p.x + p.y;
}

Color test_typedef_enum(void) {
    Color c = RED;
    return c;
}
int main(){
    int x = 42;
    Point p = {1, 2};
    test_typedef_basic(1);
    test_typedef_ptr(&x);
    test_typedef_struct(p);
    test_typedef_enum();
    return 0;
}
