// Basic typedef - specifier order variations
int typedef Int;
typedef int Int2;
typedef signed long Long;
unsigned typedef int UInt;

// Pointer typedefs
typedef int* IntPtr;
typedef int** IntPtrPtr;
typedef const int* ConstIntPtr;

// Array typedefs
typedef int Arr5[5];
typedef int Arr2x3[2][3];

// Function pointer typedefs
typedef int (*BinOp)(int, int);
typedef void (*VoidFn)(void);
typedef int (*UnaryOp)(int);

// Typedef of typedef
typedef Int MyInt;
typedef IntPtr MyIntPtr;

// Struct typedefs
typedef struct { int x; int y; } Point;
typedef struct Node { int val; struct Node* next; } Node;

// Typedef with qualifiers
typedef const int ConstInt;
typedef volatile int VolatileInt;

// Complex declarators: pointer to array
typedef int (*PtrToArr5)[5];

// Array of function pointers
typedef int (*FnPtrArr[3])(int);

// Function returning pointer
typedef int* (*FnReturningPtr)(void);

// Test functions using typedefs
int add(int a, int b) { return a + b; }
int sub(int a, int b) { return a - b; }
int dbl(int x) { return x * 2; }

Int main(){
    Int y = 0;
    int err = 0;

    // Basic typedef usage
    Int2 a = 42;
    if(a != 42) err = 1;

    Long b = 100;
    if(b != 100) err = 2;

    UInt c = 200;
    if(c != 200) err = 3;

    // Pointer typedef
    int val = 5;
    IntPtr p = &val;
    if(*p != 5) err = 4;

    IntPtrPtr pp = &p;
    if(**pp != 5) err = 5;

    // Array typedef
    Arr5 arr = {1, 2, 3, 4, 5};
    if(arr[2] != 3) err = 6;

    Arr2x3 arr2d = {{1,2,3}, {4,5,6}};
    if(arr2d[1][2] != 6) err = 7;

    // Function pointer typedef
    BinOp op = add;
    if(op(3, 4) != 7) err = 8;

    op = sub;
    if(op(10, 3) != 7) err = 9;

    // Typedef of typedef
    MyInt mi = 99;
    if(mi != 99) err = 10;

    // Struct typedef
    Point pt = {10, 20};
    if(pt.x != 10 || pt.y != 20) err = 11;

    // sizeof with typedef
    if(sizeof(Int) != sizeof(int)) err = 12;
    if(sizeof(Arr5) != 5 * sizeof(int)) err = 13;

    // Cast with typedef
    long big = 1000;
    Int casted = (Int)big;
    if(casted != 1000) err = 14;

    // Compound literal with typedef
    Point pt2 = (Point){30, 40};
    if(pt2.x != 30 || pt2.y != 40) err = 15;

    // Typedef in expression after shadowing
    {
        // Test: local variable shadows typedef in declarations
        int Int = 10;  // variable named 'Int' shadows the typedef

        // Test: shadowed name in expression context
        // (Int) + 1 should be grouped expression (10) + 1 = 11, NOT a cast
        int x = (Int) + 1;
        if(x != 11) err = 16;

        // After scope ends, Int is typedef again
    }

    // Int is typedef again here
    Int restored = 77;
    if(restored != 77) err = 17;

    // Pointer to array typedef
    int arr3[5] = {10, 20, 30, 40, 50};
    PtrToArr5 pa = &arr3;
    if((*pa)[2] != 30) err = 18;

    // Array of function pointers
    FnPtrArr fns = {dbl, dbl, dbl};
    if(fns[0](5) != 10) err = 19;

    // Const typedef
    ConstInt ci = 123;
    if(ci != 123) err = 20;

    // Multiple levels of typedef
    MyIntPtr mip = &val;
    if(*mip != 5) err = 21;

    // Typedef used as function parameter type (tested via BinOp)
    // Typedef used as return type (Int main)

    if(err) return err;
    return y;
}
