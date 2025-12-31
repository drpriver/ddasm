// Test typedef with forward-declared struct
// This is a common C idiom where typedef appears before struct definition

// Forward declare and typedef
typedef struct _object PyObject;

// Now define the actual struct
struct _object {
    int ob_refcnt;
    int ob_type;
};

// Use the typedef
int get_refcnt(PyObject *obj) {
    return obj->ob_refcnt;
}

int set_refcnt(PyObject *obj, int val) {
    obj->ob_refcnt = val;
    return val;
}

// Test with local variables
int test_local(void) {
    struct _object obj;
    PyObject *ptr = &obj;
    ptr->ob_refcnt = 42;
    ptr->ob_type = 1;
    return ptr->ob_refcnt + ptr->ob_type;  // 43
}

// Test union forward declaration
typedef union _data MyData;

union _data {
    int ival;
    char cval;
};

int get_ival(MyData *d) {
    return d->ival;
}
