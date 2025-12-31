// Test (6.7.6) pointer declarators

int* global_ptr;
int** global_ptr_ptr;

int* test_pointer_return(int* p) {
    return p;
}

int test_pointer_local(void) {
    int x = 42;
    int* p = &x;
    int** pp = &p;
    return **pp;
}

int test_const_pointer(const int* p) {
    return *p;
}

void* test_void_pointer(int* p) {
    void* v = p;
    return v;
}
