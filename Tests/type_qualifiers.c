// Test (6.7.3) type qualifiers: const, volatile, restrict, _Atomic

int test_const_param(const int x) {
    return x + 1;
}

int test_const_ptr(const int* p) {
    return *p;
}

int* test_ptr_to_const(int* const p) {
    return p;
}

// Volatile (ignored but parsed)
int test_volatile(volatile int x) {
    return x;
}

// Restrict (ignored but parsed)
void test_restrict(int* restrict p, int* restrict q) {
    *p = *q;
}

// _Atomic as qualifier
int test_atomic_qual(_Atomic int x) {
    return x;
}

// _Atomic(type) form
int test_atomic_type(_Atomic(int) x) {
    return x;
}

// _Atomic with pointers
int test_atomic_ptr(_Atomic(void*) p) {
    return p != 0;
}
int main(){
    int x = 42;
    int y = 10;
    test_const_param(1);
    test_const_ptr(&x);
    test_ptr_to_const(&x);
    test_volatile(1);
    test_restrict(&x, &y);
    test_atomic_qual(1);
    test_atomic_type(1);
    test_atomic_ptr(0);
    return 0;
}
