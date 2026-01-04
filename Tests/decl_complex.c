// Test (6.7.6) complex declarators

// Pointer to function
int (*fn_ptr)(int, int);

// Function returning pointer
int* get_ptr(int* p) {
    return p;
}

// Array of pointers
int* ptr_arr[5];

// Pointer to array
int (*arr_ptr)[10];

int test_fn_ptr(void) {
    int a = 3;
    int b = 4;
    return 0;
}

// Multiple levels of indirection
int*** triple_ptr;

// Const pointer to const
const int* const* cpp;
int main(){
    test_fn_ptr();
    return 0;
}
