// Test (6.7.2.1) union definitions and usage

union IntOrPtr {
    int i;
    void* p;
};

union Data {
    int int_val;
    char char_val;
    long long_val;
};

int test_union_int(void) {
    union IntOrPtr u;
    u.i = 42;
    return u.i;
}

void* test_union_ptr(void* p) {
    union IntOrPtr u;
    u.p = p;
    return u.p;
}

int test_union_size(void) {
    return sizeof(union Data);
}

int test_union_param(union Data d) {
    return d.int_val;
}
int main(){
    test_union_int();
    test_union_ptr(1);
    test_union_size();
    test_union_param(1);
    return 0;
}
