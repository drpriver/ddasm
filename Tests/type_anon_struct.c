// Test anonymous structs and unions inside structs

// Anonymous struct inside struct (C11)
struct WithAnonStruct {
    int a;
    struct {
        int x;
        int y;
    };
    int b;
};

int test_anon_struct(void) {
    struct WithAnonStruct s;
    s.a = 1;
    s.x = 10;  // Access anonymous struct member directly
    s.y = 20;
    s.b = 2;
    return s.a + s.x + s.y + s.b;
}

// Anonymous union inside struct
struct WithAnonUnion {
    int tag;
    union {
        int i;
        char c;
    };
};

int test_anon_union(void) {
    struct WithAnonUnion u;
    u.tag = 1;
    u.i = 42;  // Access anonymous union member directly
    return u.tag + u.i;
}

// Named anonymous struct (embedded struct with name)
struct WithNamedAnon {
    int a;
    struct {
        int x;
        int y;
    } point;
    int b;
};

int test_named_anon_struct(void) {
    struct WithNamedAnon s;
    s.a = 1;
    s.point.x = 10;  // Access via member name
    s.point.y = 20;
    s.b = 2;
    return s.a + s.point.x + s.point.y + s.b;
}

// Named anonymous union
struct WithNamedAnonUnion {
    int tag;
    union {
        int i;
        long l;
    } value;
};

int test_named_anon_union(void) {
    struct WithNamedAnonUnion u;
    u.tag = 1;
    u.value.i = 42;
    return u.tag + u.value.i;
}

// Nested anonymous structs
struct NestedAnon {
    struct {
        struct {
            int deep;
        };
        int mid;
    };
    int outer;
};

int test_nested_anon(void) {
    struct NestedAnon n;
    n.deep = 1;
    n.mid = 2;
    n.outer = 3;
    return n.deep + n.mid + n.outer;
}
int main(){
    test_anon_struct();
    test_anon_union();
    test_named_anon_struct();
    test_named_anon_union();
    test_nested_anon();
    return 0;
}
