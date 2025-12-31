// Test named embedded structs (not truly anonymous)

// Named embedded struct
struct Outer1 {
    int a;
    struct {
        int x;
        int y;
    } inner;
    int b;
};

int test_named_embedded(void) {
    struct Outer1 s;
    s.a = 1;
    s.inner.x = 10;
    s.inner.y = 20;
    s.b = 2;
    return s.a + s.inner.x + s.inner.y + s.b;
}

// Named embedded union
struct Outer2 {
    int tag;
    union {
        int i;
        long l;
    } value;
};

int test_named_embedded_union(void) {
    struct Outer2 u;
    u.tag = 1;
    u.value.i = 42;
    return u.tag + u.value.i;
}

// Multiple named embedded structs
struct Multi {
    struct { int x; int y; } pos;
    struct { int w; int h; } size;
};

int test_multi_embedded(void) {
    struct Multi m;
    m.pos.x = 1;
    m.pos.y = 2;
    m.size.w = 10;
    m.size.h = 20;
    return m.pos.x + m.pos.y + m.size.w + m.size.h;
}
