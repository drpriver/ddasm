// Test (6.7.2.1) struct definitions and usage

struct Point {
    int x;
    int y;
};

struct Rect {
    struct Point origin;
    int width;
    int height;
};

int test_struct_local(void) {
    struct Point p;
    p.x = 10;
    p.y = 20;
    return p.x + p.y;
}

int test_struct_param(struct Point p) {
    return p.x * p.y;
}

struct Point* test_struct_ptr(struct Point* p) {
    p->x = 100;
    p->y = 200;
    return p;
}

int test_nested_struct(struct Rect r) {
    return r.origin.x + r.origin.y + r.width + r.height;
}

// Struct with pointer member
struct Node {
    int value;
    struct Node* next;
};

int test_linked_node(struct Node* n) {
    // Chained ->-> not fully supported in codegen
    // Workaround: use intermediate pointer
    struct Node* next = n->next;
    if (next) {
        return n->value + next->value;
    }
    return n->value;
}
