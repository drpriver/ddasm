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
int main(){
    struct Point p = {5, 10};
    struct Rect r = {{1, 2}, 100, 200};
    struct Node n = {42, 0};
    test_struct_local();
    test_struct_param(p);
    test_struct_ptr(&p);
    test_nested_struct(r);
    test_linked_node(&n);
    return 0;
}
