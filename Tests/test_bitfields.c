// Test comma-separated bitfield declarations

struct TestBitfields {
    int x:1, y:2, z:3;
    unsigned int a:4, b:5;
    int regular_field;
};

int main() {
    struct TestBitfields t;
    t.x = 1;
    t.y = 2;
    return 0;
}
