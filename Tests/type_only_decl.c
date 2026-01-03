// Test type-only declarations (defining type without declaring variable)

int test_enum_only() {
    // Define enum without variable
    enum Direction {
        NORTH = 0,
        SOUTH = 1,
        EAST = 2,
        WEST = 3
    };

    // Use enum in separate declaration
    enum Direction d = EAST;
    return d;
}

int test_struct_only() {
    // Define struct without variable
    struct Point {
        int x;
        int y;
    };

    // Use struct in separate declaration
    struct Point p;
    p.x = 10;
    p.y = 20;
    return p.x + p.y;
}

int main() {
    if (test_enum_only() != 2) return 1;
    if (test_struct_only() != 30) return 2;
    return 0;
}
