// Test (6.5.2) postfix-expression
// array subscript, function call, member access, ++, --

struct Point {
    int x;
    int y;
};

int add(int a, int b) { return a + b; }
int negate(int x) { return -x; }

int test_subscript(int* arr) {
    int first = arr[0];
    int second = arr[1];
    arr[2] = first + second;
    return arr[2];
}

int test_call(int x) {
    int result = add(x, 10);
    result = negate(result);
    return add(add(1, 2), add(3, 4));
}

int test_member_dot(struct Point p) {
    int sum = p.x + p.y;
    return sum;
}

int test_member_arrow(struct Point* p) {
    p->x = 10;
    p->y = 20;
    return p->x + p->y;
}

int test_postfix_incr(int x) {
    int a = x++;
    int b = x--;
    return a + b;
}
