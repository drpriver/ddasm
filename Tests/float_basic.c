// Basic float operations test
double add(double a, double b) { return a + b; }
double sub(double a, double b) { return a - b; }
double mul(double a, double b) { return a * b; }
double div_f(double a, double b) { return a / b; }
double neg(double x) { return -x; }

int cmp_lt(double a, double b) { return a < b; }
int cmp_le(double a, double b) { return a <= b; }
int cmp_gt(double a, double b) { return a > b; }
int cmp_ge(double a, double b) { return a >= b; }
int cmp_eq(double a, double b) { return a == b; }
int cmp_ne(double a, double b) { return a != b; }

int to_int(double x) { return (int)x; }
double from_int(int x) { return (double)x; }

int main() {
    double x = 3.14159;
    double y = 2.71828;
    float f = 1.5f;  // float suffix
    
    double sum = add(x, y);
    double diff = sub(x, y);
    double prod = mul(x, y);
    double quot = div_f(x, y);
    double n = neg(x);
    
    int lt = cmp_lt(x, y);
    int gt = cmp_gt(x, y);
    
    int i = to_int(x);
    double d = from_int(42);
    
    return 0;
}
