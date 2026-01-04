// Test (6.5.15) conditional-expression (ternary operator)

int abs(int x) {
    return x > 0 ? x : -x;
}

int max(int a, int b) {
    return a > b ? a : b;
}

int min(int a, int b) {
    return a < b ? a : b;
}

int clamp(int x, int lo, int hi) {
    return x < lo ? lo : x > hi ? hi : x;
}

int nested(int x) {
    return x > 10 ? 100 : x > 5 ? 50 : 0;
}

// Ternary with side effects
int ternary_call(int cond) {
    return cond ? max(1, 2) : min(3, 4);
}
int main(){ return 0; }
