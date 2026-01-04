// Test (6.5.17) comma operator

int side_effect_counter;

int with_side_effect(int x) {
    side_effect_counter = x;
    return x * 2;
}

int test_simple_comma(void) {
    int x;
    x = (1, 2, 3);  // Result is 3
    return x;
}

int test_comma_side_effects(void) {
    int a = 0;
    int b = 0;
    int result = (a = 5, b = 10, a + b);
    return result;  // Should be 15
}

int test_comma_in_call(void) {
    return with_side_effect((1, 2, 3));
}

// Comma in for loop (common idiom)
int test_comma_in_for(int n) {
    int sum = 0;
    int i;
    for (i = 0; i < n; i++, sum += i) {
    }
    return sum;
}
int main(){
    test_simple_comma();
    test_comma_side_effects();
    test_comma_in_call();
    test_comma_in_for(1);
    return 0;
}
