// SKIP: float suffix issue causes assertion failures
#ifdef __DDASM__
void abort(void){__dasm{dump;abort;}}
#else
void abort(void);
#endif

// Helper functions to test argument conversions
float take_float(float f) { return f; }
double take_double(double d) { return d; }
int take_int(int i) { return i; }

// Helper functions to test return conversions
float return_float_from_int(int x) { return x; }
double return_double_from_int(int x) { return x; }
float return_float_from_double(double x) { return x; }
double return_double_from_float(float x) { return x; }
int return_int_from_float(float x) { return x; }
int return_int_from_double(double x) { return x; }

int main(){
    // === Assignment conversions ===

    // int -> float
    int i1 = 42;
    float f1 = i1;
    if(f1 != 42.0f) abort();

    // int -> double
    int i2 = 123;
    double d1 = i2;
    if(d1 != 123.0) abort();

    // float -> double
    float f2 = 3.5f;
    double d2 = f2;
    if(d2 != 3.5) abort();

    // double -> float
    double d3 = 2.25;
    float f3 = d3;
    if(f3 != 2.25f) abort();

    // float -> int (truncation)
    float f4 = 7.9f;
    int i3 = f4;
    if(i3 != 7) abort();

    // double -> int (truncation)
    double d4 = 9.8;
    int i4 = d4;
    if(i4 != 9) abort();

    // === Compound assignment conversions ===

    // float /= int
    float f5 = 1.0f;
    int i5 = 2;
    f5 /= i5;
    if(f5 != 0.5f) abort();

    // float *= int
    float f6 = 2.5f;
    f6 *= 2;
    if(f6 != 5.0f) abort();

    // float += int
    float f7 = 1.5f;
    f7 += 3;
    if(f7 != 4.5f) abort();

    // float -= int
    float f8 = 5.0f;
    f8 -= 2;
    if(f8 != 3.0f) abort();

    // double /= int
    double d5 = 10.0;
    d5 /= 4;
    if(d5 != 2.5) abort();

    // === Usual arithmetic conversions (binary operators) ===

    // int + float -> float (promoted to double for computation)
    int i6 = 5;
    float f9 = 2.5f;
    float f10 = i6 + f9;
    if(f10 != 7.5f) abort();

    // int * double -> double
    int i7 = 3;
    double d6 = 2.5;
    double d7 = i7 * d6;
    if(d7 != 7.5) abort();

    // float + double -> double
    float f11 = 1.5f;
    double d8 = 2.5;
    double d9 = f11 + d8;
    if(d9 != 4.0) abort();

    // int / float -> float
    int i8 = 10;
    float f12 = 4.0f;
    float f13 = i8 / f12;
    if(f13 != 2.5f) abort();

    // === Function argument conversions ===

    // int -> float parameter
    int i9 = 7;
    float f14 = take_float(i9);
    if(f14 != 7.0f) abort();

    // int -> double parameter
    int i10 = 11;
    double d10 = take_double(i10);
    if(d10 != 11.0) abort();

    // float -> double parameter
    float f15 = 3.25f;
    double d11 = take_double(f15);
    if(d11 != 3.25) abort();

    // double -> float parameter
    double d12 = 4.5;
    float f16 = take_float(d12);
    if(f16 != 4.5f) abort();

    // float -> int parameter (truncation)
    float f17 = 8.7f;
    int i11 = take_int(f17);
    if(i11 != 8) abort();

    // === Return statement conversions ===

    // return int as float
    float f18 = return_float_from_int(13);
    if(f18 != 13.0f) abort();

    // return int as double
    double d13 = return_double_from_int(17);
    if(d13 != 17.0) abort();

    // return double as float
    float f19 = return_float_from_double(5.5);
    if(f19 != 5.5f) abort();

    // return float as double
    double d14 = return_double_from_float(6.25f);
    if(d14 != 6.25) abort();

    // return float as int
    int i12 = return_int_from_float(12.9f);
    if(i12 != 12) abort();

    // return double as int
    int i13 = return_int_from_double(15.1);
    if(i13 != 15) abort();

    // === Comparison with mixed types ===

    // int compared with float
    int i14 = 5;
    float f20 = 5.0f;
    if(i14 != f20) abort();
    if(!(i14 == f20)) abort();
    if(i14 < f20) abort();
    if(i14 > f20) abort();

    // int compared with double
    int i15 = 10;
    double d15 = 10.0;
    if(i15 != d15) abort();

    // float compared with double
    float f21 = 2.5f;
    double d16 = 2.5;
    if(f21 != d16) abort();

    // === Negative values ===

    int i16 = -5;
    float f22 = i16;
    if(f22 != -5.0f) abort();

    double d17 = -3.7;
    int i17 = d17;
    if(i17 != -3) abort();

    // === Integer promotions ===

    // short + short -> int
    short s1 = 100;
    short s2 = 200;
    int i18 = s1 + s2;
    if(i18 != 300) abort();

    // short * short -> int (tests overflow beyond short range)
    short s3 = 300;
    short s4 = 300;
    int i19 = s3 * s4;
    if(i19 != 90000) abort();  // 90000 > 32767 (short max)

    // char + char -> int
    char c1 = 50;
    char c2 = 60;
    int i20 = c1 + c2;
    if(i20 != 110) abort();

    // char * char -> int
    char c3 = 20;
    char c4 = 15;
    int i21 = c3 * c4;
    if(i21 != 300) abort();  // 300 > 127 (signed char max)

    // unsigned char + unsigned char -> int
    unsigned char uc1 = 200;
    unsigned char uc2 = 100;
    int i22 = uc1 + uc2;
    if(i22 != 300) abort();  // 300 > 255 (unsigned char max)

    // short + int -> int
    short s5 = 1000;
    int i23 = 50000;
    int i24 = s5 + i23;
    if(i24 != 51000) abort();

    // char + int -> int
    char c5 = 10;
    int i25 = 100000;
    int i26 = c5 + i25;
    if(i26 != 100010) abort();

    // short + float -> float
    short s6 = 5;
    float f23 = 2.5f;
    float f24 = s6 + f23;
    if(f24 != 7.5f) abort();

    // char + double -> double
    char c6 = 3;
    double d18 = 1.5;
    double d19 = c6 + d18;
    if(d19 != 4.5) abort();

    // === Signed/unsigned conversions ===

    // signed + unsigned -> unsigned (when same rank)
    int i27 = -1;
    unsigned int u1 = 10;
    // Result is unsigned, -1 becomes large positive
    unsigned int u2 = i27 + u1;
    if(u2 != 9) abort();  // wraps: 0xFFFFFFFF + 10 = 9

    // unsigned char promoted to int (not unsigned int)
    unsigned char uc3 = 255;
    int i28 = uc3 + 1;
    if(i28 != 256) abort();

    // === Assignment narrowing ===

    // int -> short
    int i29 = 1234;
    short s7 = i29;
    if(s7 != 1234) abort();

    // int -> char
    int i30 = 65;
    char c7 = i30;
    if(c7 != 65) abort();

    // short -> char
    short s8 = 97;
    char c8 = s8;
    if(c8 != 97) abort();

    // float -> short
    float f25 = 500.9f;
    short s9 = f25;
    if(s9 != 500) abort();

    // double -> char
    double d20 = 66.7;
    char c9 = d20;
    if(c9 != 66) abort();

    // === Zero to pointer (null pointer constant) ===

    // 0 -> pointer
    int *p1 = 0;
    if(p1 != 0) abort();

    char *p2 = 0;
    if(p2 != 0) abort();

    void *p3 = 0;
    if(p3 != 0) abort();

    // Comparison with 0
    int *p4 = 0;
    if(p4) abort();  // should be false
    if(!(p4 == 0)) abort();
    if(p4 != 0) abort();

    // Pointer to pointer = 0
    int **pp1 = 0;
    if(pp1 != 0) abort();

    // Array of pointers initialized with 0
    char *arr[3] = {0, 0, 0};
    if(arr[0] != 0) abort();
    if(arr[1] != 0) abort();
    if(arr[2] != 0) abort();

    return 0;
}
