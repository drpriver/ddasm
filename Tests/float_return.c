#ifdef __DDASM__
void abort(void){__dasm{dump;abort;}}
#else
void abort(void);
#endif

double sin(double x);
float sinf(float x);
double atof(const char *s);
float strtof(const char *s, char **end);

int main() {
    // Test double return
    double d = sin(0.0);
    if (d != 0.0) abort();

    // Test float return
    float f = sinf(0.0f);
    if (f != 0.0f) abort();

    // Test double return from non-float args
    double pi = atof("3.14159");
    if (pi < 3.14 || pi > 3.15) abort();

    // Test float return from non-float args
    float e = strtof("2.718", 0);
    if (e < 2.71f || e > 2.72f) abort();

    return 0;
}
