#define SIZEOF_LONG 8
#define LONG_BIT 8 * SIZEOF_LONG
#if LONG_BIT != 8 * SIZEOF_LONG
#error "wtf"
#else
#endif
int main(){ return 0; }
