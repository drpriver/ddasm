#define ALIAS(x) __asm("_" #x)
extern long timezone ALIAS(timezone);
int main(){ return 0; }
