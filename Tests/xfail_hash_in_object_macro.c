// XFAIL: # (stringify) in object-like macro
#define BAD #something
int a = 1;
const char* bad = BAD;
int main(){ return 0; }
