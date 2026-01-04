struct rlimit;
int     getrlimit(int, struct rlimit *) __asm("_" "getrlimit");
int main(){ return 0; }
