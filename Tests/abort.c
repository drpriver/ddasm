#define __cold          __attribute__((__cold__))
#define __dead2         __attribute__((__noreturn__))
void	 abort(void) __cold __dead2;
int main(){ return 0; }
