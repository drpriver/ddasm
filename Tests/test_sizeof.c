struct S { int padding[8]; };

// Full SDL pattern
typedef int test[(sizeof(struct S) == sizeof(((struct S*)((void*)0))->padding)) * 2 - 1];

int start() { return 0; }
int main(){ return 0; }
