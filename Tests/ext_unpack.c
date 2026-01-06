#pragma library("libc")
int printf(const char*, ...);
int main(){
#ifdef __DDASM__
    struct Point { int x, y; } p = {1, 2};
    printf("p = %d,%d\n", __unpack(p));
    int data[2] = {3, 4};
    struct Point p2 = {__unpack(data)};
    printf("p2 = %d,%d\n", __unpack(p2));
    __builtin_debug_trap();
#endif
    return 0;
}
