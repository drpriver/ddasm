#pragma library("libc")
#include <stdio.h>
void f(float f, int line){
    unsigned x = *(unsigned*)&f;
    if(x != 0x3f800000){ // 1.f reinterpreted as integer
        printf("%s:%d: assert failed\n", __FILE__, line);
        printf("  as double: %f\n", f);
        printf("  as integer: 0x%x\n", x);
    }
}
int main(){
    float x = 1.f;
    double y = 1.;
    int z = 1;
    f(1, __LINE__);
    f(1.f, __LINE__);
    f(1., __LINE__);
    f(x, __LINE__);
    f(y, __LINE__);
    f(z, __LINE__);
}
