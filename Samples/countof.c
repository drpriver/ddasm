// C2y _Countof
#pragma library("libc")
#include <stdio.h>
#include <stddef.h>

int main(){
    int x[10];
    for(size_t i = 0; i < _Countof x; i = i + 1){
        printf("%zu\n", i);
    }
    return 0;
}
