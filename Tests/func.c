#pragma library("libc")
#include <string.h>
int str_eq(const char* a, const char* b){
    return strcmp(a, b) == 0;
}

int func(void){
    if(!str_eq(__func__, "func")) return __LINE__;
    if(!str_eq(__FUNCTION__, "func")) return __LINE__;
    if(!str_eq(__PRETTY_FUNCTION__, "func")) return __LINE__;
    _Static_assert(sizeof __func__ == 5, "sizeof __func__");
    return 0;
}

int main(){
    return func();
}
