#pragma library("libc")
#include <stdio.h>

void fizzother(FILE*, int);
int main(){
    FILE* fp = fopen("/dev/null", "wb");
    for(int i = 10000; i; i = i - 1){
        fizzother(fp, 10000);
    }
    fflush(fp);
    fclose(fp);
    return 0;
}

void fizzother(FILE* fp, int n){
    size_t a = 1;
    size_t mod_3 = 0;
    size_t mod_5 = 0;
    size_t d = n + 1;
    size_t printed = 0;
    for(;a != d; a = a+1){
        printed = 0;
        mod_3 = mod_3 + 1;
        mod_5 = mod_5 + 1;
        if(mod_3 == 3){
            fprintf(fp, "fizz");
            mod_3 = 0;
            printed = 1;
        }
        if(mod_5 == 5){
            mod_5 = 0;
            fprintf(fp, "buzz");
            printed = 1;
        }
        if(!printed) fprintf(fp, "%zu", a);
    }
}
