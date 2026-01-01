#pragma library("libc")
#include <stdio.h>

void fizzother(FILE*, int);
void main(){
    FILE* fp = fopen("/dev/null", "wb");
    for(int i = 10000; i; i = i - 1){
        fizzother(fp, 10000);
    }
    fflush(fp);
    fclose(fp);
}

void fizzother(FILE* fp, int n){
    int a = 1;
    int mod_3 = 0;
    int mod_5 = 0;
    int d = n + 1;
    int printed = 0;
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
