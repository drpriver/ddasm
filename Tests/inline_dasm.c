// Test inline dasm blocks

int add(int a, int b){
#ifdef __DDASM__
    dasm {
        add rout1 rarg1 rarg2
        ret
    }
#else
    return a + b;
#endif
}

int mul(int a, int b){
#ifdef __DDASM__
    __dasm {
        mul rout1 rarg1 rarg2
        ret
    }
#else
    return a * b;
#endif
}
int sub(int a, int b){
#ifdef __DDASM__
    __dasm__ {
        sub rout1 rarg1 rarg2
        ret
    }
#else
    return a - b;
#endif
}

int main(){
    int result = add(10, 20);
    if(result != 30) return 1;

    result = mul(5, 6);
    if(result != 30) return 2;

    result = sub(23, 22);
    if(result != 1) return 3;

    return 0;
}
