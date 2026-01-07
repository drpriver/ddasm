#pragma library("libc")
    int printf(const char*, ...);
    void exit(int);

int add(int a, int b){
    return a + b;
}

int addtwice(int a, int b){
    return add(a, b) + add(a, b);
}

int main(){
    int x = add(1, 2);
    if(x != 3) return __LINE__;
    int y = add(1, 2) + add(3, 4) + add(5, 6);
    if(y != 21) return __LINE__;
    int z = addtwice(7, 8);
    if(z != 30) return __LINE__;
    return 0;
}
