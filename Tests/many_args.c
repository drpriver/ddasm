#pragma library("libc")
int printf(const char*, ...);
void exit(int);
#define assert(cond) do {if(!cond){printf("%s:%d: '%s' failed\n", __FILE__, __LINE__, #cond); exit(1);}}while(0)
int add2(int a, int b){
    return a + b;
}
int add3(int a, int b, int c){
    return add2(a, b) + c;
}
int add4(int a, int b, int c, int d){
    return add3(a, b, c) + d;
}
int add5(int a, int b, int c, int d, int e){
    return add4(a, b, c, d) + e;
}
int add6(int a, int b, int c, int d, int e, int f){
    return add5(a, b, c, d, e) + f;
}
int add7(int a, int b, int c, int d, int e, int f, int g){
    return add6(a, b, c, d, e, f) + g;
}
int add8(int a, int b, int c, int d, int e, int f, int g, int h){
    return add7(a, b, c, d, e, f, g) + h;
}
int add9(int a, int b, int c, int d, int e, int f, int g, int h, int i){
    return add8(a, b, c, d, e, f, g, h) + i;
}
int add10(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j){
    return add9(a, b, c, d, e, f, g, h, i) + j;
}
int main(){ return 0; }
