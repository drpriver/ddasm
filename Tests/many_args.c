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

int main(){
    int a = add2(1, 2);
    assert(a == 3);
    int b = add3(1, 2, 3);
    assert(b==6);
    int c = add4(1, 2, 3, 4);
    assert(c==10);
    int d = add5(1, 2, 3, 4, 5);
    assert(d==15);
    int e = add6(1, 2, 3, 4, 5, 6);
    assert(e==21);
    int f = add7(1, 2, 3, 4, 5, 6, 7);
    assert(f==28);
    int g = add8(1, 2, 3, 4, 5, 6, 7, 8);
    assert(g==36);
    int h = add9(1, 2, 3, 4, 5, 6, 7, 8, 9);
    assert(h==45);
    int i = add10(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    assert(i==55);
    exit(0);
}
