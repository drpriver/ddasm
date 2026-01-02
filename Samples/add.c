#pragma library("libc")
int printf(const char*, ...);
int add(int a, int b){
    return a + b;
}

int main(){
    int sum = add(1, 2);
    printf("1 + 2 = %d\n", sum);
    return 0;
}
