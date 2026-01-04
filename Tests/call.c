#pragma library("libc")
    int printf(const char*, ...);
    void exit(int);

int add(int a, int b){
    return a + b;
}

void start(){
    int x = add(1, 2);
    if(x == 3){
        printf("success\n");
        exit(0);
    }
    else{
        printf("fail\n");
        exit(1);
    }
}
int main(){ return 0; }
