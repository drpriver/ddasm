#pragma library("libc")
int printf(const char*, ...);
void print(const char* s){
    const char* p = s;
    printf("s: %s\n", p);
}
void start(){
    print("hello");
}
int main(){ return 0; }
