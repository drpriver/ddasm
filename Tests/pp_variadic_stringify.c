#pragma library("libc")
int printf(const char*, ...);
#define CODE(...) #__VA_ARGS__

const char* msg = CODE(
    int main() { 
        return 0; 
    }
);
#ifdef __DDASM__
// EXTENSION
__MIXIN__(CODE(
    int foo(int a, int b){ 
        printf("%s\n", __func__);
        return a + b;
    }
))
#endif
int main(){
    const char* expected = "int main(){return 0;}";
    const char* a = expected;
    const char* b = msg;
    for(;;a++, b++){
        // whitespace can vary, so just skip
        while(*a == ' ' || *a == '\n')
            a++;
        while(*b == ' ' || *b == '\n')
            b++;
        if(*a != *b){
            printf("%s:%d: FAIL\n", __FILE__, __LINE__);
            printf("%s:%d: rest of expected is '%s'\n", __FILE__, __LINE__, a);
            printf("%s:%d: rest of msg      is '%s'\n", __FILE__, __LINE__, b);
            return __LINE__;
        }
        if(!*a) break;
    }
#ifdef __DDASM__
    int y = foo(2, 3);
    if(y != 5) return __LINE__;
#endif
    return 0;
}
