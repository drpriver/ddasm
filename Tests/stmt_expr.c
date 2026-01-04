// Test (6.8.3) expression-statement

int global_counter;

void test_expr_stmt(void) {
    global_counter = 0;
    global_counter++;
    global_counter += 10;
}

int test_call_stmt(void) {
    test_expr_stmt();
    return global_counter;
}

int test_empty_stmt(void) {
    ;
    ;;
    return 0;
}

int test_side_effects(int* p) {
    *p = 42;
    (*p)++;
    return *p;
}
int weird_switch(int x){
    switch(({
        int i;
        for(i = 0; i < x; ({
            for(int y = 0; y < x/2; y++)
                i++;
        }))
            ;
        i;
    })){
        case 1: return 1;
        case 2: return 2;
        case 6: return 6;
        default: return x + 10;
    }
}
#pragma library("libc")
int printf(const char*, ...);
int bizarro(int x){
    if(({
        switch(x){
            case 1: return 1;
        }
        printf("x: %d\n", x);
        x;
    })){
        printf("x+2: %d\n", x+2);
        return x+2;
    }
    return x;
}
int main(){
    int x = weird_switch(10);
    if(x != 20) return 1;
    if(bizarro(1) != 1) return 2;
    if(bizarro(5) != 7) return 3;
    return 0;
}
