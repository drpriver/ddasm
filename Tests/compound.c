#pragma library("libc")
int printf(const char*, ...);

typedef struct S {
    char c[6];
} S;

int main(){
    printf("1: %s\n", (char[6]){'h', 'i'});
    printf("2: %s\n", ((struct { char c[6];}){'h', 'i'}).c);
    printf("3: %s\n", &((struct { char c[6];}){'h', 'i'}).c[0]);
    printf("4: %s\n", &((char [6]){'h', 'i'})[0]);
    printf("5: %s\n", &(struct { char c[6];}){'h', 'i'}.c[0]);
    printf("6: %s\n", (char*)&(S){'h', 'i'});
    return 0;
}
