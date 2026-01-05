#pragma library("libc")
int printf(const char*, ...);
int puts(const char*);
int strcmp(const char*, const char*);
struct MyStruct {
    int x, y;
    char* txt;
};
struct Point {int x, y;};

int main(){
    struct MyStruct m = { 1, 2, "hello"};
    if(m.x != 1) return __LINE__;
#ifdef __DDASM__
    if(m[0] != 1) return __LINE__;
    printf("m[0] = %d, m.x = %d\n", m[0], m.x);
#endif
    if(m.y != 2) return __LINE__;
#ifdef __DDASM__
    if(m[1] != 2) return __LINE__;
    printf("m[1] = %d, m.y = %d\n", m[1], m.y);
#endif
    if(strcmp(m.txt, "hello") != 0) return __LINE__;
#ifdef __DDASM__
    if(strcmp(m[2], "hello") != 0) return __LINE__;
    printf("m[2] = '%s', m.txt = '%s'\n", m[2], m.txt);
#endif
#ifdef __DDASM__
    struct MyStruct *pm = &m;
    printf("%d, %d, %s\n", __unpack(m));
    printf("%d, %d, %s\n", __unpack(pm));
    struct SPair {
        const char *p, *p2;
    } pair = {"hello", "hello"};
    if(strcmp(__unpack(pair)) != 0) return __LINE__;
    if(strcmp((pair)[0], (pair)[1]) != 0) return __LINE__;
    if(strcmp(__unpack(((pair))))) return __LINE__;
    // puts(__unpack(m));
#endif
    struct Point p = {3, 4};
#ifdef __DDASM__
    struct MyStruct m2 = {__unpack(p), "world"};
    printf("%d, %d, %s\n", __unpack(m2));
    if(m2[0] != 3) return __LINE__;
    if(m2[1] != 4) return __LINE__;
    if(strcmp(m2[2], "world") != 0) return __LINE__;
#endif
    return 0;
}
