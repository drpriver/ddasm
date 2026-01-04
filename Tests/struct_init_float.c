#pragma library("libc")
#ifdef __DDASM__
void abort(void){__dasm{abort;}}
#else
void abort(void);
#endif
int printf(const char*, ...);

int fail = 0;

#define assert_eq(lhs, rhs) if((lhs) != (rhs)){printf("%s:%d: assertion failed: %s == %s\n", __FILE__, __LINE__, #lhs, #rhs); printf("  %s = %f\n", #lhs, (lhs)); printf("  %s (bits) = 0x%x\n", #lhs, *(unsigned*)&lhs); printf("  %s = %f\n", #rhs, (rhs)); fail = 1;}
typedef struct {
    float x, y;
} Vec2;

Vec2 gv = {1,2};
Vec2 gv2 = {-1,-2};

int main(){
    Vec2 v = {3, 4};
    float a = 5;
    float b = 6.f;
    Vec2 v2 = {a, b};
    Vec2 v3 = {(float)-5, (float)-6};
    int x = 7;
    int y = 8;
    Vec2 v4 = {x, y};
    Vec2 v5 = {-x, (float)-y};
    Vec2 v6 = (Vec2){10, 11};
    assert_eq(gv.x, 1.f);
    assert_eq(gv.y, 2.f);
    assert_eq(gv2.x, -1.f);
    assert_eq(gv2.y, -2.f);
    assert_eq(v.x, 3.f);
    assert_eq(v.y, 4.f);
    assert_eq(a, 5.f);
    assert_eq(b, 6.f);
    assert_eq(v2.x, 5.f);
    assert_eq(v2.y, 6.f);
    assert_eq(v3.x, -5.f);
    assert_eq(v3.y, -6.f);
    assert_eq(v4.x, 7.f);
    assert_eq(v4.y, 8.f);
    assert_eq(v5.x, -7.f);
    assert_eq(v5.y, -8.f);
    assert_eq(v6.x, 10.f);
    assert_eq(v6.y, 11.f);
    if(fail) abort();
    return 0;
}
