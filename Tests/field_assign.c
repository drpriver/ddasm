struct Bar {
    int x;
};
struct Foo {
    struct Bar b[1];
};
int main(){
    struct Foo f;
    f.b[0].x = 1;
    return 0;
}
