int main(){
    sizeof (struct Foo {
     int x;
     }){0};
    struct Foo f = {1};
    return f.x != 1;
}
