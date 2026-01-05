int main(){
    int x = sizeof x;
    _Static_assert(sizeof x == 4, "sizeof x == 4");
    if(x != sizeof(int)) return 1;
    return 0;
}
