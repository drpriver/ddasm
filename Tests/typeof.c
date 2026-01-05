int main(){
    int x = 3;
    typeof(int) z = 5;
    typeof(x) y = 4;
    typeof(int[4]) w = {5, 6, 7, 8};
    if(x != 3) return 1;
    if(y != 4) return 2;
    if(z != 5) return 3;
    if(w[0] != 5) return 4;
    if(w[1] != 6) return 5;
    if(w[2] != 7) return 6;
    if(w[3] != 8) return 7;
    return 0;
}
