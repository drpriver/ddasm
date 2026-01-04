#ifdef __DDASM__
void abort(void){__dasm{dump;abort;}}
#else
void abort(void);
#endif
int main(){
    int x = 1;
    int y = 2;
    float a = x;
    a /= y;
    if(a != .5f) abort();
    return 0;
}
