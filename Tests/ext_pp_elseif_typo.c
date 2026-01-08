// #elseif is not valid (should be #elif), but we accept it as
// an alternative spelling.
#if 0
int a = 1;
#elseif 1
int b = 2;
int main(){
    return 0;
}
#endif
