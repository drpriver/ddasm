#define LINE __LINE__
_Static_assert(LINE == __LINE__, "LINE != __LINE__");

int main(){return 0;}
