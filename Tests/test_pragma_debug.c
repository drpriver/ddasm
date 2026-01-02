#define FOO bar
#define X 5
#define ADD(a,b) ((a)+(b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))

#pragma expand FOO
#pragma expand ADD(1, 2)
#pragma eval X + 3
#pragma eval 1 + 2 * 3
#pragma eval defined(FOO)
#pragma eval defined(UNDEFINED)
#pragma reveal FOO
#pragma reveal ADD
#pragma reveal MAX
#pragma reveal UNDEFINED
#pragma message "Hello from preprocessor!"
#pragma message "X is defined as: " X
#pragma reveal __FILE__
#pragma expand __FILE__
#pragma expand UNDEFINED
#pragma eval UNDEFINED

int main() { return 0; }
