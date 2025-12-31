#define PASTE(a, b) a##b
#define PASTE_SPACE(a, b) a ## b

int PASTE(foo, bar);
int PASTE_SPACE(baz, qux);
