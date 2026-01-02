#define PASTE(a, b) a ## b
#define COMPILE_TIME_ASSERT(name, x) typedef int SDL_compile_time_assert_ ## name[(x) * 2 - 1]

int PASTE(foo, bar);
COMPILE_TIME_ASSERT(mytest, 1);
