// XFAIL: token paste creating invalid token
#define PASTE(a, b) a ## b
int x = PASTE(+, -);  // +- is not a valid token
