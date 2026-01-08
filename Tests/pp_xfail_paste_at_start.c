// XFAIL: ## at start of replacement
#define BAD(x) ## x
int BAD(foo);
