#define __swift_unavailable(_msg)       __attribute__((__availability__(swift, unavailable, message=_msg)))
void	 srand(unsigned) __swift_unavailable("Use arc4random instead.");
