// Test <signal.h> - Signal handling
#include <signal.h>

void handler(int sig) {
    // signal handler
}

int test_signal(void) {
    signal(SIGINT, handler);
    return 0;
}
int main(){
    test_signal();
    return 0;
}
