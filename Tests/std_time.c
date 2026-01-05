// Test <time.h> - Date and time
#pragma library("libc")
#include <time.h>

time_t test_time(void) {
    return time(0);
}
int main(){
    test_time();
    return 0;
}
