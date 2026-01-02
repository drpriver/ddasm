#pragma library("libc.so.6")
extern int printf(char* fmt, ...);

enum Days {
    MONDAY = 1,
    TUESDAY,
    WEDNESDAY,
    THURSDAY,
    FRIDAY,
    SATURDAY,
    SUNDAY
};

void print_day(enum Days d) {
    if (d == SATURDAY) {
        printf("Weekend!\n");
    } else if (d == SUNDAY) {
        printf("Weekend!\n");
    } else {
        printf("Weekday %d\n", d);
    }
}

int main() {
    print_day(MONDAY);
    print_day(FRIDAY);
    print_day(SATURDAY);
    print_day(SUNDAY);

    // Test arithmetic with enums
    int next_day;
    next_day = TUESDAY + 1;
    printf("Day after Tuesday: %d\n", next_day);

    return 0;
}
