// Test Duff's device - case labels inside nested loops within switch

// Classic Duff's device for copying
void duffs_copy(int *to, int *from, int count) {
    int n = (count + 7) / 8;
    if (count == 0) return;
    switch (count & 7) {
        case 0: do { *to++ = *from++;
        case 7:      *to++ = *from++;
        case 6:      *to++ = *from++;
        case 5:      *to++ = *from++;
        case 4:      *to++ = *from++;
        case 3:      *to++ = *from++;
        case 2:      *to++ = *from++;
        case 1:      *to++ = *from++;
        } while (--n > 0);
    }
}

// Simpler version for testing - fills array with value
void duffs_fill(int *dst, int val, int count) {
    int n = (count + 3) / 4;
    if (count == 0) return;
    switch (count & 3) {
        case 0: do { *dst++ = val;
        case 3:      *dst++ = val;
        case 2:      *dst++ = val;
        case 1:      *dst++ = val;
        } while (--n > 0);
    }
}

int main() {
    int src[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    int dst[10] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

    // Test copy with various counts
    duffs_copy(dst, src, 10);

    // Verify copy worked
    int sum = 0;
    for (int i = 0; i < 10; i++) {
        sum += dst[i];
    }
    // sum should be 1+2+3+4+5+6+7+8+9+10 = 55
    if (sum != 55) return 1;

    // Test fill
    int arr[7];
    duffs_fill(arr, 42, 7);

    int fill_sum = 0;
    for (int i = 0; i < 7; i++) {
        fill_sum += arr[i];
    }
    // fill_sum should be 42 * 7 = 294
    if (fill_sum != 294) return 2;

    // Test edge case: count = 1
    int single[1];
    duffs_fill(single, 99, 1);
    if (single[0] != 99) return 3;

    // Test edge case: count = 0
    int zero[1] = {123};
    duffs_fill(zero, 0, 0);
    if (zero[0] != 123) return 4;  // Should be unchanged

    return 0;
}
