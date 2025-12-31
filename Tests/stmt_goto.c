// Test (6.8.6.1) goto statement and (6.8.1) labeled-statement

int test_goto(int n) {
    int i = 0;
loop:
    if (i >= n) goto done;
    i++;
    goto loop;
done:
    return i;
}

int test_forward_goto(void) {
    int x = 0;
    goto skip;
    x = 100;  // This should be skipped
skip:
    x = x + 1;
    return x;  // Should return 1
}

int test_backward_goto(void) {
    int count = 0;
again:
    count++;
    if (count < 5) goto again;
    return count;  // Should return 5
}

int test_nested_labels(int n) {
    int result = 0;
outer:
    if (n <= 0) goto end;
    result = result + n;
inner:
    n = n - 1;
    if (n > 0) goto inner;
end:
    return result;
}

int test_multiple_gotos(int x) {
    if (x == 1) goto one;
    if (x == 2) goto two;
    goto other;
one:
    return 10;
two:
    return 20;
other:
    return 0;
}
