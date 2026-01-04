#pragma library("libc")
#include <stdio.h>

typedef struct {
    int refcount;
    int value;
} Object;

int start() {
    Object obj = {5, 100};
    Object* p = &obj;

    // Test pre-decrement on member access (like --op->ob_refcnt)
    printf("initial refcount: %d\n", p->refcount);

    int r1 = --p->refcount;
    printf("after --p->refcount: refcount=%d, returned=%d\n", p->refcount, r1);

    // Test pre-increment
    int r2 = ++p->refcount;
    printf("after ++p->refcount: refcount=%d, returned=%d\n", p->refcount, r2);

    // Test post-decrement
    int r3 = p->refcount--;
    printf("after p->refcount--: refcount=%d, returned=%d\n", p->refcount, r3);

    // Test post-increment
    int r4 = p->refcount++;
    printf("after p->refcount++: refcount=%d, returned=%d\n", p->refcount, r4);

    // Test the comparison pattern used in Py_DECREF: if (--p->refcount == 0)
    p->refcount = 1;
    if (--p->refcount == 0) {
        printf("refcount reached zero!\n");
    }

    // Verify final state
    if (p->refcount == 0 && p->value == 100) {
        printf("PASS\n");
        return 0;
    } else {
        printf("FAIL: refcount=%d, value=%d\n", p->refcount, p->value);
        return 1;
    }
}
int main(){ return 0; }
