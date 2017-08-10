#ifndef TOY_NUM_LIB
#define TOY_NUM_LIB

#include "constants.h"

int from_31_num(int a) {
    if (use_31_arith) {
        return a >> 1;
    } else {
        return a;
    }
}

int to_31_num(int a) {
    if (use_31_arith) {
        int r = (a << 1) + 1;
        return r;
    } else {
        return a;
    }
}

#endif
