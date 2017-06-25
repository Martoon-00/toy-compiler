#ifndef TOY_NUM_LIB
#define TOY_NUM_LIB

int from_31_num(int a) {
    return a >> 1;
}

int to_31_num(int a) {
    int r = (a << 1) + 1;
    return r;
}

#endif
