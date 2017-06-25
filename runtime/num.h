#ifndef TOY_NUM_LIB
#define TOY_NUM_LIB

int from_31_num(int a) {
    return (a >> 1) & (-1 - (1 << 30));
}

int to_31_num(int a) {
    int r = (a << 1) + 1;
    r ^= a & (1 << 31);
    return r;
}

#endif
