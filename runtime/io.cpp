#include <stdio.h>

#include "num.h"

extern "C" {
namespace Toy {

    void debug_write(int val) {
        printf("%d\n", val);
    }

    void write(int val) {
        printf("%d\n", from_31_num(val));
    }

    int read() {
        int val;
        printf("> ");
        scanf("%d", &val);
        return to_31_num(val);
    }
}
}
