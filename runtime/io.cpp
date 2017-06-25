#include <stdio.h>

#include "num.h"

extern "C" {
namespace Toy {

    // always return a value
    typedef int toy_void;

    toy_void debug_write(int val) {
        printf("%d\n", val);
        return 0;
    }

    toy_void write(int val) {
        printf("%d\n", from_31_num(val));
        return 0;
    }

    int read() {
        int val;
        printf("> ");
        scanf("%d", &val);
        return to_31_num(val);
    }
}
}
