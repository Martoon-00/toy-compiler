#include <stdio.h>

extern "C" {
namespace Toy {
 
    void write(int val) {
        printf("%d\n", val);
    }

    int read() {
        int val;
        printf("> ");
        scanf("%d", &val);
       return val;
    } 
}
}