#ifndef TOY_VOID_LIB
#define TOY_VOID_LIB

#include <iostream>

    // NOTE: currently unused
    // In this compiler all functions have to return something
    // This automatically holds for user-defined functions;
    // for runtime functions we use this return type to indicate
    // that function returns nothing.
    class toy_void {
        int pseudo_result;

    public:
        toy_void(): pseudo_result(0) {}

        ~toy_void() {
            if (!pseudo_result) {
                std::cerr << "Function with toy-void result type didn't return!" << std::endl;
            }
        }
    };

#endif
