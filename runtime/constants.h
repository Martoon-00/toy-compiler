#ifndef TOY_CONSTANTS_LIB
#define TOY_CONSTANTS_LIB

#include <cstdlib>

bool use_gc = std::getenv("DISABLE_GC") == NULL;

bool use_31_arith = use_gc;

#endif
