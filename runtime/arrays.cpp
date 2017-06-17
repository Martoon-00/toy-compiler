#include <stdlib.h>
#include <iostream>
#include <map>
#include <stdexcept>
#include <algorithm>
#include <sstream>

#define VALUE_SIZE sizeof(int)

struct array_meta {
    int ref_counter;
    int arr_length;
    array_meta(int arr_length): ref_counter(0), arr_length(arr_length) {}
};

std::map<int*, int> allocated = std::map<int*, int>();

extern "C" {
namespace Toy {

    int* allocate(int size) {
        int* ptr = (int*) std::calloc(1, size * VALUE_SIZE + sizeof(array_meta));
        allocated[ptr] = size;
        ((array_meta*) ptr)[0] = array_meta(size);
        return (int*)((array_meta*) ptr + 1);
    }

    array_meta& access_array_meta(int* ptr) {
        return ((array_meta*) ptr)[-1];
    }

    void deallocate(int* arr_ptr) {
        int* ptr = arr_ptr - sizeof(array_meta);
        if (allocated.count(ptr)) {
            std::free(ptr);
            allocated.erase(ptr);
        } else {
            // TODO: uncomment
            // std::stringbuf desc;
            // std::ostream(&desc) << "Freeing non-allocated pointer!: " << ptr;
            // throw std::runtime_error(desc.str());
        }
    }

    void alloc_counter_increment(int* ptr) {
        access_array_meta(ptr).ref_counter++;
    }

    void alloc_counter_decrement(int* ptr) {
        int counts = --access_array_meta(ptr).ref_counter;
        if (!counts) {
            deallocate(ptr);
        }
    }

    void ensure_no_allocations() {
        if (!allocated.empty()) {
            std::cerr << "Unallocated memory pointers:";
            std::for_each(allocated.begin(), allocated.end(), [](const std::pair<int*, int> &it){
                    std::cerr << "\t" << it.first << " - " << it.second << " bytes";
                });
            throw std::runtime_error("Memory leaked!");
        }
    }

    //////////////////////////////////////
    //// Dedicated external functions ////
    //////////////////////////////////////

    int arrlen(int* ptr) {
        return access_array_meta(ptr).arr_length;
    }

    int* arrmake(int size, int def_val) {
        int* res = allocate(size);
        std::for_each(res, res + size, [&](int &it){ it = def_val; });
        return res;
    }

    int* Arrmake(int size, int* def_val) {
        return arrmake(size, (int) def_val);
    }
}
}
