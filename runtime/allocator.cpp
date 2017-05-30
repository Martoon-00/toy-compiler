#include <stdlib.h>
#include <iostream>
#include <map>
#include <stdexcept>
#include <algorithm>
#include <sstream>

#define VALUE_SIZE sizeof(int)

struct alloc_meta {
    int counter;
    alloc_meta(): counter(0) {}
};

std::map<int*, int> allocated = std::map<int*, int>();

extern "C" {
namespace Toy {

    int* allocate(int size) {
        int* ptr = (int*) std::calloc(1, size * VALUE_SIZE + sizeof(alloc_meta));
        allocated[ptr] = size;
        return (int*)((alloc_meta*) ptr + 1);
    }

    alloc_meta& access_alloc_meta(int* ptr) {
        return ((alloc_meta*) ptr)[-1];
    }

    void alloc_counter_increment(int* ptr) {
        access_alloc_meta(ptr).counter++;
    }

    void alloc_counter_decrement(int* ptr) {
        access_alloc_meta(ptr).counter--;
    }

    void deallocate(int* arr_ptr) {
        int* ptr = arr_ptr - sizeof(alloc_meta);
        if (allocated.count(ptr)) {
            std::free(ptr);
            allocated.erase(ptr);
        } else {
            std::stringbuf desc;
            std::ostream(&desc) << "Freeing non-allocated pointer!: " << ptr;
            throw std::runtime_error(desc.str());
        }
    }

    void ensureNoAllocations() {
        if (!allocated.empty()) {
            std::cerr << "Unallocated memory pointers:";
            std::for_each(allocated.begin(), allocated.end(), [](const std::pair<int*, int> &it){
                    std::cerr << "\t" << it.first << " - " << it.second << " bytes";
                });
            throw std::runtime_error("Memory leaked!");
        }
    }

}
}
