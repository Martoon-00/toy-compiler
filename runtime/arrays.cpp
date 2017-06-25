#include <stdlib.h>
#include <iostream>
#include <set>
#include <map>
#include <stdexcept>
#include <algorithm>
#include <sstream>

#include "num.h"

#define VALUE_SIZE sizeof(int)

extern "C" {
namespace Arrays {

    struct array_meta {
        int ref_counter;
        int arr_length;
        array_meta(int arr_length): ref_counter(0), arr_length(arr_length) {}
    };

    struct raw {};

    std::map<raw*, int> allocated = std::map<raw*, int>();
    std::set<raw*> ever_allocated = std::set<raw*>();

    array_meta& access_array_meta(int* ptr) {
        return ((array_meta*) ptr)[-1];
    }

    int* from_raw_ptr(raw* raw_ptr) {
        return (int*)(static_cast<array_meta*>(static_cast<void*>(raw_ptr)) + 1);
    }

    raw* to_raw_ptr(int* ptr) {
        return static_cast<raw*>(static_cast<void*>((array_meta*) ptr - 1));
    }

    bool is_reference(int* ptr) {
        int ptr_ = (int) ptr;
        if (ptr_ < sizeof(array_meta))
            return false;
        // TODO: understand from the pointer
        return ever_allocated.count(to_raw_ptr(ptr));
    }

    void ref_counter_increment(int* ptr) {
        if (ptr == NULL)
            return;

        if (!is_reference(ptr))
            return;

        raw* raw_ptr = to_raw_ptr(ptr);

        if (!ever_allocated.count(raw_ptr))
            throw std::runtime_error("Incrementing counter of not a reference!");

        if (!allocated.count(raw_ptr)) {
            std::stringbuf desc;
            std::ostream(&desc) << "Incrementing freed pointer!: " << ptr;
            throw std::runtime_error(desc.str());
        }

        int counts = ++access_array_meta(ptr).ref_counter;
        // std::cerr << ptr << " - incremented " << counts << std::endl;
    }

    // accepts normal number
    int* allocate(int size) {
        if (size < 0) {
            std::stringbuf desc;
            std::ostream(&desc) << "Can't allocate with negative size " << size;
            throw std::runtime_error(desc.str());
        }

        raw* raw_ptr = static_cast<raw*>(std::calloc(1, size * VALUE_SIZE + sizeof(array_meta)));
        allocated[raw_ptr] = size;
        ever_allocated.insert(raw_ptr);
        int* ptr = from_raw_ptr(raw_ptr);
        access_array_meta(ptr) = array_meta(size);
        ref_counter_increment(ptr);
        return ptr;
    }

    void ref_counter_decrement(int* ptr);

    // internal
    void deallocate(int* ptr) {
        if (!is_reference(ptr))
            throw std::runtime_error("Deallocating not a reference!");

        raw* raw_ptr = to_raw_ptr(ptr);

        if (allocated.count(raw_ptr)) {
            int len = access_array_meta(ptr).arr_length;
            int** ptr_ = reinterpret_cast<int**>(ptr);
            std::for_each(ptr_, ptr_ + len, ref_counter_decrement);
            std::free(raw_ptr);
            allocated.erase(raw_ptr);
        } else {
            std::stringbuf desc;
            std::ostream(&desc) << "Freeing freed pointer!: " << raw_ptr;
            throw std::runtime_error(desc.str());
        }
    }

    void ref_counter_decrement(int* ptr) {
        if (ptr == NULL)
            return;

        if (!is_reference(ptr)){
            return;
        }

        if (!ever_allocated.count(to_raw_ptr(ptr)))
            throw std::runtime_error("Decrementing counter of not a reference!");

        int counts = --access_array_meta(ptr).ref_counter;
        // std::cerr << ptr << " - decremented " << counts << std::endl;
        if (counts < 0) {
            std::stringbuf desc;
            std::ostream(&desc) << "Ref counter got negative!: " << ptr;
            throw std::runtime_error(desc.str());
        } else if (!counts) {
            deallocate(ptr);
        }
    }

    //////////////////////////////////////
    //// Dedicated external functions ////
    //////////////////////////////////////

    int arrlen(int* ptr) {
        int len = access_array_meta(ptr).arr_length;
        return to_31_num(len);
    }

    int* arrmake(int _size, int def_val) {
        int size = from_31_num(_size);
        int* res = allocate(size);
        std::for_each(res, res + size, [&](int &it){
            it = def_val;
            ref_counter_increment((int*) def_val);
        });
        return res;
    }

    int* Arrmake(int size, int* def_val) {
        return arrmake(size, (int) def_val);
    }

    void free(int* ptr) {
        ref_counter_decrement(ptr);
    }

    void ensure_no_allocations() {
        if (!allocated.empty()) {
            std::cerr << "Unfreed memory pointers:";
            std::for_each(allocated.begin(), allocated.end(), [](decltype(*allocated.begin()) &it){
                    std::cerr << "\t"
                        << from_raw_ptr(it.first) << " - "
                        << it.second << " bytes, "
                        << access_array_meta(from_raw_ptr(it.first)).ref_counter << " refs\n";
                });
            throw std::runtime_error("Memory leaked!");
        }
    }

}
}
