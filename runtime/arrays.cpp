#include <stdlib.h>
#include <iostream>
#include <set>
#include <map>
#include <stdexcept>
#include <algorithm>
#include <sstream>

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

    void ref_counter_increment(int* ptr) {
        raw* raw_ptr = to_raw_ptr(ptr);

        if (ptr == NULL)
            return;

        if (!ever_allocated.count(raw_ptr))
            return;

        if (!allocated.count(raw_ptr)) {
            std::stringbuf desc;
            if (ever_allocated.count(raw_ptr)) {  // TODO: will be useful
                std::ostream(&desc) << "Incrementing freed pointer!: ";
            } else {
                std::ostream(&desc) << "Incrementing not allocated pointer!: ";
            }
            std::ostream(&desc) << ptr;
            throw std::runtime_error(desc.str());
        }
        int counts = ++access_array_meta(ptr).ref_counter;
        // std::cerr << ptr << " - incremented " << counts << std::endl;
    }

    int* allocate(int size) {
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
        raw* raw_ptr = to_raw_ptr(ptr);
        if (!ever_allocated.count(raw_ptr))
            return;

        if (allocated.count(raw_ptr)) {
            int len = access_array_meta(ptr).arr_length;
            int** ptr_ = reinterpret_cast<int**>(ptr);
            std::for_each(ptr_, ptr_ + len, ref_counter_decrement);
                std::free(raw_ptr);
            allocated.erase(raw_ptr);
        } else {
            std::stringbuf desc;  // TODO: cases of `ever_allocated`
            std::ostream(&desc) << "Freeing freed pointer!: " << raw_ptr;
            throw std::runtime_error(desc.str());
        }
    }

    void ref_counter_decrement(int* ptr) {
        if (ptr == NULL)
            return;

        if (!allocated.count(to_raw_ptr(ptr)))
            return;

        int counts = --access_array_meta(ptr).ref_counter;
        // std::cerr << ptr << " - decremented " << counts << std::endl;
        if (!counts) {
            deallocate(ptr);
        }
    }

    //////////////////////////////////////
    //// Dedicated external functions ////
    //////////////////////////////////////

    int arrlen(int* ptr) {
        auto res = access_array_meta(ptr).arr_length;
        ref_counter_decrement(ptr);
        return res;
    }

    int* arrmake(int size, int def_val) {
        int* res = allocate(size);
        std::for_each(res, res + size, [&](int &it){
            it = def_val;
        });
        return res;
    }

    int* Arrmake(int size, int* def_val) {
        auto res = arrmake(size, (int) def_val);
        for (int i = 0; i < size; i++)
            ref_counter_increment(def_val);
        ref_counter_decrement(def_val);
        return res;
    }

    void free(int* ptr) {
        ref_counter_decrement(ptr);  // for argument expiration
        ref_counter_decrement(ptr);  // actual cleaning
    }

    void ensure_no_allocations() {
        if (!allocated.empty()) {
            std::cerr << "Unfreed memory pointers:";
            std::for_each(allocated.begin(), allocated.end(), [](decltype(*allocated.begin()) &it){
                    std::cerr << "\t"
                        << it.first << " - "
                        << it.second << " bytes, "
                        << access_array_meta(from_raw_ptr(it.first)).ref_counter << " refs\n";
                });
            throw std::runtime_error("Memory leaked!");
        }
    }

}
}
