set -e

make
g++ -m32 -std=c++0x runtime.o example.s
./a.out
