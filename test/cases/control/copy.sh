# Yes, this script steals key (once failed) tests from compiler-tests repo,
# because I'm too lazy to run all of them each time.

cp ../../../compiler-tests/$1/generated$2.expr $1
cp ../../../compiler-tests/$1/generated$2.input $1
cp ../../../compiler-tests/$1/orig/generated$2.log $1
