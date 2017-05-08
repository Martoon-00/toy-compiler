all: build-in-tests control-tests

TF=compiler-tests
CHECK=make -j 4 -C $(TF)
CLEAN=make clean -C $(TF)
TEST_RUNS=100

build:
	stack build

build-in-tests: build
	stack --no-terminal test -j 4 --test-arguments --qc-max-success=$(TEST_RUNS)

core-tests: build
	$(CHECK)/core

expressions-tests: build
	$(CHECK)/expressions

deep-expressions-tests: build
	$(CHECK)/deep-expressions

perfomance-tests: build
	$(CHECK)/performance

control-tests: expressions-tests deep-expressions-tests

clean:
	$(CLEAN)/core
	$(CLEAN)/expressions
	$(CLEAN)/deep-expressions
	$(CLEAN)/performance

.PHONY: build, clean
