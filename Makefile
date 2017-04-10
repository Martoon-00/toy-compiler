all: build-in-tests control-tests

TF=compiler-tests
CHECK=make -j 4 -C $(TF)
CLEAN=make clean -C $(TF)

build: src
	stack build

build-in-tests: build
	stack test

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
	stack clean
	$(CLEAN)/core
	$(CLEAN)/expressions
	$(CLEAN)/deep-expressions
	$(CLEAN)/performance

.PHONY: build, clean
