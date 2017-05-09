all: build-in-tests control-tests

TF=compiler-tests
CHECK=make -j 4 -C $(TF)
CLEAN=make clean -C $(TF)
TEST_RUNS=100

CORE_TESTS=test001 test002 test003 test004 test005 test006 test007 test008 test009 test010 #test011 test012 test013 test014 test015 test016 test017 test018 test019 test020 test021 test022 test023 test024 test025 test026 test027 test028 test029 test030

build:
	stack build

build-in-tests: build
	stack --no-terminal test -j 4 --test-arguments --qc-max-success=$(TEST_RUNS)

core-tests: build
	$(CHECK)/core TESTS="$(CORE_TESTS)"

expressions-tests: build
	$(CHECK)/expressions

deep-expressions-tests: build
	$(CHECK)/deep-expressions

perfomance-tests: build
	$(CHECK)/performance

control-tests: core-tests expressions-tests deep-expressions-tests

clean:
	$(CLEAN)/core
	$(CLEAN)/expressions
	$(CLEAN)/deep-expressions
	$(CLEAN)/performance

.PHONY: build, clean
