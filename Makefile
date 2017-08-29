all: build-in-tests control-tests

TF=compiler-tests
RUNTIME=./runtime
CHECK=make -j 4 -C
CLEAN=make clean -C
TEST_RUNS=100

# up to functions
CORE_TESTS=test001 test002 test003 test004 test005 test006 test007 test008 test009 test010 test011 test012 test013 test014 test015 test016 test017 test018 test019 test020 test021 test022 test023 test024 test025 test026 test029 test030 #test027 test028

build:
	make -C $(RUNTIME)
	stack build

build-in-tests: build
	stack --no-terminal test -j 4 --test-arguments --qc-max-success=$(TEST_RUNS)

core-tests: build
	$(CHECK) $(TF)/core TESTS="$(CORE_TESTS)"

expressions-tests: build
	$(CHECK) $(TF)/expressions

deep-expressions-tests: build
	$(CHECK) $(TF)/deep-expressions

perfomance-tests: build
	$(CHECK) $(TF)/performance

jump-tests: build
	$(CHECK) ./test/cases/control/jumps

control-tests: core-tests expressions-tests deep-expressions-tests jump-tests

# this is separated because travis can't handle amount of 'expressions' tests
travis-tests: core-tests deep-expressions-tests jump-tests

clean:
	make clean -C $(RUNTIME)
	$(CLEAN) $(TF)/core
	$(CLEAN) $(TF)/expressions
	$(CLEAN) $(TF)/deep-expressions
	$(CLEAN) $(TF)/performance
	$(CLEAN) ./test/cases/control/jumps

.PHONY: build, clean
