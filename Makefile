all: build-in-tests
	

build: src
	stack build

build-in-tests: build
	stack test

clean:
	stack clean

.PHONY: build, clean

	
