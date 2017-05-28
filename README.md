# Toy-compiler

[![Build Status](https://travis-ci.org/Martoon-00/toy-compiler.svg?branch=master)](https://travis-ci.org/Martoon-00/toy-compiler)

Homework in compilers cource.

---

### Preliminaries

To use compiler, you first need to generate `runtime.o`:

```
make -C runtime
```

Then use `rc` to launch compiler.

By default, `rc` searches for `runtime.o` in `./runtime/` folder, but you can
specify custom location via environmental variable `RC_RUNTIME`.

### Execution

Compiler `rc` accepts 2 arguments

1. Mode. According to task, compiler supports three modes:

    1.1. `-i`: Interpretation

    1.2. `-s`: Conversion to intermediate form with following interpretation

    1.3. `-o`: Compilation. Unlike previous modes, it doesn't execute the program, rather creates binary which executes

2. Path to file which contains the program.


### Tests

This repository contains tests written with `QuickCheck`. They can be run with `stack test` or `make build-in-tests`. They check for all basic features of compiler in all described modes and work quite fast.

Repository also contains dedicated testing repository as submodule. One can run them with `make control-tests`, it may take [~~a week or two~~](https://github.com/anlun/compiler-tests/issues/2) several hours.
