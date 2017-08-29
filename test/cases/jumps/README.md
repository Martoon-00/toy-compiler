# Jumps

## Specification

### Introduction

Jump functionality is served by two new statements:

* __Label__ = ":" *string_literal*

* __Goto__ = "goto" __Label__

### Compilation and execution

For program to compile following should hold:

* All labels declaration should define different labels.

* __Goto__ to undefined label is a compile-time error.


Label declaration is processed in exactly the same way as __Skip__ statement.

Goto is executed in the following way:

* If specified label is sited in current function scope, then execution
continues from the next statement after referenced label (if label is the last
statement in the function, function quites).
Environment is preserved during jump, i.e. values of local variables and on
stack (if present) remain unchanged.

* If there is no specified label defined in current function, then function
execution safely aborts as if it returned a value, and given __Goto__ statement is
performed recursively immediatelly after function call ends.

* If there is no specified label defined in any function of the current call
stack up to the _main_, then whole program execution finishes exceptionally.


## Tests

Tests have same structure like ones in `compiler-test` submodule.

They can be run with `make jump-tests` from the root of the project.

##### Various scenarious are present

* Forward / backward jumps

* Local / nonlocal jumps

* Jumps to label in main / other function

##### Special cases

* `long-jump` - jump outside of two nested function calls at once

* `recursion` - jump out of recursed function call
