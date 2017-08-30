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

For instance, in program

```
write(1);
goto :l;
write(2);
:l;
write(3);
```

`write(2)` statement is no executed, because after executing `goto` statement
further execution continues from `write(3)`.

* If there is no specified label defined in current function, then function
execution safely aborts as if it returned a value, and given __Goto__ statement is
performed recursively immediately after function call ends.

In program

```
fun f() begin
    write(1);
    goto :l;
    write(2);
end

f();
write(3);
:l;
write(4);
```

when execution reaches `goto` statement, function `f` call finishes, and further
execution continues from `write(4)` statement, thus skipping `write(2)` and
`write(3)`.

* If there is no specified label defined in any function of the current call
stack up to the _main_, then whole program execution finishes exceptionally.

```
fun f() begin
    :l;
end

goto :l;
f();
```

In this program, label `:l` doesn't occur to be in current function scope
regardless of how many times we fall out of current function, so program
terminates.

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
