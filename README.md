# Olifant [![Build Status](https://travis-ci.org/jaseemabid/Olifant.svg?branch=master)](https://travis-ci.org/jaseemabid/olifant)

A simple lambda calculus like language targeting LLVM, __WIP__

Project is setup with [stack][stack]

    $ git pull https://github.com/jaseemabid/Olifant && cd Olifant
    $ stack setup && stack build

See the blog post [Lessons learned building a toy compiler][blog] for a detailed
introduction to the project.

The test coverage isn't great, but has a bunch of good code samples. They run
pretty fast

```
$ stack test

Unit Tests
  Compiler
    Identity function:                         OK
    Const function:                            OK
    Ensure arity:                              OK
      Fewer arguments
      Surplus arguments
    Find undefined variables:                  OK
  LLVM Code generator
    Global variables:                          OK (0.01s)
    Simple identity function:                  OK (0.01s)
  Parser
    Literal numbers, identifiers and booleans: OK (0.01s)
    λ definitions:                             OK (0.01s)
    Combinators:                               OK
    λ application:                             OK
    Let expressions:                           OK
    Handle sequences of expressions:           OK
    Let bindings with aliases:                 OK

All 13 tests passed (0.01s)
```


[blog]: https://jaseemabid.github.io/2017/07/04/compiler.html
[stack]: https://haskellstack.org
