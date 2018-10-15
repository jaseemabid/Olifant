# Olifant [![Build Status](https://travis-ci.org/jaseemabid/Olifant.svg?branch=master)](https://travis-ci.org/jaseemabid/Olifant) [![Docker Repository on Quay](https://quay.io/repository/jaseemabid/olifant/status "Docker Repository on Quay")](https://quay.io/repository/jaseemabid/olifant)

A simple functional language targeting LLVM, __WIP__ ⚙⚒

Project is setup with [stack][stack]

    $ git pull https://github.com/jaseemabid/Olifant && cd Olifant
    $ stack setup && stack build

See the blog post [Lessons learned building a toy compiler][blog] for a detailed
introduction to the project.

The test coverage isn't great, but has a bunch of good code samples. They run
pretty fast 🏎 too.

    $ stack test

The README maybe sparse, but there is a lot of inline documentation 📜 in the
code; which can be read with Haddock.

    $ stack haddock --no-haddock-deps --open

Olifant programs can be compiled to native binaries and executed in one step ⚡.

    $ stack exec olifant <<< 'sum 4 5'
    $ 9

This is NOT an interpreter, a machine native binary is produced and executed in
one step 🎩.

    $ file /tmp/cmd.exe
    $ /tmp/cmd.exe: Mach-O 64-bit executable x86_64

There are some samples programs in the examples directory - these are real
programs used by the test runner so if the tests are ✅, these are guaranteed to
work.

The intermediary forms can be exported with additional flags. For example, the
output of the parser can be viewed with `-p`.

    $ stack exec olifant -- -p <<< 'sum 4 5'

```haskell
[ App (Ref
       {rname = "sum", ri = 0, rty = TInt :> (TInt :> TInt), rscope = Extern})
      [Lit (Number 4), Lit (Number 5)]
]
```

Similarly, Core is exported with `-c` and the intermediary LLVM IR is exported
with `-l`.

The compiler accepts programs from stdin or takes a file as argument.

    $ olifant -l examples/vars.ol
    $ file vars.ll
    vars.ll: ASCII text

Docker images are automatically built by quay.io as well as by Travis CI as part
of automated testing. If you have trouble building the dependencies/libraries
locally, try the container.

    $ make container
    $ docker run -it olifant/olifant

I would *LOVE* any code contributions. There is a [milestone][milestone] to
track priority bugs and features if you are looking for some inspiration.

[blog]: https://jaseemabid.github.io/2017/07/04/compiler.html
[stack]: https://haskellstack.org
[milestone]: https://github.com/jaseemabid/Olifant/milestone/2
