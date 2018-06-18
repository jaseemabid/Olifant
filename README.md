# Olifant [![Build Status](https://travis-ci.org/jaseemabid/Olifant.svg?branch=master)](https://travis-ci.org/jaseemabid/Olifant) [![Docker Repository on Quay](https://quay.io/repository/jaseemabid/olifant/status "Docker Repository on Quay")](https://quay.io/repository/jaseemabid/olifant)

A simple functional language targeting LLVM, __WIP__

Project is setup with [stack][stack]

    $ git pull https://github.com/jaseemabid/Olifant && cd Olifant
    $ stack setup && stack build

See the blog post [Lessons learned building a toy compiler][blog] for a detailed
introduction to the project.

The test coverage isn't great, but has a bunch of good code samples. They run
pretty fast too.

    $ stack test

The README maybe sparse, but there is a lot of inline documentation in the code;
which can be read with Haddock.

    $ stack haddock --no-haddock-deps --open

Olifant programs can be compiled to LLVM IR and then to native binaries with
Clang.

    $ stack exec olifant <<< 'sum 4 5' > sum.ll
    $ make sum
    $ ./sum
    9

The compiler accepts programs from stdin or takes a file as argument.

Docker images are automatically built by quay.io as well as by Travis CI as part
of automated testing. Any of the following work.

    $ docker pull olifant/olifant

I would *LOVE* any code contributions. There is a [milestone][milestone] to
track priority bugs and features if you are looking for some inspiration.

[blog]: https://jaseemabid.github.io/2017/07/04/compiler.html
[stack]: https://haskellstack.org
[milestone]: https://github.com/jaseemabid/Olifant/milestone/2
