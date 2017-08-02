# Olifant [![Build Status](https://travis-ci.org/jaseemabid/Olifant.svg?branch=master)](https://travis-ci.org/jaseemabid/olifant)

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

I would *LOVE* any code contributions. There is a [milestone][milestone] to
track priority bugs and features if you are looking for some inspiration.

[blog]: https://jaseemabid.github.io/2017/07/04/compiler.html
[stack]: https://haskellstack.org
[milestone]: https://github.com/jaseemabid/Olifant/milestone/2
