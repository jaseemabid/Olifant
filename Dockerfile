FROM debian:sid
MAINTAINER Jaseem Abid <jaseemabid@gmail.com>

ENV LANG en_US.UTF-8
ENV LC_ALL en_US.UTF-8

# Setup base image deps
RUN apt-get update && apt-get install -y \
    clang-6.0 \
    g++ \
    haskell-stack \
    llvm-6.0 \
    locales \
    locales-all \
    netbase \
    && rm -rf /var/lib/apt/lists/*

# Stack version shipped with Debian Sid is broken (obviously) and needs an
# upgrade before it works. netbase is a required dependency to provide
# /etc/protocols, that's missing as well.
# Ref https://github.com/bos/wreq/issues/5#issuecomment-108086543

ENV PATH="/root/.local/bin:${PATH}"
RUN stack upgrade

# Setup the Haskell version from stack.yml, this should change only rarely
WORKDIR /olifant
ADD stack.yaml .
RUN stack setup --no-terminal

# Explicit update for better caching, this is a huge download
RUN stack update

# Having to add source files before installing big deps has a huge performance
# cost, but that can be fixed later.
#
# Avoiding this line will make the next step fail with the error
#
# ```
# Stack looks for packages in the directories configured in the 'packages' and 'extra-deps' fields defined in your stack.yaml
# The current entry points to /olifant/ but no .cabal or package.yaml file could be found there.
# ```

ADD . /olifant

# Install the heaviest deps, rerunning this takes a long time
RUN stack install llvm-hs llvm-hs-pure

# Build without running tests for better caching
RUN stack build --test --no-run-tests
