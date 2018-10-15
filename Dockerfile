FROM debian:sid
MAINTAINER Jaseem Abid <jaseemabid@gmail.com>

ENV LANG en_US.UTF-8
ENV LC_ALL en_US.UTF-8

# Setup base image deps
RUN apt-get update && apt-get install -y \
    clang-7 \
    g++ \
    haskell-stack \
    llvm-7 \
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

WORKDIR /olifant
ADD stack.yaml .

# Setup the GHC and libraries based on Stackage version without any application
# code for efficient docker cache.
RUN stack setup

# Explicit update for better caching, this is a huge download
RUN stack update

# Install the heaviest deps. Rerunning this is very expensive

# Stack fails to install packages in a folder with stack.yaml if the rest of the
# project is missing with the following error. Quick workaround is to cd /
#
# > Stack looks for packages in the directories configured in the 'packages' and
# > 'extra-deps' fields defined in your stack.yaml. The current entry points to
# > /olifant/ but no .cabal or package.yaml file could be found there.

RUN cd /tmp && stack install bytestring containers mtl parsec protolude tasty text \
     --resolver $(grep resolver /olifant/stack.yaml | awk '{print $2}')

# Install explicitly mentioned additional dependencies like llvm-hs
RUN cd /tmp && stack install megaparsec-7.0.0
RUN cd /tmp && stack install llvm-hs-pure-7.0.0 llvm-hs-7.0.1

ADD . /olifant

# Build without running tests for better caching
RUN stack build --test --no-run-tests
