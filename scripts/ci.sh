#!/bin/bash
#
# This script can be use both for for CI as well as building locally.
#
# 1. If you use hvr's ppa then you can set the GHCVER and CABALVER variables
#    to select your compiler and cabal-install executable.
#
# 2. Alternatively you can set the COMPILER_PATH variable to compile
#    with a given ghc executable.
#
# 3. If you do none of these, the default ghc, cabal in your PATH is used
#    which typically is your distros cabal and ghc.
#
#

export PATH="/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$PATH"

if [ -n "$COMPILER_PATH" ]
then
    PATH="$COMPILER_PATH":$PATH
fi

ghc --version
cabal --version

cabal update
cabal new-configure
cabal new-build
cabal new-test
