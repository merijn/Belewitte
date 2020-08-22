#!/usr/bin/env sh
BASEPATH="$( cd "$(dirname "$0")/../" ; pwd -P )"
PROJECTFILE="$BASEPATH/cabal.project"
BUILDPATH="$BASEPATH/.build/haskell"
PKG_CONFIG_PATH="${PKG_CONFIG_PATH%:}"
PKG_CONFIG_PATH="${PKG_CONFIG_PATH#:}"
export PKG_CONFIG_PATH

unset MAKEFLAGS
unset MAKELEVEL

CABAL="$(make -sC "$BASEPATH/" report-cabal 2>/dev/null)"
if [ $? -ne 0 ] || [ -z "$CABAL" ]; then
    printf "cabal-install not found, can't run Haskell code\n"
    exit 1
fi

make -C "$BASEPATH/" cabal.project.local >/dev/null
exec $CABAL -v0 --project-file="$PROJECTFILE" --builddir="$BUILDPATH" v2-run "exe:$(basename "$0")" -- "$@"
