#!/usr/bin/env sh
exec cabal -v0 --builddir="../.build/haskell" new-run "exe:$(basename "$0")" -- $@
