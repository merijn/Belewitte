#!/usr/bin/env sh
SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
PROJECTFILE="$(realpath "$SCRIPTPATH/../cabal.project")"
BUILDPATH="$(realpath "$SCRIPTPATH/../.build/haskell")"
exec cabal -v0 --project-file="$PROJECTFILE" --builddir="$BUILDPATH" v2-run "exe:$(basename "$0")" -- $@
