#!/usr/bin/env sh
SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
PROJECTFILE="$(realpath "$SCRIPTPATH/../cabal.project")"
BUILDPATH="$(realpath "$SCRIPTPATH/../.build/haskell")"
PKG_CONFIG_PATH="${PKG_CONFIG_PATH%:}"
PKG_CONFIG_PATH="${PKG_CONFIG_PATH#:}"
export PKG_CONFIG_PATH
exec cabal -v0 --project-file="$PROJECTFILE" --builddir="$BUILDPATH" v2-run "exe:$(basename "$0")" -- $@
