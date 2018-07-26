#!/usr/bin/env sh
SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
FILENAME="${0##*/}"
BUILDDIR="$(basename $SCRIPTPATH)"
exec cabal --enable-profiling --project-file="$SCRIPTPATH/cabal.project" --builddir="$SCRIPTPATH/../.build/$BUILDDIR" new-run "exe:$FILENAME" -- $@
