#!/usr/bin/env sh
SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
FILENAME="${0##*/}"
BUILDDIR="$(basename $SCRIPTPATH)"
`cabal-plan --builddir="$SCRIPTPATH/../.build/$BUILDDIR" list-bin --suffix "$FILENAME"` $*
