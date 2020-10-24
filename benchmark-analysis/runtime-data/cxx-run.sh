#!/usr/bin/env sh
BASEPATH="$( cd "$(dirname "$0")/../../" ; pwd -P )"

unset MAKEFLAGS
unset MAKELEVEL

CXX="$(make -sC "$BASEPATH/" report-cxx 2>/dev/null)"
if [ $? -ne 0 ] || [ -z "$CXX" ]; then
    printf "C++17 compiler not found!"
    exit 1
fi

exec $CXX -shared -fPIC -x c++ /dev/stdin -o "$@"
