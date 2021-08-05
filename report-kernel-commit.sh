#!/usr/bin/env sh

cd "$(dirname $0)"

if [ -d ".hg" ]; then
    COMMIT="$(hg log -r "last(modifies('set:$1/* - set:$1/Makefile'))" -T "{gitnode}\n")"
elif [ -d ".git" ]; then
    COMMIT="$(git log -n 1 --pretty=format:%H $1 ":!$1/Makefile")"
fi

if [ -n "$COMMIT" ]; then
    printf "$COMMIT"
else
    printf "Unknown\n"
fi
