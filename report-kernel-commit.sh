#!/usr/bin/env sh

cd "$(dirname $0)"

if [ -d ".hg" ]; then
    COMMIT="$(hg log -r "last(modifies('$1/*'))" -T "{gitnode}\n")"
elif [ -d ".git" ]; then
    COMMIT="$(git log -n 1 --pretty=format:%H $1)"
fi

if [ -n "$COMMIT" ]; then
    printf "$COMMIT"
else
    printf "Unknown\n"
fi
