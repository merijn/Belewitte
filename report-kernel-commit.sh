#!/usr/bin/env sh

cd "$(dirname $0)"

if [ -d ".hg" ]; then
    hg log -r "last(modifies('$1/*'))" -T "{gitnode}\n"
elif [ -d ".git" ]; then
    git log -n 1 --pretty=format:%H $1
else
    printf "Repository type couldn't be determined!\n"
    exit 1
fi
