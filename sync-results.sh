#!/bin/sh
rsync -rvz --include '*/' --include '*.timings' --include '*.levels' --exclude '*' --prune-empty-dirs das5:~/gpu/data/ data/
