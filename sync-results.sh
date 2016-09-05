#!/bin/sh
rsync -rvz --include '*/' --include '*.timings' --exclude '*' --prune-empty-dirs das5:~/gpu/data/ data/
