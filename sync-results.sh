#!/bin/sh
rsync -rvz --include '*/' --include '*.frontier' --include '*.timings' --include '*.visited' --exclude '*' --prune-empty-dirs das5:/var/scratch/mverstra/data/ data/
