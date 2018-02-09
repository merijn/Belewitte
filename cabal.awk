#!/usr/bin/env bash
awk -v firstbin="$1" -v srcs="$2" '
{
    if (match($0, "^[^:]*:exe:[^ ]*  ")) {
        front = substr($0, RSTART, RLENGTH-2)
        back = substr($0, RSTART+RLENGTH)
        if (sub("^[^:]*:[^:]*:", "", front)) {
            if (front == firstbin) {
                printf("%s: SRCDIR:=$(SRCDIR)\n", back)
                printf("%s: DEST:=$(DEST)\n%s: %s\n", back, back, srcs)
                printf("\t$(PRINTF) \" CABAL\t%s\\n\"\n", front)
                printf("\t$(AT)cd $(SRCDIR); cabal new-build ")
                printf("--builddir=\"$(PWD)/$(DEST)\" $(if $(AT),>/dev/null,)\n\n")
            } else {
                printf("$(SRCDIR)/%s: $(SRCDIR)/%s\n\n", front, firstbin)
            }
        }
    }
}
'
