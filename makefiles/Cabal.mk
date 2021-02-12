CABAL_PKG_NAME:=$(shell sed -n 's/^Name: *\([-a-zA-Z0-9_]*\)$$/\1/p' $(SRCDIR)/*.cabal)
ifeq ($(SRCDIR),.)
all: haskell_pkg_$(CABAL_PKG_NAME)
endif
