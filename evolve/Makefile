SRCDIR := $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))
-include makefiles/SubDir.mk ../makefiles/SubDir.mk
-include makefiles/Cabal.mk ../makefiles/Cabal.mk

all: $(SRCDIR)/gen-graph $(SRCDIR)/evolve

$(DEST)/evolve.o: $(BOOST_PREREQ)

$(SRCDIR)/evolve: LDFLAGS+=-ltbb $(if $(TBB_LIB_PATH), -L$(TBB_LIB_PATH))

$(DEST)/Connectivity.o: CPATH:=$(if $(TBB_INCLUDE_PATH), $(TBB_INCLUDE_PATH):)$(CPATH)

$(SRCDIR)/gen-graph: $(DEST)/gen-graph.o $(LIBS)/libutils.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $^ -o $@

$(SRCDIR)/evolve: $(DEST)/evolve.o $(DEST)/Connectivity.o $(LIBS)/libutils.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $^ -o $@

.PHONY: clean-$(NAME)-bins

clean-$(NAME)-bins: SRCDIR:=$(SRCDIR)
clean-$(NAME)-bins: NAME:=$(NAME)
clean-$(NAME)-bins:
	$(PRINTF) "cleaning executables for: $(NAME)\n"
	$(AT)rm -rf $(SRCDIR)/gen-graph $(SRCDIR)/evolve

clean-bins: clean-$(NAME)-bins

ifeq ($(SRCDIR),.)
include $(BASE)/utils/Makefile
endif
