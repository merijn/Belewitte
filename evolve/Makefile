SRCDIR := $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))
-include SubDir.mk ../SubDir.mk

all: $(SRCDIR)/gen-graph $(SRCDIR)/evolve haskell-dependencies

$(DEST)/evolve.o: CXXFLAGS+=$(BOOST_CXX_FLAGS)
$(DEST)/evolve.o: $(BOOST_PREREQ)

$(SRCDIR)/evolve: LDFLAGS+=-ltbb

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
