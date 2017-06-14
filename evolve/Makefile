SRCDIR := $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))
-include SubDir.mk ../SubDir.mk

all: $(SRCDIR)/gen-graph $(SRCDIR)/evolve

$(DEST)/evolve.o: CXXFLAGS+=-I$(BOOST_PATH)/include -isystem$(BOOST_PATH)/include
$(DEST)/Connectivity.o: CPATH:=$(ICC_CPATH):$(CPATH)
$(DEST)/Connectivity.o: CXX=icc

$(SRCDIR)/evolve: LDFLAGS+=-ltbb
$(SRCDIR)/evolve: LD=icc

$(SRCDIR)/gen-graph: $(DEST)/gen-graph.o $(LIBS)/libutils.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $^ -o $@

$(SRCDIR)/evolve: $(DEST)/evolve.o $(DEST)/Connectivity.o $(LIBS)/libutils.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $^ -o $@

CLEAN_BINS+=$(SRCDIR)/gen-graph $(SRCDIR)/evolve
