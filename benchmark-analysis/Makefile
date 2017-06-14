SRCDIR := $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))
-include SubDir.mk ../SubDir.mk

all: $(SRCDIR)/graph-details

$(DEST)/graph-details.o: \
    CXXFLAGS+=-I$(BOOST_PATH)/include -isystem$(BOOST_PATH)/include

$(SRCDIR)/graph-details: \
    $(DEST)/graph-details.o $(LIBS)/liboptions.a $(LIBS)/libutils.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) -L$(BOOST_PATH)/lib/ -lboost_system -lboost_filesystem $^ -o $@

CLEAN_BINS+=$(SRCDIR)/graph-details

ifeq ($(SRCDIR),.)
include $(BASE)/options/Makefile
include $(BASE)/utils/Makefile
endif
