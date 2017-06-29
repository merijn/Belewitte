SRCDIR := $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))
-include SubDir.mk ../SubDir.mk

all: $(SRCDIR)/graph-details

$(DEST)/graph-details.o: \
    CXXFLAGS+=-I$(BOOST_PATH)/include -isystem$(BOOST_PATH)/include

$(SRCDIR)/graph-details: \
    $(DEST)/graph-details.o $(LIBS)/liboptions.a $(LIBS)/libutils.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) -L$(BOOST_PATH)/lib/ -lboost_system -lboost_filesystem $^ -o $@

.PHONY: clean-$(NAME)-bins

clean-$(NAME)-bins: SRCDIR:=$(SRCDIR)
clean-$(NAME)-bins: NAME:=$(NAME)
clean-$(NAME)-bins:
	$(PRINTF) "cleaning dependencies for: $(NAME)\n"
	$(AT)rm -rf $(SRCDIR)/graph-details

clean-bins: clean-$(NAME)-bins

ifeq ($(SRCDIR),.)
include $(BASE)/options/Makefile
include $(BASE)/utils/Makefile
endif
