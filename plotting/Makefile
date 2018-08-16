SRCDIR := $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))
-include SubDir.mk ../SubDir.mk

all: $(SRCDIR)/graph-edges haskell-dependencies

$(call santargets,$(SRCDIR)/graph-edges): $(SRCDIR)/graph-edges%: \
    $(DEST)/graph-edges%.o $(LIBS)/liboptions%.a $(LIBS)/libutils%.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $^ -o $@

.PHONY: clean-$(NAME)-bins clean-$(NAME)-bin-%san

clean-$(NAME)-bin-asan: SRCDIR:=$(SRCDIR)
clean-$(NAME)-bin-asan: NAME:=$(NAME)
clean-$(NAME)-bin-asan:
	$(PRINTF) "cleaning asan for: $(NAME)\n"
	$(AT)rm -rf $(SRCDIR)/graph-edges.asan

clean-$(NAME)-bin-msan: SRCDIR:=$(SRCDIR)
clean-$(NAME)-bin-msan: NAME:=$(NAME)
clean-$(NAME)-bin-msan:
	$(PRINTF) "cleaning msan for: $(NAME)\n"
	$(AT)rm -rf $(SRCDIR)/graph-edges.msan

clean-$(NAME)-bin-ssan: SRCDIR:=$(SRCDIR)
clean-$(NAME)-bin-ssan: NAME:=$(NAME)
clean-$(NAME)-bin-ssan:
	$(PRINTF) "cleaning ssan for: $(NAME)\n"
	$(AT)rm -rf $(SRCDIR)/graph-edges.ssan

clean-$(NAME)-bin-tsan: SRCDIR:=$(SRCDIR)
clean-$(NAME)-bin-tsan: NAME:=$(NAME)
clean-$(NAME)-bin-tsan:
	$(PRINTF) "cleaning tsan for: $(NAME)\n"
	$(AT)rm -rf $(SRCDIR)/graph-edges.tsan

clean-asan: clean-$(NAME)-bin-asan
clean-msan: clean-$(NAME)-bin-msan
clean-ssan: clean-$(NAME)-bin-ssan
clean-tsan: clean-$(NAME)-bin-tsan

clean-$(NAME)-bins: SRCDIR:=$(SRCDIR)
clean-$(NAME)-bins: NAME:=$(NAME)
clean-$(NAME)-bins:
	$(PRINTF) "cleaning dependencies for: $(NAME)\n"
	$(AT)rm -rf $(SRCDIR)/graph-edges

clean-bins: clean-$(NAME)-bins

ifeq ($(SRCDIR),.)
include $(BASE)/options/Makefile
include $(BASE)/utils/Makefile
endif
