$(NAME)_CABAL:=$(wildcard $(SRCDIR)/*.cabal)
$(NAME)_CABAL_BINS:=$(patsubst %,$(SRCDIR)/%, \
    $(shell sed -n "s/^Executable \(.*\)/\1/p" $($(NAME)_CABAL)))
$(NAME)_CABAL_FIRST_BIN:=$(notdir $(firstword $($(NAME)_CABAL_BINS)))

$(NAME)_CABAL_FILES:=$(filter %.cabal,\
    $(shell sed -n "s/^packages: \(.*\)/\1/p" $(SRCDIR)/cabal.project))
ifneq ($(SRCDIR),.)
    $(NAME)_CABAL_FILES:=$(patsubst ./%,$(SRCDIR)/%,$($(NAME)_CABAL_FILES))
endif
$(NAME)_CABAL_FILES:=$(patsubst ../%,$(BASE)/%,$($(NAME)_CABAL_FILES))
$(NAME)_CABAL_DIRS:=$(foreach file, $($(NAME)_CABAL_FILES), $(dir $(file)))

$($(NAME)_CABAL_FILES): | $(filter-out ./,$($(NAME)_CABAL_DIRS))
$($(NAME)_CABAL_BINS): | $(DEST)/$(NAME).mk

$(DEST)/$(NAME).mk: NAME:=$(NAME)
$(DEST)/$(NAME).mk: SRCDIR:=$(SRCDIR)
$(DEST)/$(NAME).mk: $($(NAME)_CABAL_FILES) $(BASE)/cabal.awk | $(DEST)/
	$(PRINTF) " CABAL\t$(NAME) (new-build)\n"
	$(AT)cd $(SRCDIR); cabal new-build --builddir="$(PWD)/$(dir $@)" \
	$(if $(AT),>/dev/null,)
	$(PRINTF) " CABAL\t$(NAME) (dependencies)\n"
	$(AT)cabal-plan --builddir="$(dir $@)" list-bins \
	    --prefix "$(patsubst %.cabal,%,$(notdir $($(NAME)_CABAL)))" \
	| $(BASE)/cabal.awk "$($(NAME)_CABAL_FIRST_BIN)" \
	    "$(foreach dir,$($(NAME)_CABAL_DIRS),\
		$(shell find $(wildcard $(CURDIR)/$(dir)) -iname '*.hs'))" >$@

-include $(DEST)/$(NAME).mk

.PHONY: clean-$(NAME)-cabal-bins

clean-$(NAME)-cabal-bins: NAME:=$(NAME)
clean-$(NAME)-cabal-bins:
	$(PRINTF) "cleaning executables for: $(NAME)\n"
	$(AT)rm -rf $($(NAME)_CABAL_BINS)

clean-bins: clean-$(NAME)-cabal-bins
