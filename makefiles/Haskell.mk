ifdef CABAL
CABALCONFIG:=$(BASE)/cabal.project.local
ifeq ($(BASE)/$(PROJECTFILE).freeze, $(wildcard $(BASE)/$(PROJECTFILE).freeze))
CABALFREEZE:=$(BASE)/cabal.project.freeze
endif
endif

$(BASE)/cabal.project: $(BASE)/$(PROJECTFILE) $(BASE)/Config.mk
	$(PRINTF) " CP\t$(@F)\n"
	$(AT)cp $< $@

$(BASE)/cabal.project.freeze: $(BASE)/$(PROJECTFILE).freeze $(BASE)/Config.mk
	$(PRINTF) " CP\t$(@F)\n"
	$(AT)cp $< $@

.PHONY: haskell_%
haskell_%: $(CABALCONFIG) $(CABALFREEZE)
ifndef CABAL
	$(PRINTF) "cabal-install not found, skipping Haskell parts\n"
else
	$(PRINTF) " CABAL\t$*\n"
	$(AT)$(CABAL) --builddir="$(abspath $(BUILD)/haskell/)" \
	    v2-build $* $(if $(AT),2>/dev/null >/dev/null,)

$(CABALCONFIG): $(BASE)/cabal.project
	$(PRINTF) " CABAL\tconfigure\n"
	$(AT)$(CABAL) --builddir="$(abspath $(BUILD)/haskell/)" v2-update \
	    $(if $(AT),2>/dev/null >/dev/null,)
ifneq ($(BASE)/$(PROJECTFILE).freeze, $(wildcard $(BASE)/$(PROJECTFILE).freeze))
	$(AT)rm -f $(BASE)/cabal.project.freeze
endif
	$(AT)$(CABAL) --builddir="$(abspath $(BUILD)/haskell/)" \
	    --with-compiler="$(GHC)" -j24 v2-configure \
	    $(if $(AT),2>/dev/null >/dev/null,)
	$(AT)rm -f $(CABALCONFIG)~

.PHONY: freeze
freeze:
	$(PRINTF) "Generating frozen config.\n"
	$(AT)rm -f $(BASE)/cabal.project.freeze
	$(AT)$(CABAL) --builddir="$(abspath $(BUILD)/haskell/)" \
	    v2-freeze $(if $(AT),2>/dev/null >/dev/null,)
	$(AT)cp "$(BASE)/cabal.project.freeze" "$(BASE)/$(PROJECTFILE).freeze"

.PHONY: install-%
install-%: $(CABALCONFIG) $(CABALFREEZE) | $(TARGET)/
ifdef TARGET
	$(PRINTF) " CABAL\t$@\n"
	$(AT)cp $$($(CABAL) --builddir="$(abspath $(BUILD)/haskell/)" \
	    v2-exec -- command -v $*) $(TARGET)/ \
	    $(if $(AT),2>/dev/null >/dev/null,)
else
	$(PRINTF) "TARGET is not set.\n"
endif
endif
