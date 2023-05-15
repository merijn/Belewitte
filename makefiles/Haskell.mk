$(BASE)/cabal.project: $(PROJECTFILE) $(BASE)/Config.mk
	$(PRINTF) " CP\t$(@F)\n"
	$(AT)cp $< $@

ifeq ($(FREEZEFILE), $(wildcard $(FREEZEFILE)))
$(BASE)/cabal.project.freeze: $(FREEZEFILE) $(BASE)/Config.mk
	$(PRINTF) " CP\t$(@F)\n"
	$(AT)cp $< $@
else
$(BASE)/cabal.project.freeze: $(BASE)/Config.mk
	$(PRINTF) " MK\t$(@F)\n"
	$(AT)touch $@
endif

.PHONY: haskell_%
haskell_%: $(BASE)/cabal.project.local $(BASE)/cabal.project.freeze
ifndef CABAL
	$(PRINTF) "cabal-install not found, skipping Haskell parts\n"
else
	$(PRINTF) " CABAL\t$(subst _,:,$*)\n"
	$(AT)$(CABAL) --builddir="$(abspath $(BUILD)/haskell/)" \
	    v2-build $(subst _,:,$*) $(if $(AT),2>/dev/null >/dev/null,)

$(BASE)/cabal.project.local: $(BASE)/cabal.project $(BASE)/Config.mk
	$(PRINTF) " CABAL\tconfigure\n"
	$(AT)$(CABAL) --builddir="$(abspath $(BUILD)/haskell/)" v2-update \
	    $(if $(AT),2>/dev/null >/dev/null,)
ifneq ($(FREEZEFILE), $(wildcard $(FREEZEFILE)))
	$(AT)rm -f $(BASE)/cabal.project.freeze
endif
	$(AT)$(CABAL) --builddir="$(abspath $(BUILD)/haskell/)" \
	    --with-compiler="$(GHC)" -j24 v2-configure \
	    $(if $(AT),2>/dev/null >/dev/null,)
	$(AT)rm -f $(BASE)/cabal.project.local~

.PHONY: freeze
freeze: $(BASE)/cabal.project
	$(PRINTF) "Generating frozen config.\n"
	$(AT)rm -f $(BASE)/cabal.project.freeze
	$(AT)$(CABAL) --builddir="$(abspath $(BUILD)/haskell/)" \
	    v2-freeze $(if $(AT),2>/dev/null >/dev/null,)
	$(AT)cp "$(BASE)/cabal.project.freeze" "$(FREEZEFILE)"

.PHONY: install-%
install-%: $(BASE)/cabal.project.local $(BASE)/cabal.project.freeze | $(TARGET)/
ifdef TARGET
	$(PRINTF) " CABAL\t$@\n"
	$(AT)cp $$($(CABAL) --builddir="$(abspath $(BUILD)/haskell/)" \
	    v2-exec -- command -v $*) $(TARGET)/ \
	    $(if $(AT),2>/dev/null >/dev/null,)
else
	$(PRINTF) "TARGET is not set.\n"
endif
endif
