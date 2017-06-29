SRCDIR := $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))
-include SubDir.mk ../SubDir.mk

ifeq ($(SRCDIR),.)
all: $(LIBS)/lib$(NAME).a
endif

$(LIBS)/lib$(NAME).a: $($(NAME)_CPP_OBJS)
	$(PRINTF) " AR\t$(subst ../$(BUILD)/,,$@)\n"
	$(AT)$(AR) rcs $@ $^

.PHONY: clean-$(NAME)-libs

clean-$(NAME)-libs: NAME:=$(NAME)
clean-$(NAME)-libs:
	$(PRINTF) "cleaning libs for: $(NAME)\n"
	$(AT)rm -rf $(LIBS)/lib$(NAME).so

clean-libs: clean-$(NAME)-libs
