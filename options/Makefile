SRCDIR := $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))
-include SubDir.mk ../SubDir.mk

ifeq ($(SRCDIR),.)
all: $(LIBS)/lib$(NAME).a
endif

$(LIBS)/lib$(NAME).a: $($(NAME)_CPP_OBJS)
	$(PRINTF) " AR\t$@\n"
	$(AT)$(AR) rcs $@ $^

CLEAN_LIBS+=$(LIBS)/lib$(NAME).a
