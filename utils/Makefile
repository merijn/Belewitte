SRCDIR := $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))
-include SubDir.mk ../SubDir.mk

all: $(BUILD)/lib$(NAME).a

$(BUILD)/lib$(NAME).a: $($(NAME)_CPP_OBJS)
	$(PRINTF) " AR\t$@\n"
	$(AT)$(AR) rcs $@ $^

CLEAN_LIBS+=$(BUILD)/lib$(NAME).a
