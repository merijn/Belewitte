ifeq ($(SRCDIR),.)
    NAME := $(notdir $(CURDIR))
    DEST := ../build/$(NAME)
    BUILD := ../build
    BASE := ..
    include ../Common.mk
    CXXFLAGS += -I../
    NVCCXXFLAGS += -I../
else
    DIRNAME := $(patsubst ../%,%,$(SRCDIR))
    NAME := $(DIRNAME)
    DEST := $(findstring ../,$(SRCDIR))build/$(DIRNAME)
    BUILD := $(findstring ../,$(SRCDIR))build
endif

include $(BASE)/Rules.mk

$(NAME)_CPP_SRCS:=$(notdir $(wildcard $(SRCDIR)/*.cpp))
$(NAME)_CPP_OBJS:=$(patsubst %.cpp, $(DEST)/%.o, $($(NAME)_CPP_SRCS))

-include $(patsubst %.cpp, $(DEST)/%.d, $($(NAME)_CPP_SRCS))

$($(NAME)_CPP_OBJS): | $(DEST)/

.PHONY: clean-$(NAME)-objs clean-$(NAME)-deps

clean-$(NAME)-objs: NAME:=$(NAME)
clean-$(NAME)-objs:
	$(PRINTF) "cleaning objects for: $(NAME)\n"
	$(AT)rm -rf $($(NAME)_CPP_OBJS)

clean-$(NAME)-deps: NAME:=$(NAME)
clean-$(NAME)-deps: DEST:=$(DEST)
clean-$(NAME)-deps:
	$(PRINTF) "cleaning dependencies for: $(NAME)\n"
	$(AT)rm -rf $(DEST)/*.d

clean-objs: clean-$(NAME)-objs
clean-deps: clean-$(NAME)-deps
