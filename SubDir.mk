ifeq ($(SRCDIR),.)
NAME := $(notdir $(CURDIR))
DEST := ../build/$(NAME)
BUILD := ../build
BASE := ..
include ../Common.mk
CXXFLAGS += -I../
NVCCXXFLAGS += -I../
else
NAME := $(SRCDIR)
DEST := build/$(SRCDIR)
BUILD := build
BASE := .

$(DEST)/:
	$(AT)mkdir -p $@
endif

$(NAME)_CPP_SRCS:=$(notdir $(wildcard $(SRCDIR)/*.cpp))
$(NAME)_CPP_OBJS:=$(patsubst %.cpp, $(DEST)/%.o, $($(NAME)_CPP_SRCS))

-include $(patsubst %.cpp, $(DEST)/%.d, $($(NAME)_CPP_SRCS))

$($(NAME)_CPP_OBJS): | $(DEST)/

CLEAN_OBJS+=$($(NAME)_CPP_OBJS)
CLEAN_DEPS+=$(DEST)/*.d
