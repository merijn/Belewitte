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
    DEST := build/$(DIRNAME)
    BUILD := build
endif

include $(BASE)/Rules.mk

$(NAME)_CPP_SRCS:=$(notdir $(wildcard $(SRCDIR)/*.cpp))
$(NAME)_CPP_OBJS:=$(patsubst %.cpp, $(DEST)/%.o, $($(NAME)_CPP_SRCS))

-include $(patsubst %.cpp, $(DEST)/%.d, $($(NAME)_CPP_SRCS))

$($(NAME)_CPP_OBJS): | $(DEST)/

CLEAN_OBJS+=$($(NAME)_CPP_OBJS)
CLEAN_DEPS+=$(DEST)/*.d
