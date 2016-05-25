SRCDIR := $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))

ifeq ($(SRCDIR),.)
NAME := $(notdir $(CURDIR))
DEST := ../build/$(NAME)
include ../Common.mk
else
NAME := $(SRCDIR)
DEST := build/$(SRCDIR)
endif

$(NAME)_CUDA_SRCS:=$(notdir $(wildcard $(SRCDIR)/*.cu))
$(NAME)_CUDA_OBJS:=$(patsubst %.cu, $(DEST)/%.obj, $($(NAME)_CUDA_SRCS))

$(NAME)_CPP_SRCS:=$(notdir $(wildcard $(SRCDIR)/*.cpp))
$(NAME)_CPP_OBJS:=$(patsubst %.cpp, $(DEST)/%.o, $($(NAME)_CPP_SRCS))

-include $(patsubst %.cu, $(DEST)/%.d, $($(NAME)_CUDA_SRCS))
-include $(patsubst %.cpp, $(DEST)/%.d, $($(NAME)_CPP_SRCS))
-include $(SRCDIR)/Extra.mk

all: $(DEST)/lib$(NAME).a

$($(NAME)_CUDA_OBJS) $($(NAME)_CPP_OBJS): | $(DEST)/

$(DEST)/:
	$(AT)mkdir -p $@

$(DEST)/lib$(NAME).a: $($(NAME)_CPP_OBJS) $($(NAME)_CUDA_OBJS) $(DEST)/device.o
	$(PRINTF) " NVLINK\t$@\n"
	$(AT)$(NVLINK) --lib --output-file $@ $^

$(DEST)/device.o: $($(NAME)_CUDA_OBJS)
	$(PRINTF) " NVLINK\t$@\n"
	$(AT)$(NVLINK) $(NVCCFLAGS) --compiler-options "-Wno-deprecated" --device-link $^ --output-file $@

CLEAN_OBJS+=$($(NAME)_CUDA_OBJS) $($(NAME)_CPP_OBJS) $(DEST)/device.o
CLEAN_LIBS+=$(DEST)/lib$(NAME).a
