SRCDIR := $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))

ifeq ($(SRCDIR),.)
NAME := $(notdir $(CURDIR))
DEST := ../build/$(NAME)
BUILD := ../build/
include ../Common.mk
else
NAME := $(SRCDIR)
DEST := build/$(SRCDIR)
BUILD := build/

$(DEST)/:
	$(AT)mkdir -p $@
endif

$(NAME)_CUDA_SRCS:=$(notdir $(wildcard $(SRCDIR)/*.cu))
$(NAME)_CUDA_OBJS:=$(patsubst %.cu, $(DEST)/%.obj, $($(NAME)_CUDA_SRCS))
$(NAME)_CUDA_PTXS:=$(patsubst %.cu, $(SRCDIR)/%.ptx, $($(NAME)_CUDA_SRCS))

$(NAME)_CPP_SRCS:=$(notdir $(wildcard $(SRCDIR)/*.cpp))
$(NAME)_CPP_OBJS:=$(patsubst %.cpp, $(DEST)/%.o, $($(NAME)_CPP_SRCS))

-include $(patsubst %.cu, $(DEST)/%.d, $($(NAME)_CUDA_SRCS))
-include $(patsubst %.cpp, $(DEST)/%.d, $($(NAME)_CPP_SRCS))
-include $(SRCDIR)/Extra.mk

all: $(DEST)/lib$(NAME).so

ptx: $($(NAME)_CUDA_PTXS)

$($(NAME)_CUDA_OBJS) $($(NAME)_CPP_OBJS): | $(DEST)/
ifeq ($(UNAME),Linux)
$($(NAME)_CPP_OBJS): CXXFLAGS+=-fPIC
$($(NAME)_CUDA_OBJS) $(DEST)/device.o: NVCCHOSTCXXFLAGS+=-fPIC
endif

$(DEST)/lib$(NAME).so: $($(NAME)_CPP_OBJS) $($(NAME)_CUDA_OBJS) $(DEST)/device.o
	$(PRINTF) " NVLINK\t$@\n"
	$(AT)$(LD) $(DYLIBLDFLAGS) -shared -o $@ $^

$(DEST)/device.o: NVCCHOSTCXXFLAGS+=-Wno-deprecated

$(DEST)/device.o: $($(NAME)_CUDA_OBJS)
	$(PRINTF) " NVLINK\t$@\n"
	$(AT)$(NVLINK) $(NVCCXXFLAGS) --device-link $^ --output-file $@

CLEAN_OBJS+=$($(NAME)_CUDA_OBJS) $($(NAME)_CPP_OBJS) $(DEST)/device.o
CLEAN_PTXS+=$($(NAME)_CUDA_PTXS)
CLEAN_LIBS+=$(DEST)/lib$(NAME).so
CLEAN_DEPS+=$(DEST)/*.d
