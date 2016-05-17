V = 0
AT_0 := @
AT_1 :=
AT = $(AT_$(V))

ifeq ($(V), 1)
    PRINTF := @\#
else
    PRINTF := @printf
endif

TOP := $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))
DEST := build/$(TOP)

$(TOP)_CUDA_SRCS=$(wildcard $(TOP)/*.cu)
$(TOP)_CUDA_OBJS=$(patsubst %.cu, build/%.obj, $($(TOP)_CUDA_SRCS))

$(TOP)_CPP_SRCS=$(wildcard $(TOP)/*.cpp)
$(TOP)_CPP_OBJS=$(patsubst %.cpp, build/%.o, $($(TOP)_CPP_SRCS))

CLEANUP += $($(TOP)_CUDA_OBJS) $($(TOP)_CPP_OBJS) $(DEST)/lib$(TOP).a
CLEANUP += $(DEST)/device.o

-include $(patsubst %.cu, build/%.d, $($(TOP)_CUDA_SRCS))
-include $(patsubst %.cpp, build/%.d, $($(TOP)_CPP_SRCS))

.PHONY: all clean clean-$(TOP)

all: $(DEST)/lib$(TOP).a

$($(TOP)_CUDA_OBJS) $($(TOP)_CPP_OBJS): | $(DEST)/

$(DEST)/:
	$(AT)mkdir -p $@

$(DEST)/lib$(TOP).a: $($(TOP)_CPP_OBJS) $($(TOP)_CUDA_OBJS) $(DEST)/device.o
	$(PRINTF) " NVLINK\t$@\n"
	$(AT)$(NVLINK) --lib --output-file $@ $^

$(DEST)/device.o: $($(TOP)_CUDA_OBJS)
	$(PRINTF) " NVLINK\t$@\n"
	$(AT)$(NVLINK) $(NVCCFLAGS) --compiler-options "-Wno-deprecated" --device-link $^ --output-file $@

clean-$(TOP):
	$(AT)rm -rf $(CLEANUP)

clean: clean-$(TOP)
