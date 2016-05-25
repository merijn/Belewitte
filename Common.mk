.PHONY: all clean clean-objs clean-bins clean-libs clean-all

all:

clean: clean-objs

CLEAN_OBJS:=
CLEAN_BINS:=
CLEAN_LIBS:=

clean-objs:
	$(PRINTF) "cleaning object files...\n"
	$(AT)rm -rf $(CLEAN_OBJS)

clean-libs:
	$(PRINTF) "cleaning libraries...\n"
	$(AT)rm -rf $(CLEAN_LIBS)

clean-bins:
	$(PRINTF) "cleaning executables...\n"
	$(AT)rm -rf $(CLEAN_BINS)

clean-all:
	$(PRINTF) "cleaning...\n"
	$(AT)rm -rf $(CLEAN_OBJS)
	$(AT)rm -rf $(CLEAN_LIBS)
	$(AT)rm -rf $(CLEAN_BINS)

.DELETE_ON_ERROR:

V = 0
AT_0 := @
AT_1 :=
AT = $(AT_$(V))

ifeq ($(V), 1)
    PRINTF := @\#
else
    PRINTF := @printf
endif

COMMON_CFLAGS=-O3 -MMD -MP -std=c++14 -g $(EXTRA_CFLAGS)

CLANGWFLAGS=-Weverything -Wno-c++98-compat -Wno-c++98-compat-pedantic \
         -Wno-documentation-deprecated-sync -Wno-documentation -Wno-padded \
         -Wno-unused-const-variable -Wno-reserved-id-macro
CLANGFLAGS=$(COMMON_CFLAGS) $(CLANGWFLAGS) -ftrapv

ICCWFLAGS=-Wall -Wremarks -Wcheck -Werror -diag-disable=869,981,11074,11076
ICC_CFLAGS=$(COMMON_CFLAGS) $(ICCWFLAGS) -xHost

CC=clang++
CFLAGS=$(if $(findstring clang++, $(CC)), $(CLANGFLAGS), \
            $(if $(findstring icc, $(CC)), $(ICC_CFLAGS), $(COMMON_CFLAGS)))
LDFLAGS=-lcudart -ldl -g $(EXTRA_LDFLAGS)

LD=$(CC)

NVCC?=nvcc
NVCCFLAGS=-std=c++11 -O3 -g -G -lineinfo
NVCCARCHFLAGS= \
    -gencode arch=compute_20,code=sm_20 \
    -gencode arch=compute_20,code=sm_21 \
    -gencode arch=compute_30,code=sm_30 \
    -gencode arch=compute_35,code=sm_35 \
    -gencode arch=compute_50,code=sm_50 \
    -gencode arch=compute_52,code=sm_52 \
    -gencode arch=compute_53,code=sm_53

NVLINK=$(NVCC)

UNAME := $(shell uname -s)
ifeq ($(UNAME),Darwin)
    CFLAGS += -isystem/Developer/NVIDIA/CUDA-7.5/include/
    LDFLAGS += -L/usr/local/cuda/lib -framework opencl

    NVWFLAGS = $(CLANGWFLAGS) -Wno-unused-macros -Wno-c++11-long-long \
               -Wno-old-style-cast -Wno-used-but-marked-unused \
               -Wno-unused-function -Wno-missing-variable-declarations \
               -Wno-pedantic -Wno-missing-prototypes -Wno-unused-parameter

    NVCCFLAGS += --compiler-options "$(NVWFLAGS)" \
                 -isystem /Developer/NVIDIA/CUDA-7.5/include/

endif
ifeq ($(UNAME),Linux)
    CLANGWFLAGS += -Wno-reserved-id-macro
    CLANGFLAGS += --gcc-toolchain=$(addsuffix .., $(dir $(shell which gcc)))

    CFLAGS += -isystem$(CUDA_PATH)/include/
    LDFLAGS += -L$(CUDA_PATH)/lib64 -lOpenCL

    NVWFLAGS =

    NVCCFLAGS += --compiler-options "$(NVWFLAGS)"
endif

$(DEST):
	$(AT)mkdir -p $@

$(DEST)/%.o: %.cpp | $(DEST)/
	$(PRINTF) " CXX\t$*.cpp\n"
	$(AT)$(CC) $(CFLAGS) -I. $< -c -o $@

$(DEST)/%.obj: %.cu | $(DEST)/
	$(PRINTF) " NVCC\t$*.cu\n"
	$(AT)$(NVCC) $(NVCCFLAGS) -M -I. $< -o build/$*.d
	$(AT)$(NVCC) $(NVCCFLAGS) $(NVCCARCHFLAGS) -I. --device-c $< -o $@
