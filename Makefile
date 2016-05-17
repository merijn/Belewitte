.PHONY: all clean

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

NVCC=nvcc
NVCCFLAGS=-std=c++11 -O3 -g -G -lineinfo
NVCCARCHFLAGS=-gencode arch=compute_20,code=sm_20 \
    -gencode arch=compute_20,code=sm_21 \
    -gencode arch=compute_30,code=sm_30 \
    -gencode arch=compute_35,code=sm_35

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

build/Connectivity.o: CPATH:=$(ICC_CPATH):$(CPATH)
build/Connectivity.o: CC=icc

evolve: LDFLAGS+=-ltbb
evolve: LD=icc
endif

CLEANUP = main normalise build/*.o

all: main normalise gen-graph reorder-graph check-degree evolve print-graph

-include $(patsubst %.cpp, build/%.d, $(wildcard *.cpp))

DIRS=bfs pagerank
include $(foreach d, $(DIRS), $d/Makefile)

main: build/main.o build/CUDA.o build/OpenCL.o build/Timer.o build/Util.o \
      build/bfs/libbfs.a build/pagerank/libpagerank.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $^ -o $@

normalise: build/normalise.o build/Util.o
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $^ -o $@

gen-graph: build/gen-graph.o build/Util.o
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $^ -o $@

reorder-graph: build/reorder-graph.o build/Util.o
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $^ -o $@

check-degree: build/check-degree.o build/Util.o
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $^ -o $@

build/evolve.o: CFLAGS+=-I$(BOOST_PATH) -isystem$(BOOST_PATH)

evolve: build/evolve.o build/Util.o build/Connectivity.o
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $^ -o $@

print-graph: build/print-graph.o build/Util.o
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $^ -o $@

build:
	$(AT)mkdir -p $@

build/%.o: %.cpp | build/
	$(PRINTF) " CXX\t$*.cpp\n"
	$(AT)$(CC) $(CFLAGS) -I. $< -c -o $@

build/%.obj: %.cu | build/
	$(PRINTF) " NVCC\t$*.cu\n"
	$(AT)$(NVCC) $(NVCCFLAGS) -M -I. $< -o build/$*.d
	$(AT)$(NVCC) $(NVCCFLAGS) $(NVCCARCHFLAGS) -I. --device-c $< -o $@

clean:
	$(PRINTF) "cleaning...\n"
	$(AT)rm -rf $(CLEANUP)
