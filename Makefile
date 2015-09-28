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

CC=clang++
WFLAGS = -Weverything -Wno-c++98-compat -Wno-c++98-compat-pedantic \
         -Wno-documentation-deprecated-sync -Wno-documentation -Wno-padded \
         -Wno-unused-const-variable

CFLAGS=-MMD -MP -std=c++14 $(WFLAGS) -g
LDFLAGS=-lcudart -ldl -g

LD=$(CC)

NVCC=nvcc
NVCCFLAGS=-std=c++11 -O3 -g -G -lineinfo \
    -gencode arch=compute_20,code=sm_20 \
    -gencode arch=compute_20,code=sm_21 \
    -gencode arch=compute_30,code=sm_30 \
    -gencode arch=compute_35,code=sm_35

NVLINK=$(NVCC)

UNAME := $(shell uname -s)
ifeq ($(UNAME),Darwin)
    CFLAGS += -isystem/Developer/NVIDIA/CUDA-7.0/include/
    LDFLAGS += -L/usr/local/cuda/lib -framework opencl

    NVWFLAGS = $(WFLAGS) -Wno-unused-macros -Wno-c++11-long-long \
               -Wno-old-style-cast -Wno-used-but-marked-unused \
               -Wno-unused-function -Wno-missing-variable-declarations \
               -Wno-pedantic -Wno-missing-prototypes -Wno-unused-parameter

    NVCCFLAGS += --compiler-options "$(NVWFLAGS)" \
                 -isystem /Developer/NVIDIA/CUDA-7.0/include/

endif
ifeq ($(UNAME),Linux)
    WFLAGS += -Wno-reserved-id-macro
    CFLAGS += --gcc-toolchain=$(addsuffix .., $(dir $(shell which gcc))) \
              -isystem$(CUDA_PATH)/include/
    LDFLAGS += -L$(CUDA_PATH)/lib64 -lOpenCL

    NVWFLAGS =

    NVCCFLAGS += --compiler-options "$(NVWFLAGS)"
endif

CLEANUP = main normalise build/*.o

all: main normalise gen-graph reorder-graph check-degree

-include $(patsubst %.cpp, build/%.d, $(wildcard *.cpp))

DIRS=bfs pagerank
include $(foreach d, $(DIRS), $d/Makefile)

main: build/main.o build/CUDA.o build/OpenCL.o build/Timer.o build/Util.o \
      build/GraphFile.o build/bfs.o build/bfs/libbfs.a build/pagerank.o \
      build/pagerank/libpagerank.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(CFLAGS) $(LDFLAGS) $^ -o $@

normalise: build/normalise.o build/Util.o build/GraphFile.o
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(CFLAGS) -ldl $^ -o $@

gen-graph: build/gen-graph.o build/GraphFile.o
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(CFLAGS) $^ -o $@

reorder-graph: build/reorder-graph.o build/GraphFile.o
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(CFLAGS) $^ -o $@

check-degree: build/check-degree.o build/GraphFile.o
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(CFLAGS) $^ -o $@

build:
	$(AT)mkdir -p $@

build/%.o: %.cpp | build/
	$(PRINTF) " CXX\t$*.cpp\n"
	$(AT)$(CC) $(CFLAGS) $< -c -o $@

build/%.o: %.cu | build/
	$(PRINTF) " NVCC\t$*.cu\n"
	$(AT)$(NVCC) -M $< -o build/$*.d
	$(AT)$(NVCC) $(NVCCFLAGS) --device-c $< -o $@

clean:
	$(PRINTF) "cleaning...\n"
	$(AT)rm -rf $(CLEANUP)
