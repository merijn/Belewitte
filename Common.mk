.PHONY: all clean debug everything ptx clean-% asan msan ssan tsan

all:
debug: asan msan ssan tsan
everything: all debug

clean: clean-objs clean-ptx

clean-objs:
clean-ptx:
clean-libs:
clean-bins:
clean-deps:
clean-debug: clean-asan clean-msan clean-ssan clean-tsan
clean-asan:
clean-msan:
clean-ssan:
clean-tsan:
clean-all: clean-objs clean-libs clean-deps clean-deps clean-ptx
	$(PRINTF) "removing build directory...\n"
	$(AT)rm -rf $(BUILD)

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

COMMON_CXXFLAGS=-O3 -MMD -MP -std=c++17 -g -I$(BASE)

CLANGWFLAGS=-Weverything -Wno-c++98-compat -Wno-c++98-compat-pedantic \
         -Wno-documentation-deprecated-sync -Wno-documentation -Wno-padded \
         -Wno-unused-const-variable -Wno-reserved-id-macro \
         -Wno-global-constructors -Wno-exit-time-destructors

CLANGCXXFLAGS=$(COMMON_CXXFLAGS) $(CLANGWFLAGS) -ftrapv

ICCWFLAGS=-Wall -Wremarks -Wcheck -Werror -diag-disable=869,981,10382,11074,11076
ICC_CXXFLAGS=$(COMMON_CXXFLAGS) $(ICCWFLAGS) -xHost


CXX?=clang++
CXXFLAGS=$(if $(findstring clang++, $(CXX)), $(CLANGCXXFLAGS), \
            $(if $(findstring icc, $(CXX)), $(ICC_CXXFLAGS), $(COMMON_CXXFLAGS)))

LDFLAGS=-ldl -g

LD=$(CXX)

NVCC?=nvcc
NVCCXXFLAGS?=-std=c++11 -O3 -g -Wno-deprecated-declarations
NVCCARCHFLAGS?= \
    -gencode arch=compute_30,code=sm_30 \
    -gencode arch=compute_35,code=sm_35 \
    -gencode arch=compute_50,code=sm_50 \
    -gencode arch=compute_52,code=sm_52 \
    -gencode arch=compute_53,code=sm_53

PTXARCH?=sm_53

NVCCHOSTCXXFLAGS?=

NVLINK=$(NVCC)
SED?=sed

BOOST_PATH?=$(HOME)/opt/
BOOST_CXX_FLAGS=-I$(BOOST_PATH)/include -isystem$(BOOST_PATH)/include
BOOST_LD_FLAGS=-L$(BOOST_PATH)/lib -L$(BOOST_PATH)/lib64

LIBS := $(BUILD)
UNAME := $(shell uname -s)
ifeq ($(UNAME),Darwin)
    CXXFLAGS += -isystem$(CUDA_PATH)/include/ -Wno-undefined-func-template

    DYLIBLDFLAGS += -flat_namespace -undefined suppress

    NVWFLAGS = $(CLANGWFLAGS) -Wno-unused-macros -Wno-c++11-long-long \
               -Wno-old-style-cast -Wno-used-but-marked-unused \
               -Wno-unused-function -Wno-missing-variable-declarations \
               -Wno-pedantic -Wno-missing-prototypes -Wno-unused-parameter \
               -Wno-missing-noreturn

    NVCCXXFLAGS += --compiler-options "$(NVWFLAGS) $(NVCCHOSTCXXFLAGS)" \
                   -isystem $(CUDA_PATH)/include/

endif
ifeq ($(UNAME),Linux)
    CLANGWFLAGS += -Wno-reserved-id-macro
    CLANGCXXFLAGS += -stdlib=libc++

    CXXFLAGS += -isystem$(CUDA_PATH)/include/

ifeq ($(LD),clang++)
    LD += -stdlib=libc++
endif

    NVWFLAGS =

    NVCCXXFLAGS += --compiler-options "$(NVWFLAGS) $(NVCCHOSTCXXFLAGS)" \
                   -Wno-deprecated-gpu-targets
endif

$(BUILD)/kernels/:
	$(AT)mkdir -p $@

.PHONY: haskell-dependencies
haskell-dependencies:
	$(PRINTF) " CABAL\t$@\n"
	$(AT)cabal --builddir="$(abspath $(BUILD)/haskell/)" \
	    new-build all $(if $(AT),2>/dev/null >/dev/null,)
