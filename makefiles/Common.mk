.PHONY: all clean debug everything ptx clean-% asan msan ssan tsan

include $(BASE)/Config.mk

# Set defaults if not defined in Config.mk
ifndef CXX
CXX:=$(shell command -v clang++ 2> /dev/null)
endif
ifndef CABAL
CABAL:=$(shell command -v cabal 2> /dev/null)
endif
ifndef GHC
GHC:=$(shell command -v ghc 2> /dev/null)
endif
ifndef NVCC
NVCC:=$(shell command -v nvcc 2> /dev/null)
endif
ifndef PROJECTFILE
GHC_VERSION_PARTS:=$(subst ., ,$(shell $(GHC) --numeric-version))
GHC_VERSION:=$(word 1,$(GHC_VERSION_PARTS)).$(word 2,$(GHC_VERSION_PARTS))
ifneq ($(wildcard $(BASE)/cabal.project.ghc-$(GHC_VERSION)),)
PROJECTFILE:=cabal.project.ghc-$(GHC_VERSION)
else
PROJECTFILE:=cabal.project.ghc-8.10
endif
endif

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
clean-all: clean-objs clean-libs clean-deps clean-deps clean-ptx clean-bins \
    clean-debug
	$(PRINTF) "removing build directory...\n"
	$(AT)rm -rf $(BUILD) $(BASE)/cabal.project.local

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

.PHONY: report-cabal
report-cabal:
	$(PRINTF) "$(CABAL)"

.PHONY: report-cxx
report-cxx:
	$(PRINTF) "$(CXX) $(filter-out -MMD -MP,$(CXXFLAGS))"

.PHONY: missing-cuda
missing-cuda:
	$(PRINTF) "nvcc not found, skipping GPU kernel libraries\n"

COMMON_CXXFLAGS=-MMD -MP -std=c++17 -g -I$(BASE)

CLANGWFLAGS=-Weverything -Wno-c++98-compat -Wno-c++98-compat-pedantic \
         -Wno-documentation-deprecated-sync -Wno-documentation -Wno-padded \
         -Wno-unused-const-variable -Wno-reserved-id-macro \
         -Wno-global-constructors -Wno-exit-time-destructors

CLANGCXXFLAGS=$(COMMON_CXXFLAGS) $(CLANGWFLAGS) -ftrapv

CXXFLAGS=$(if $(findstring clang++, $(CXX)), $(CLANGCXXFLAGS), $(COMMON_CXXFLAGS))

CXX_IS_CLANG:=$(findstring clang++, $(CXX))

LD=$(CXX)
LDFLAGS=-ldl -g

ifdef CXX_IS_CLANG
    CLANG_LIB_PATH:=$(shell $(CXX) --version | grep "^InstalledDir: " | sed 's/InstalledDir: //')
    LDFLAGS+= -rpath $(CLANG_LIB_PATH)/../lib/
endif

NVCC?=nvcc
NVCCXXFLAGS?=-std=c++11 -O3 -g -Wno-deprecated-declarations
NVCCARCHFLAGS?= \
    -gencode arch=compute_30,code=sm_30 \
    -gencode arch=compute_35,code=sm_35 \
    -gencode arch=compute_50,code=sm_50 \
    -gencode arch=compute_52,code=sm_52 \
    -gencode arch=compute_53,code=sm_53 \
    -gencode arch=compute_60,code=sm_60 \
    -gencode arch=compute_61,code=sm_61 \
    -gencode arch=compute_62,code=sm_62 \
    -gencode arch=compute_70,code=sm_70 \
    -gencode arch=compute_72,code=sm_72 \
    -gencode arch=compute_75,code=sm_75

PTXARCH?=sm_53

NVCCHOSTCXXFLAGS?=

NVLINK=$(NVCC)
SED?=sed

DOWNLOAD:=$(BUILD)/download
PREFIX:=$(BUILD)/prefix

LIBS := $(BUILD)
UNAME := $(shell uname -s)
ifeq ($(UNAME),Darwin)
    CXXFLAGS += -isystem$(CUDA_PATH)/include/ -Wno-undefined-func-template

    CLANGWFLAGS += -Wno-poison-system-directories
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
    LDFLAGS += -lpthread

ifdef CXX_IS_CLANG
    LD += -stdlib=libc++
endif

    NVWFLAGS =

    NVCCXXFLAGS += --compiler-options "$(NVWFLAGS) $(NVCCHOSTCXXFLAGS)" \
                   -Wno-deprecated-gpu-targets
endif

$(BUILD)/kernels/ $(DOWNLOAD)/ $(PREFIX)/ $(TARGET)/:
	$(PRINTF) " MKDIR\t$@\n"
	$(AT)mkdir -p $@

PKG_CONFIG_PATH:=$(patsubst :%,%,$(patsubst %:,%,$(PKG_CONFIG_PATH)))

include $(BASE)/makefiles/Haskell.mk
include $(BASE)/makefiles/Boost.mk
