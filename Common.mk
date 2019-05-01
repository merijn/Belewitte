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
clean-all: clean-objs clean-libs clean-deps clean-deps clean-ptx clean-bins \
    clean-debug
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

COMMON_CXXFLAGS=-MMD -MP -std=c++17 -g -I$(BASE)

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
    -gencode arch=compute_70,code=sm_70

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

ifdef CXX_IS_CLANG
    LD += -stdlib=libc++
endif

    NVWFLAGS =

    NVCCXXFLAGS += --compiler-options "$(NVWFLAGS) $(NVCCHOSTCXXFLAGS)" \
                   -Wno-deprecated-gpu-targets
endif

$(BUILD)/kernels/ $(DOWNLOAD)/ $(PREFIX)/:
	$(PRINTF) " MKDIR\t$@\n"
	$(AT)mkdir -p $@

.PHONY: haskell-dependencies
haskell-dependencies:
	$(PRINTF) " CABAL\t$@\n"
	$(AT)cabal --builddir="$(abspath $(BUILD)/haskell/)" \
	    new-build all $(if $(AT),2>/dev/null >/dev/null,)

BOOST_VERSION:=1.70.0
BOOST_NAME:=boost_1_70_0
BOOST_SHASUM:=882b48708d211a5f48e60b0124cf5863c1534cd544ecd0664bb534a4b5d506e9
BOOST_ROOT:=$(DOWNLOAD)/$(BOOST_NAME)
BOOST_PREREQ:=$(PREFIX)/include/boost/

BOOST_CXX_FLAGS:=-I$(PREFIX)/include -isystem$(PREFIX)/include
BOOST_LD_FLAGS:=-L$(PREFIX)/lib -L$(PREFIX)/lib64
ifdef CXX_IS_CLANG
BOOST_LD_FLAGS+= -rpath $(PREFIX)/lib -rpath $(PREFIX)/lib64
else
BOOST_LD_FLAGS+= -Wl,-rpath -Wl,$(PREFIX)/lib -Wl,-rpath -Wl,$(PREFIX)/lib64
endif

$(DOWNLOAD)/$(BOOST_NAME).tar.gz: | $(DOWNLOAD)/
	$(PRINTF) " CURL\tboost $(BOOST_VERSION)\n"
	$(AT)curl -s -L https://dl.bintray.com/boostorg/release/$(BOOST_VERSION)/source/$(BOOST_NAME).tar.gz >$@
	$(AT)printf "$(BOOST_SHASUM)  $@\n" | shasum -c /dev/stdin >/dev/null

$(BOOST_ROOT)/: $(DOWNLOAD)/$(BOOST_NAME).tar.gz
	$(PRINTF) " UNTAR\tboost $(BOOST_VERSION)\n"
	$(AT)tar xf $< -C $(DOWNLOAD)/

ifdef CXX_IS_CLANG
    $(BOOST_PREREQ): BOOST_B2_ARGS:=cxxflags="-stdlib=libc++" linkflags="-stdlib=libc++"
    BOOST_COMPILER:=clang
else
    BOOST_COMPILER:=gcc
endif
$(BOOST_PREREQ): | $(BOOST_ROOT)/
	$(PRINTF) " B2\tboost $(BOOST_VERSION)\n"
	$(AT)printf "using $(BOOST_COMPILER) : : $(CXX) ;" >$|/tools/build/src/user-config.jam
	$(AT)cd $| && ./bootstrap.sh toolset=$(BOOST_COMPILER) \
	    --prefix="$(abspath $(PREFIX))" \
	    --with-libraries=filesystem,system,regex \
	    $(if $(AT),2>/dev/null >/dev/null,)
	$(AT)cd $| && ./b2 toolset=$(BOOST_COMPILER) -j24 $(BOOST_B2_ARGS) \
	    $(if $(AT),2>/dev/null >/dev/null,)
	$(AT)cd $| && ./b2 toolset=$(BOOST_COMPILER) install \
	    $(if $(AT),2>/dev/null >/dev/null,)
