BOOST_VERSION:=1.70.0
BOOST_NAME:=boost_1_70_0
BOOST_SHASUM:=882b48708d211a5f48e60b0124cf5863c1534cd544ecd0664bb534a4b5d506e9
BOOST_ROOT:=$(DOWNLOAD)/$(BOOST_NAME)
BOOST_PREREQ:=$(PREFIX)/include/boost/

BOOST_CXX_FLAGS:=-I$(PREFIX)/include -isystem$(PREFIX)/include
BOOST_LD_FLAGS:=$(if $(wildcard $(PREFIX)/lib),-L$(PREFIX)/lib) \
    $(if $(wildcard $(PREFIX)/lib64),-L$(PREFIX)/lib64)

ifdef CXX_IS_CLANG
BOOST_LD_FLAGS+= -rpath $(abspath $(PREFIX))/lib -rpath $(abspath $(PREFIX))/lib64
else
BOOST_LD_FLAGS+= -Wl,-rpath -Wl,$(abspath $(PREFIX))/lib -Wl,-rpath \
                 -Wl,$(abspath $(PREFIX))/lib64
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
