SRCDIR := .
BUILD := .build
DEST := .build
BASE := .
include Common.mk
include Rules.mk

ifeq ($(UNAME),Darwin)
main: LDFLAGS += -L$(CUDA_PATH)/lib -framework opencl
else ifeq ($(UNAME),Linux)
main: LDFLAGS += -Xlinker --export-dynamic -L$(OPENCL_LIB) -lOpenCL -L$(CUDA_PATH)/lib64 -lcudart
endif

ifeq ($(CXX),clang++)
main: LDFLAGS += -rpath $(CUDA_PATH)/lib/
else ifeq ($(CXX),g++)
main: LDFLAGS += -Wl,-rpath -Wl,$(CUDA_PATH)/lib/
endif

all: main normalise reorder-graph check-degree print-graph

-include $(patsubst %.cpp, build/%.d, $(wildcard *.cpp))

$(DEST)/main.o: CXXFLAGS+=-I$(BOOST_PATH)/include -isystem$(BOOST_PATH)/include

main: $(DEST)/main.o $(DEST)/AlgorithmConfig.o $(DEST)/Backend.o \
      $(DEST)/CUDA.o $(DEST)/OpenCL.o $(DEST)/Timer.o $(LIBS)/liboptions.a \
      $(LIBS)/libutils.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) -L$(BOOST_PATH)/lib/ -lboost_regex -lboost_system -lboost_filesystem $^ -o $@

normalise: $(DEST)/normalise.o $(LIBS)/libutils.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) -L$(BOOST_PATH)/lib/ -lboost_system -lboost_filesystem $^ -o $@

reorder-graph: $(DEST)/reorder-graph.o $(LIBS)/libutils.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $^ -o $@

check-degree: $(DEST)/check-degree.o $(LIBS)/libutils.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $^ -o $@

print-graph: $(DEST)/print-graph.o $(LIBS)/libutils.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $^ -o $@

.PHONY: clean-main-objs clean-main-deps clean-main-bins

clean-main-objs: DEST:=$(DEST)
clean-main-objs:
	$(PRINTF) "cleaning objects for: main\n"
	$(AT)rm -rf $(DEST)/*.o

clean-main-deps: DEST:=$(DEST)
clean-main-deps:
	$(PRINTF) "cleaning dependencies for: main\n"
	$(AT)rm -rf $(DEST)/*.d

clean-main-bins:
	$(PRINTF) "cleaning executables for: main\n"
	$(AT)rm -rf main normalise reorder-graph check-degree print-graph

clean-objs: clean-main-objs
clean-deps: clean-main-deps
clean-bins: clean-main-bins

-include $(foreach d, $(wildcard */), $(d)Makefile)
