SRCDIR := .
BUILD := build
DEST := build
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

all: main normalise reorder-graph check-degree print-graph graph-details

-include $(patsubst %.cpp, build/%.d, $(wildcard *.cpp))

$(DEST)/main.o $(DEST)/graph-details.o: \
    CXXFLAGS+=-I$(BOOST_PATH)/include -isystem$(BOOST_PATH)/include

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

graph-details: $(DEST)/graph-details.o $(LIBS)/liboptions.a $(BUILD)/libutils.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) -L$(BOOST_PATH)/lib/ -lboost_system -lboost_filesystem $^ -o $@

CLEAN_OBJS+=$(DEST)/*.o

CLEAN_BINS+=main normalise reorder-graph check-degree print-graph

CLEAN_DEPS+=$(DEST)/*.d

-include $(foreach d, $(wildcard */), $(d)Makefile)
