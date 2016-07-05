DEST := build
include Common.mk

ifeq ($(UNAME),Darwin)
main: LDFLAGS += -L$(CUDA_PATH)/lib -framework opencl
else ifeq ($(UNAME),Linux)
build/Connectivity.o: CPATH:=$(ICC_CPATH):$(CPATH)
build/Connectivity.o: CXX=icc

evolve: LDFLAGS+=-ltbb
evolve: LD=icc

main: LDFLAGS += -Xlinker --export-dynamic -L$(CUDA_PATH)/lib64 -lOpenCL
endif

ifeq ($(CXX),clang++)
main: LDFLAGS += -rpath $(CUDA_PATH)/lib/
else ifeq ($(CXX),g++)
main: LDFLAGS += -Wl,-rpath -Wl,$(CUDA_PATH)/lib/
endif

all: main normalise gen-graph reorder-graph check-degree evolve print-graph

-include $(patsubst %.cpp, build/%.d, $(wildcard *.cpp))
-include $(foreach d, $(wildcard */), $(d)Makefile)

build/main.o build/evolve.o: CXXFLAGS+=-I$(BOOST_PATH)/include -isystem$(BOOST_PATH)/include

main: build/main.o build/CUDA.o build/OpenCL.o build/Timer.o build/Util.o \
      build/Options.o
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) -L$(BOOST_PATH)/lib/ -lboost_regex -lboost_system -lboost_filesystem -lcudart $^ -o $@

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

evolve: build/evolve.o build/Util.o build/Connectivity.o
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $^ -o $@

print-graph: build/print-graph.o build/Util.o
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $^ -o $@

CLEAN_OBJS+=build/*.o

CLEAN_BINS+=main normalise gen-graph reorder-graph check-degree evolve \
            print-graph

CLEAN_DEPS+=build/*.d
