DEST := build
include Common.mk

ifeq ($(UNAME),Linux)
build/Connectivity.o: CPATH:=$(ICC_CPATH):$(CPATH)
build/Connectivity.o: CC=icc

evolve: LDFLAGS+=-ltbb
evolve: LD=icc
endif

all: main normalise gen-graph reorder-graph check-degree evolve print-graph

-include $(patsubst %.cpp, build/%.d, $(wildcard *.cpp))
-include $(foreach d, $(wildcard */), $(d)Makefile)

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

CLEAN_OBJS+=build/*.o

CLEAN_BINS+=main normalise gen-graph reorder-graph check-degree evolve \
            print-graph
