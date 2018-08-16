SRCDIR := .
BUILD := .build
DEST := .build
BASE := .
include Common.mk
include Rules.mk

EXES := main normalise reorder-graph check-degree print-graph graph-details

ifeq ($(UNAME),Darwin)
$(call santargets,main): \
    LDFLAGS += -L$(CUDA_PATH)/lib -framework opencl -lcudart
else ifeq ($(UNAME),Linux)
$(call santargets,main): \
    LDFLAGS += -Xlinker --export-dynamic -L$(OPENCL_LIB) -lOpenCL \
               -L$(CUDA_PATH)/lib64 -lcudart
endif

ifeq ($(CXX),clang++)
$(call santargets,main): LDFLAGS += -rpath $(CUDA_PATH)/lib/
else ifeq ($(CXX),g++)
$(call santargets,main): LDFLAGS += -Wl,-rpath -Wl,$(CUDA_PATH)/lib/
endif

all: $(EXES)
asan: $(foreach exe,$(EXES),$(exe).asan)
msan: $(foreach exe,$(EXES),$(exe).msan)
ssan: $(foreach exe,$(EXES),$(exe).ssan)
tsan: $(foreach exe,$(EXES),$(exe).tsan)

-include $(patsubst %.cpp, .build/%.d, $(wildcard *.cpp))

$(call sanobjects,$(DEST)/main) $(call sanobjects,$(DEST)/normalise) \
    $(call sanobjects,$(DEST)/graph-details): \
    CXXFLAGS+=-I$(BOOST_PATH)/include -isystem$(BOOST_PATH)/include

$(call santargets,main): main% : $(DEST)/main%.o $(DEST)/AlgorithmConfig%.o \
      $(DEST)/Backend%.o $(DEST)/CUDA%.o $(DEST)/OpenCL%.o $(DEST)/Timer%.o \
      $(LIBS)/liboptions%.a $(LIBS)/libutils%.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) -L$(BOOST_PATH)/lib/ -lboost_regex -lboost_system -lboost_filesystem $^ -o $@

$(call santargets,normalise): normalise%: $(DEST)/normalise%.o \
      $(LIBS)/libutils%.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) -L$(BOOST_PATH)/lib/ -lboost_system -lboost_filesystem $^ -o $@

$(call santargets,reorder-graph): reorder-graph%: $(DEST)/reorder-graph%.o \
      $(LIBS)/libutils%.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $^ -o $@

$(call santargets,check-degree): check-degree%: $(DEST)/check-degree%.o \
      $(LIBS)/libutils%.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $^ -o $@

$(call santargets,print-graph): print-graph%: $(DEST)/print-graph%.o \
      $(LIBS)/libutils%.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $^ -o $@

$(call santargets,graph-details): graph-details%: $(DEST)/graph-details%.o \
      $(LIBS)/libutils%.a $(LIBS)/liboptions%.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) -L$(BOOST_PATH)/lib/ -lboost_system -lboost_filesystem $^ -o $@

.PHONY: clean-main-objs clean-main-deps clean-main-bins clean-main-%san

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
	$(AT)rm -rf $(EXES)

clean-main-asan:
	$(PRINTF) "cleaning asan for: main\n"
	$(AT)rm -f $(foreach exe,$(EXES),$(exe).asan)

clean-main-msan:
	$(PRINTF) "cleaning msan for: main\n"
	$(AT)rm -f $(foreach exe,$(EXES),$(exe).msan)

clean-main-ssan:
	$(PRINTF) "cleaning ssan for: main\n"
	$(AT)rm -f $(foreach exe,$(EXES),$(exe).ssan)

clean-main-tsan:
	$(PRINTF) "cleaning tsan for: main\n"
	$(AT)rm -f $(foreach exe,$(EXES),$(exe).tsan)

clean-asan: clean-main-asan
clean-msan: clean-main-msan
clean-ssan: clean-main-ssan
clean-tsan: clean-main-tsan

clean-objs: clean-main-objs
clean-deps: clean-main-deps
clean-bins: clean-main-bins

-include $(foreach d, $(wildcard */), $(d)Makefile)
