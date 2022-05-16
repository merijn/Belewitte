SRCDIR := .
BUILD := .build
DEST := .build
BASE := .
include makefiles/Common.mk
include makefiles/Rules.mk

EXES := kernel-runner normalise-graph reorder-graph check-degree print-graph \
        graph-details

ifeq ($(UNAME),Darwin)
$(call santargets,kernel-runner): \
    LDFLAGS += -L$(CUDA_PATH)/lib -framework opencl -lcudart
else ifeq ($(UNAME),Linux)
$(call santargets,kernel-runner): \
    LDFLAGS += -Xlinker --export-dynamic -L$(OPENCL_LIB) -lOpenCL \
               -L$(CUDA_PATH)/lib64 -lcudart
endif

ifdef CXX_IS_CLANG
$(call santargets,kernel-runner): LDFLAGS += -rpath $(CUDA_PATH)/lib/
else ifeq ($(CXX),g++)
$(call santargets,kernel-runner): LDFLAGS += -Wl,-rpath -Wl,$(CUDA_PATH)/lib/
endif

all: $(EXES) haskell_all
asan: $(foreach exe,$(EXES),$(exe).asan)
msan: $(foreach exe,$(EXES),$(exe).msan)
ssan: $(foreach exe,$(EXES),$(exe).ssan)
tsan: $(foreach exe,$(EXES),$(exe).tsan)

-include $(patsubst %.cpp, .build/%.d, $(wildcard *.cpp))

$(call sanobjects,$(DEST)/kernel-runner) \
    $(call sanobjects,$(DEST)/normalise-graph) \
    $(call sanobjects,$(DEST)/graph-details): $(BOOST_PREREQ)

ifndef NVCC
.PHONY: $(call santargets,kernel-runner)
$(call santargets,kernel-runner):
	$(PRINTF) "nvcc not found, skipping kernel-runner\n"
else ifndef OPENCL_LIB
.PHONY: $(call santargets,kernel-runner)
$(call santargets,kernel-runner):
	$(PRINTF) "OpenCL not found, skipping kernel-runner\n"
else
$(call santargets,kernel-runner): kernel-runner% : $(DEST)/kernel-runner%.o \
    $(DEST)/Algorithm%.o $(DEST)/Backend%.o $(DEST)/CUDA%.o \
    $(DEST)/ImplementationBase%.o $(DEST)/OpenCL%.o $(DEST)/Timer%.o \
    $(LIBS)/liboptions%.a $(LIBS)/libutils%.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $(BOOST_LD_FLAGS) -lboost_regex -lboost_system -lboost_filesystem $^ -o $@
endif

$(call santargets,normalise-graph): normalise-graph%: $(DEST)/normalise-graph%.o \
      $(LIBS)/libutils%.a
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(LDFLAGS) $(BOOST_LD_FLAGS) -lboost_system -lboost_filesystem $^ -o $@

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
	$(AT)$(LD) $(LDFLAGS) $(BOOST_LD_FLAGS) -lboost_system -lboost_filesystem $^ -o $@

.PHONY: clean-kernel-runner-objs clean-kernel-runner-deps \
        clean-kernel-runner-bins clean-kernel-runner-%san

clean-kernel-runner-objs: DEST:=$(DEST)
clean-kernel-runner-objs:
	$(PRINTF) "cleaning objects for: kernel-runner\n"
	$(AT)rm -rf $(DEST)/*.o

clean-kernel-runner-deps: DEST:=$(DEST)
clean-kernel-runner-deps:
	$(PRINTF) "cleaning dependencies for: kernel-runner\n"
	$(AT)rm -rf $(DEST)/*.d

clean-kernel-runner-bins:
	$(PRINTF) "cleaning executables for: kernel-runner\n"
	$(AT)rm -rf $(EXES)

clean-kernel-runner-asan:
	$(PRINTF) "cleaning asan for: kernel-runner\n"
	$(AT)rm -f $(foreach exe,$(EXES),$(exe).asan)

clean-kernel-runner-msan:
	$(PRINTF) "cleaning msan for: kernel-runner\n"
	$(AT)rm -f $(foreach exe,$(EXES),$(exe).msan)

clean-kernel-runner-ssan:
	$(PRINTF) "cleaning ssan for: kernel-runner\n"
	$(AT)rm -f $(foreach exe,$(EXES),$(exe).ssan)

clean-kernel-runner-tsan:
	$(PRINTF) "cleaning tsan for: kernel-runner\n"
	$(AT)rm -f $(foreach exe,$(EXES),$(exe).tsan)

clean-asan: clean-kernel-runner-asan
clean-msan: clean-kernel-runner-msan
clean-ssan: clean-kernel-runner-ssan
clean-tsan: clean-kernel-runner-tsan

clean-objs: clean-kernel-runner-objs
clean-deps: clean-kernel-runner-deps
clean-bins: clean-kernel-runner-bins

-include $(foreach d, $(wildcard */), $(d)Makefile)
