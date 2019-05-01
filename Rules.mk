$(DEST)/:
	$(AT)mkdir -p $@

$(DEST)/%.o: $(SRCDIR)/%.cpp | $(DEST)/
	$(PRINTF) " CXX\t$*.cpp\n"
	$(AT)$(CXX) $(CXXFLAGS) -O3 -I. $< -c -o $@

define make-static =
$(PRINTF) " AR\t$(subst ../$(BUILD)/,,$@)\n"
$(AT)$(AR) rcs $@ $^
endef

define make-dynamic =
$(PRINTF) " LD\t$@\n"
$(AT)$(LD) $(DYLIBLDFLAGS) -shared -o $@ $^
endef

define make-cuda-lib =
$(PRINTF) " NVLINK\t$@\n"
$(AT)$(NVLINK) $(NVCCXXFLAGS) --device-link $^ --output-file $@
endef

ifneq ($(CXX), clang++)
santargets = $(1)
sanobjects = $(1).o
else
santargets = $(1) $(1).asan $(1).msan $(1).ssan $(1).tsan
sanobjects = $(1).o $(1).asan.o $(1).msan.o $(1).ssan.o $(1).tsan.o

SANITISERS=-fsanitize=undefined -fsanitize=integer -fsanitize=nullability \
    -fno-omit-frame-pointer -O0

%.asan.o: CXXFLAGS:=$(CXXFLAGS) $(SANITISERS) -fsanitize=address \
    -DVERSION=.asan
%.msan.o: CXXFLAGS:=$(CXXFLAGS) $(SANITISERS) -fsanitize=memory -fPIE \
    -DVERSION=.msan
%.ssan.o: CXXFLAGS:=$(CXXFLAGS) $(SANITISERS) -fsanitize=safe-stack \
    -DVERSION=.ssan
%.tsan.o: CXXFLAGS:=$(CXXFLAGS) $(SANITISERS) -fsanitize=thread \
    -DVERSION=.tsan

%.asan %.asan.a: LDFLAGS:=$(LDFLAGS) $(SANITISERS) -fsanitize=address
%.msan %.msan.a: LDFLAGS:=$(LDFLAGS) $(SANITISERS) -fsanitize=memory -fPIE -pie
%.ssan %.ssan.a: LDFLAGS:=$(LDFLAGS) $(SANITISERS) -fsanitize=safe-stack
%.tsan %.tsan.a: LDFLAGS:=$(LDFLAGS) $(SANITISERS) -fsanitize=thread

$(DEST)/%.asan.o: $(SRCDIR)/%.cpp | $(DEST)/
	$(PRINTF) " CXX\t$*.cpp\n"
	$(AT)$(CXX) $(CXXFLAGS) -I. $< -c -o $@

$(DEST)/%.msan.o: $(SRCDIR)/%.cpp | $(DEST)/
	$(PRINTF) " CXX\t$*.cpp\n"
	$(AT)$(CXX) $(CXXFLAGS) -I. $< -c -o $@

$(DEST)/%.ssan.o: $(SRCDIR)/%.cpp | $(DEST)/
	$(PRINTF) " CXX\t$*.cpp\n"
	$(AT)$(CXX) $(CXXFLAGS) -I. $< -c -o $@

$(DEST)/%.tsan.o: $(SRCDIR)/%.cpp | $(DEST)/
	$(PRINTF) " CXX\t$*.cpp\n"
	$(AT)$(CXX) $(CXXFLAGS) -I. $< -c -o $@
endif

$(DEST)/%.obj: %.cu | $(DEST)/
	$(PRINTF) " NVCC\t$*.cu\n"
	$(AT)$(NVCC) $(NVCCXXFLAGS) -M -I. $< -o $(@:.obj=.d)
	$(AT)$(SED) -i.bak "s#$(notdir $*).o#$(@) $*.ptx#" $(@:.obj=.d)
	$(AT)rm -f $(@:.obj=.d).bak
	$(AT)$(NVCC) $(NVCCXXFLAGS) $(NVCCARCHFLAGS) -lineinfo -I. --device-c $< -o $@

$(DEST)/%.debug.obj: %.cu | $(DEST)/
	$(PRINTF) " NVCC\t$*.cu\n"
	$(AT)$(NVCC) $(NVCCXXFLAGS) -M -I. $< -o $(@:.obj=.d)
	$(AT)$(SED) -i.bak "s#$(notdir $*).o#$(@) $*.ptx#" $(@:.obj=.d)
	$(AT)rm -f $(@:.obj=.d).bak
	$(AT)$(NVCC) $(NVCCXXFLAGS) $(NVCCARCHFLAGS) --device-debug -I. --device-c $< -o $@

%.ptx: %.cu
	$(PRINTF) " PTX\t$*.cu\n"
	$(AT)$(NVCC) $(NVCCXXFLAGS) -M -I. $< -o $(BUILD)$(@:.ptx=.d)
	$(AT)$(SED) -i.bak "s#$(notdir $*).o#$(@) $*.ptx#" $(BUILD)$(@:.ptx=.d)
	$(AT)rm -f $(BUILD)$(@:.ptx=.d).bak
	$(AT)$(NVCC) $(NVCCXXFLAGS) -arch $(PTXARCH) -I. -src-in-ptx --ptx $< -o $@
	$(AT)$(SED) -i.bak '1,/^\t\/\/ .globl/ d; /^\t.file\t/,$$ d' $@
	$(AT) rm -f $@.bak
