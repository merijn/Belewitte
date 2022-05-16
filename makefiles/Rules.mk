.SECONDEXPANSION:
$(BUILD)/%.o: $(BASE)/%.cpp | $$(dir $$@)
	$(PRINTF) " CXX\t$*.cpp\n"
	$(AT)$(CXX) $(CXXFLAGS) -O3 -I. $< -c -o $@

define make-static
$(PRINTF) " AR\t$(subst ../$(BUILD)/,,$@)\n"
$(AT)$(AR) rcs $@ $^
endef

define make-dynamic
$(PRINTF) " LD\t$@\n"
$(AT)$(LD) $(DYLIBLDFLAGS) -shared -o $@ $^
endef

define make-cuda-lib
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

.SECONDEXPANSION:
$(DEST)/%.asan.o: $(BASE)/%.cpp | $$(dir $$@)
	$(PRINTF) " CXX\t$*.cpp\n"
	$(AT)$(CXX) $(CXXFLAGS) -I. $< -c -o $@

.SECONDEXPANSION:
$(DEST)/%.msan.o: $(BASE)/%.cpp | $$(dir $$@)
	$(PRINTF) " CXX\t$*.cpp\n"
	$(AT)$(CXX) $(CXXFLAGS) -I. $< -c -o $@

.SECONDEXPANSION:
$(DEST)/%.ssan.o: $(BASE)/%.cpp | $$(dir $$@)
	$(PRINTF) " CXX\t$*.cpp\n"
	$(AT)$(CXX) $(CXXFLAGS) -I. $< -c -o $@

.SECONDEXPANSION:
$(DEST)/%.tsan.o: $(BASE)/%.cpp | $$(dir $$@)
	$(PRINTF) " CXX\t$*.cpp\n"
	$(AT)$(CXX) $(CXXFLAGS) -I. $< -c -o $@
endif

$(DEST)/%.obj: SRCDIR:=$(SRCDIR)
.SECONDEXPANSION:
$(DEST)/%.obj: $(BASE)/%.cu | $$(dir $$@)
	$(PRINTF) " NVCC\t$*.cu\n"
	$(AT)$(NVCC) $(NVCCXXFLAGS) -M -I. $< -o $(@:.obj=.d)
	$(AT)$(SED) -i.bak "s#$(notdir $*).o#$(@) $(SRCDIR)/$*.ptx#" $(@:.obj=.d)
	$(AT)rm -f $(@:.obj=.d).bak
	$(AT)$(NVCC) $(NVCCXXFLAGS) $(NVCCARCHFLAGS) -lineinfo -I. --device-c $< -o $@

$(DEST)/%.debug.obj: SRCDIR:=$(SRCDIR)
.SECONDEXPANSION:
$(DEST)/%.debug.obj: $(BASE)/%.cu | $$(dir $$@)
	$(PRINTF) " NVCC\t$*.cu\n"
	$(AT)$(NVCC) $(NVCCXXFLAGS) -M -I. $< -o $(@:.obj=.d)
	$(AT)$(SED) -i.bak "s#$(notdir $*).o#$(@) $(SRCDIR)/$*.ptx#" $(@:.obj=.d)
	$(AT)rm -f $(@:.obj=.d).bak
	$(AT)$(NVCC) $(NVCCXXFLAGS) $(NVCCARCHFLAGS) --device-debug -I. --device-c $< -o $@

$(SRCDIR)/%.ptx: SRCDIR:=$(SRCDIR)
$(SRCDIR)/%.ptx: $(SRCDIR)/%.cu
	$(PRINTF) " PTX\t$*.cu\n"
	$(AT)$(NVCC) $(NVCCXXFLAGS) -M -I. $< -o $(BUILD)/$(@:.ptx=.d)
	$(AT)$(SED) -i.bak "s#$(notdir $*).o#$(@) $(SRCDIR)/$*.ptx#" $(BUILD)/$(@:.ptx=.d)
	$(AT)rm -f $(BUILD)/$(@:.ptx=.d).bak
	$(AT)$(NVCC) $(NVCCXXFLAGS) -arch $(PTXARCH) -I. -lineinfo -src-in-ptx --ptx $< -o $@
