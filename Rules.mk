$(DEST)/:
	$(AT)mkdir -p $@

$(DEST)/%.o: $(SRCDIR)/%.cpp | $(DEST)/
	$(PRINTF) " CXX\t$*.cpp\n"
	$(AT)$(CXX) $(CXXFLAGS) -I. $< -c -o $@

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
