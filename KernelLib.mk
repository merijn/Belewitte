$(NAME)_CUDA_SRCS:=$(notdir $(wildcard $(SRCDIR)/*.cu))
$(NAME)_CUDA_OBJS:=$(patsubst %.cu, $(DEST)/%.obj, $($(NAME)_CUDA_SRCS))
$(NAME)_CUDA_DEBUG_OBJS:=$(patsubst %.obj, %.debug.obj, $($(NAME)_CUDA_OBJS))
$(NAME)_CUDA_PTXS:=$(patsubst %.cu, $(SRCDIR)/%.ptx, $($(NAME)_CUDA_SRCS))

-include $(patsubst %.cu, $(DEST)/%.d, $($(NAME)_CUDA_SRCS))

$($(NAME)_CUDA_OBJS): | $(DEST)/

all: $(BUILD)/kernels/lib$(NAME)kernel.so \
    $(BUILD)/kernels/lib$(NAME)kerneldebug.so
ptx: $($(NAME)_CUDA_PTXS)

ifeq ($(UNAME),Linux)
$($(NAME)_CPP_OBJS): CXXFLAGS+=-fPIC
$($(NAME)_CUDA_OBJS) $(DEST)/device.o: NVCCHOSTCXXFLAGS+=-fPIC
$($(NAME)_CUDA_DEBUG_OBJS) $(DEST)/device-debug.o: NVCCHOSTCXXFLAGS+=-fPIC
endif

$(BUILD)/kernels/lib$(NAME)kernel.so: $($(NAME)_CPP_OBJS) $($(NAME)_CUDA_OBJS) \
    $(DEST)/device.o | $(BUILD)/kernels/
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(DYLIBLDFLAGS) -shared -o $@ $^

$(BUILD)/kernels/lib$(NAME)kerneldebug.so: $($(NAME)_CPP_OBJS) \
    $($(NAME)_CUDA_DEBUG_OBJS) $(DEST)/device-debug.o | $(BUILD)/kernels/
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(DYLIBLDFLAGS) -shared -o $@ $^

$(DEST)/device.o: NVCCHOSTCXXFLAGS+=-Wno-deprecated

$(DEST)/device.o: $($(NAME)_CUDA_OBJS)
	$(PRINTF) " NVLINK\t$@\n"
	$(AT)$(NVLINK) $(NVCCXXFLAGS) --device-link $^ --output-file $@

$(DEST)/device-debug.o: NVCCHOSTCXXFLAGS+=-Wno-deprecated

$(DEST)/device-debug.o: $($(NAME)_CUDA_DEBUG_OBJS)
	$(PRINTF) " NVLINK\t$@\n"
	$(AT)$(NVLINK) $(NVCCXXFLAGS) --device-link $^ --output-file $@

.PHONY: clean-ptx clean-objs clean-libs clean-$(NAME)-ptx clean-$(NAME)-objs \
    clean-$(NAME)-deps

clean-$(NAME)-ptx: NAME:=$(NAME)
clean-$(NAME)-ptx:
	$(PRINTF) "cleaning PTX files for: $(NAME)\n"
	$(AT)rm -rf $($(NAME)_CUDA_PTXS)

clean-$(NAME)-cuda-objs: NAME:=$(NAME)
clean-$(NAME)-cuda-objs: DEST:=$(DEST)
clean-$(NAME)-cuda-objs:
	$(PRINTF) "cleaning CUDA objects for: $(NAME)\n"
	$(AT)rm -rf $($(NAME)_CUDA_OBJS) $(DEST)/device.o $($(NAME)_CUDA_DEBUG_OBJS) $(DEST)/device-debug.o

clean-$(NAME)-cuda-libs: NAME:= $(NAME)
clean-$(NAME)-cuda-libs: DEST:= $(DEST)
clean-$(NAME)-cuda-libs:
	$(PRINTF) "cleaning CUDA dependencies for: $(NAME)\n"
	$(AT)rm -rf $(BUILD)/kernels/lib$(NAME).so $(BUILD)/kernels/lib$(NAME)-debug.so

clean-ptx: clean-$(NAME)-ptx
clean-objs: clean-$(NAME)-cuda-objs
clean-libs: clean-$(NAME)-cuda-libs
