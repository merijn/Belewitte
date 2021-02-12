$(NAME)_CUDA_SRCS:=$(notdir $(wildcard $(SRCDIR)/*.cu))
$(NAME)_CUDA_OBJS:=$(patsubst %.cu, $(DEST)/%.obj, $($(NAME)_CUDA_SRCS))
$(NAME)_CUDA_DEBUG_OBJS:=$(patsubst %.obj, %.debug.obj, $($(NAME)_CUDA_OBJS))
$(NAME)_CUDA_PTXS:=$(patsubst %.cu, $(SRCDIR)/%.ptx, $($(NAME)_CUDA_SRCS))

-include $(patsubst %.cu, $(DEST)/%.d, $($(NAME)_CUDA_SRCS))

$($(NAME)_CUDA_OBJS): | $(DEST)/
$($(NAME)_CUDA_DEBUG_OBJS): | $(DEST)/

ifdef NVCC
all: $(BUILD)/kernels/lib$(NAME)kernel.so \
    $(BUILD)/kernels/lib$(NAME)kerneldebug.so

asan: $(BUILD)/kernels/lib$(NAME)kernel.asan.so \
    $(BUILD)/kernels/lib$(NAME)kerneldebug.asan.so

msan: $(BUILD)/kernels/lib$(NAME)kernel.msan.so \
    $(BUILD)/kernels/lib$(NAME)kerneldebug.msan.so

ssan: $(BUILD)/kernels/lib$(NAME)kernel.ssan.so \
    $(BUILD)/kernels/lib$(NAME)kerneldebug.ssan.so

tsan: $(BUILD)/kernels/lib$(NAME)kernel.tsan.so \
    $(BUILD)/kernels/lib$(NAME)kerneldebug.tsan.so

ptx: $($(NAME)_CUDA_PTXS)
else
all: missing-cuda
asan: missing-cuda
msan: missing-cuda
ssan: missing-cuda
tsan: missing-cuda
ptx: missing-cuda
endif

ifeq ($(UNAME),Linux)
$(foreach obj,$($(NAME)_CPP_OBJS), $(call sanobjects,$(obj:.o=))): CXXFLAGS+=-fPIC
$($(NAME)_CUDA_OBJS) $(DEST)/device.o: NVCCHOSTCXXFLAGS+=-fPIC
$($(NAME)_CUDA_DEBUG_OBJS) $(DEST)/device-debug.o: NVCCHOSTCXXFLAGS+=-fPIC
endif

$($(NAME)_CPP_OBJS): COMMIT:=$(shell $(BASE)/report-kernel-commit.sh $(NAME))
$($(NAME)_CPP_OBJS): CXXFLAGS+=-DKERNEL_COMMIT=\"$(COMMIT)\"

$(BUILD)/kernels/lib$(NAME)kernel.so: $($(NAME)_CPP_OBJS) $($(NAME)_CUDA_OBJS) \
    $(DEST)/device.o | $(BUILD)/kernels/
	$(make-dynamic)

$(BUILD)/kernels/lib$(NAME)kernel%.so: $($(NAME)_CPP_OBJS:.o=%.o) \
    $($(NAME)_CUDA_OBJS) $(DEST)/device.o | $(BUILD)/kernels/
	$(make-dynamic)

$(BUILD)/kernels/lib$(NAME)kerneldebug.so: $($(NAME)_CPP_OBJS) \
    $($(NAME)_CUDA_DEBUG_OBJS) $(DEST)/device-debug.o | $(BUILD)/kernels/
	$(make-dynamic)

$(BUILD)/kernels/lib$(NAME)kerneldebug%.so: $($(NAME)_CPP_OBJS:.o=%.o) \
    $($(NAME)_CUDA_DEBUG_OBJS) $(DEST)/device-debug.o | $(BUILD)/kernels/
	$(make-dynamic)

$(DEST)/device.o: NVCCHOSTCXXFLAGS+=-Wno-deprecated

$(DEST)/device.o: $($(NAME)_CUDA_OBJS)
	$(make-cuda-lib)

$(DEST)/device-debug.o: NVCCHOSTCXXFLAGS+=-Wno-deprecated

$(DEST)/device-debug.o: $($(NAME)_CUDA_DEBUG_OBJS)
	$(make-cuda-lib)

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
