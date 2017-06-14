$(NAME)_CUDA_SRCS:=$(notdir $(wildcard $(SRCDIR)/*.cu))
$(NAME)_CUDA_OBJS:=$(patsubst %.cu, $(DEST)/%.obj, $($(NAME)_CUDA_SRCS))
$(NAME)_CUDA_PTXS:=$(patsubst %.cu, $(SRCDIR)/%.ptx, $($(NAME)_CUDA_SRCS))

-include $(patsubst %.cu, $(DEST)/%.d, $($(NAME)_CUDA_SRCS))

$($(NAME)_CUDA_OBJS): | $(DEST)/

all: $(DEST)/lib$(NAME).so
ptx: $($(NAME)_CUDA_PTXS)

ifeq ($(UNAME),Linux)
$($(NAME)_CPP_OBJS): CXXFLAGS+=-fPIC
$($(NAME)_CUDA_OBJS) $(DEST)/device.o: NVCCHOSTCXXFLAGS+=-fPIC
endif

$(DEST)/lib$(NAME).so: $($(NAME)_CPP_OBJS) $($(NAME)_CUDA_OBJS) $(DEST)/device.o
	$(PRINTF) " LD\t$@\n"
	$(AT)$(LD) $(DYLIBLDFLAGS) -shared -o $@ $^

$(DEST)/device.o: NVCCHOSTCXXFLAGS+=-Wno-deprecated

$(DEST)/device.o: $($(NAME)_CUDA_OBJS)
	$(PRINTF) " NVLINK\t$@\n"
	$(AT)$(NVLINK) $(NVCCXXFLAGS) --device-link $^ --output-file $@

CLEAN_PTXS+=$($(NAME)_CUDA_PTXS)
CLEAN_OBJS+=$($(NAME)_CUDA_OBJS) $(DEST)/device.o
CLEAN_LIBS+=$(DEST)/lib$(NAME).so
