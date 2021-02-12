ifeq ($(SRCDIR),.)
    NAME := $(notdir $(CURDIR))
    DEST := ../.build/$(NAME)
    BUILD := ../.build
    BASE := ..
    include $(BASE)/makefiles/Common.mk
    CXXFLAGS += -I../
    NVCCXXFLAGS += -I../
else
    DIRNAME := $(patsubst ../%,%,$(SRCDIR))
    NAME := $(DIRNAME)
    DEST := $(findstring ../,$(SRCDIR)).build/$(DIRNAME)
    BUILD := $(findstring ../,$(SRCDIR)).build
endif

include $(BASE)/makefiles/Rules.mk

$(NAME)_CPP_HEADERS:=$(notdir $(wildcard $(SRCDIR)/*.hpp))
$(NAME)_CPP_SRCS:=$(notdir $(wildcard $(SRCDIR)/*.cpp))
$(NAME)_CPP_OBJS:=$(patsubst %.cpp, $(DEST)/%.o, $($(NAME)_CPP_SRCS))

-include $(patsubst %.cpp, $(DEST)/%.d, $($(NAME)_CPP_SRCS))

$(foreach obj,$($(NAME)_CPP_OBJS), $(call sanobjects,$(obj))): | $(DEST)/

.PHONY: clean-$(NAME)-objs clean-$(NAME)-deps clean-$(NAME)-%san

clean-$(NAME)-objs: NAME:=$(NAME)
clean-$(NAME)-objs:
	$(PRINTF) "cleaning objects for: $(NAME)\n"
	$(AT)rm -rf $($(NAME)_CPP_OBJS)

clean-$(NAME)-deps: NAME:=$(NAME)
clean-$(NAME)-deps: DEST:=$(DEST)
clean-$(NAME)-deps:
	$(PRINTF) "cleaning dependencies for: $(NAME)\n"
	$(AT)rm -rf $(DEST)/*.d

clean-$(NAME)-asan: NAME:=$(NAME)
clean-$(NAME)-asan: DEST:=$(DEST)
clean-$(NAME)-asan:
	$(PRINTF) "cleaning asan for: $(NAME)\n"
	$(AT)rm -rf $($(NAME)_CPP_OBJS:.o=.asan.o)

clean-$(NAME)-msan: NAME:=$(NAME)
clean-$(NAME)-msan: DEST:=$(DEST)
clean-$(NAME)-msan:
	$(PRINTF) "cleaning msan for: $(NAME)\n"
	$(AT)rm -rf $($(NAME)_CPP_OBJS:.o=.msan.o)

clean-$(NAME)-ssan: NAME:=$(NAME)
clean-$(NAME)-ssan: DEST:=$(DEST)
clean-$(NAME)-ssan:
	$(PRINTF) "cleaning ssan for: $(NAME)\n"
	$(AT)rm -rf $($(NAME)_CPP_OBJS:.o=.asan.o)

clean-$(NAME)-tsan: NAME:=$(NAME)
clean-$(NAME)-tsan: DEST:=$(DEST)
clean-$(NAME)-tsan:
	$(PRINTF) "cleaning tsan for: $(NAME)\n"
	$(AT)rm -rf $($(NAME)_CPP_OBJS:.o=.tsan.o)

clean-objs: clean-$(NAME)-objs
clean-deps: clean-$(NAME)-deps
clean-asan: clean-$(NAME)-asan
clean-msan: clean-$(NAME)-msan
clean-ssan: clean-$(NAME)-ssan
clean-tsan: clean-$(NAME)-tsan
