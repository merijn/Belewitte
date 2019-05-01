SRCDIR := $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))
-include SubDir.mk ../SubDir.mk

ifeq ($(SRCDIR),.)
all: $(LIBS)/lib$(NAME).a
endif

asan: $(LIBS)/lib$(NAME).asan.a
msan: $(LIBS)/lib$(NAME).msan.a
ssan: $(LIBS)/lib$(NAME).ssan.a
tsan: $(LIBS)/lib$(NAME).tsan.a

$(DEST)/Options.o: CXXFLAGS+=$(BOOST_CXX_FLAGS)
$(DEST)/Options.o: $(BOOST_PREREQ)

$(LIBS)/lib$(NAME).a: $($(NAME)_CPP_OBJS)
	$(make-static)

$(LIBS)/lib$(NAME)%.a: $($(NAME)_CPP_OBJS:.o=%.o)
	$(make-static)

.PHONY: clean-$(NAME)-libs

clean-$(NAME)-libs: NAME:=$(NAME)
clean-$(NAME)-libs:
	$(PRINTF) "cleaning libs for: $(NAME)\n"
	$(AT)rm -rf $(LIBS)/lib$(NAME).a

clean-libs: clean-$(NAME)-libs

clean-$(NAME)-lib-%: NAME:=$(NAME)
clean-$(NAME)-lib-%:
	$(PRINTF) "cleaning debug for: $(NAME)\n"
	$(AT)rm -rf $(LIBS)/lib$(NAME).$*.a

clean-asan: clean-$(NAME)-lib-asan
clean-msan: clean-$(NAME)-lib-msan
clean-ssan: clean-$(NAME)-lib-ssan
clean-tsan: clean-$(NAME)-lib-tsan
