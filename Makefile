# Verbosity levels
# 0: only warnings and errors are displayed
# 1: command summary only (default)
# 2: executed commands are shown

ifdef V
    VERBOSE := $(V)
else
    VERBOSE := 1
endif

ifeq "$(VERBOSE)" "0"
   Q := @
   ECHO_OUTPUT :=  > /dev/null
else ifeq "$(VERBOSE)" "1"
    Q := @
    ECHO_OUTPUT := 
else
    Q :=
    ECHO_OUTPUT :=
endif

OCB_FLAGS := -use-ocamlfind -I src
OCB := ocamlbuild $(OCB_FLAGS)

byte: tiger.byte

all: native byte

native: tiger.native

tiger.native:
	@echo "  ML $< (native)" $(ECHO_OUTPUT)
	$(Q)$(OCB) tigercc.native

tiger.byte:
	@echo "  ML $< (byte)" $(ECHO_OUTPUT)
	$(Q)$(OCB) tigercc.byte

clean:
	@echo "  CLEAN " $(ECHO_OUTPUT)
	$(Q)$(OCB) -clean

debug:
	@echo " ML $< (byte, debug)" $(ECHO_OUTPUT)
	$(OCB) -tag debug tigercc.byte

test:
	@echo "  TESTING " $(ECHO_OUTPUT)
	$(OCB) -package oUnit -tag debug -I tests test.byte
	$(Q)./test.byte

.PHONY: test clean
