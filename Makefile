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

all: tiger

tiger:
	@echo "  BUILDING " $(ECHO_OUTPUT)
	$(Q)jbuilder build tigercc.exe $(ECHO_OUTPUT)

clean:
	@echo "  CLEANING " $(ECHO_OUTPUT)
	$(Q)rm -rf _build

test: all
	@echo "  TESTING " $(ECHO_OUTPUT)
	$(Q)_build/default/tigercc.exe input.test

.PHONY: test clean
