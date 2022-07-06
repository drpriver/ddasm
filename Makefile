OPT?=-O1 -g

# too lazy to make this work for windows
MKDIR=mkdir -p

Bin: ; $(MKDIR) $@
Deps: ; $(MKDIR) $@

DEPFILES:=$(wildcard Deps/*.deps)
include $(DEPFILES)

# Apparently a collosal amount of dead code is generated.
# Telling the linker to strip it out saves a lot!
LDSTRIP=-L-dead_strip

Bin/%: %.d | Bin Deps
	ldc2 -i $< -betterC -fvisibility=hidden $(OPT) $(LDSTRIP) -of $@ -makedeps=Deps/$*.deps --allinst

.PHONY: ddasm
ddasm: Bin/ddasm

.PHONY: ds2dasm
ds2dasm: Bin/ds2dasm

.PHONY: all
all: ds2dasm ddasm

.PHONY: clean
clean:
	$(RM) $(wildcard Bin/*)

.DEFAULT_GOAL:= all
