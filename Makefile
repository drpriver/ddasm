OPT?=-O3 -g

Bin: ; mkdir $@
Deps: ; mkdir $@

DEPFILES:=$(wildcard Deps/*.deps)
include $(DEPFILES)

Bin/%: %.d | Bin Deps
	ldc2 -i $< -betterC -fvisibility=hidden $(OPT) -of $@ -makedeps=Deps/$*.deps

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
