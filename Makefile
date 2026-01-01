OPT?=-O0 -g

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

.PHONY: c2dasm
c2dasm: Bin/c2dasm

.PHONY: cpp
cpp: Bin/cpp

.PHONY: all
all: ds2dasm ddasm c2dasm cpp

.PHONY: clean
clean:
	$(RM) $(wildcard Bin/*)

.PHONY: tests
tests: ddasm c2dasm
	bash Tests/run_tests.sh

.DEFAULT_GOAL:= all
