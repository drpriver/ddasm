OPT?=-O0 -g

Bin: ; mkdir $@
Deps: ; mkdir $@

DEPFILES:=$(wildcard Deps/*.deps)
include $(DEPFILES)

Bin/%: %.d | Bin Deps
	ldc2 -i $< -betterC -fvisibility=hidden $(OPT) -of $@ -makedeps=Deps/$*.deps
Bin/%-debug: %.d | Bin Deps
	ldc2 -i $< -betterC -fvisibility=hidden -O0 -g -of $@ -makedeps=Deps/$*-debug.deps
Bin/%-release: %.d | Bin Deps
	ldc2 -i $< -betterC -fvisibility=hidden -O3 -g -of $@ -makedeps=Deps/$*-release.deps

.PHONY: ddasm
ddasm: Bin/ddasm Bin/ddasm-debug Bin/ddasm-release

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

.PHONY: static-tests
static-tests: ddasm c2dasm cpp
	Tests/run_tests.py

# generated dynamically
.PHONY: abi-tests
abi-tests: ddasm c2dasm
	Tests/test_abi.py

.PHONY: tests
tests: static-tests abi-tests


.DEFAULT_GOAL:= all
