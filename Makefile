# See https://github.com/ldc-developers/ldc/issues/3864
export MACOSX_DEPLOYMENT_TARGET:=11
OPT?=-O0

# too lazy to make this work for windows
MKDIR=mkdir -p

Bin: ; $(MKDIR) $@
Deps: ; $(MKDIR) $@

DEPFILES:=$(wildcard Deps/*.deps)
include $(DEPFILES)

Bin/ddasm: source/ddasm.d | Bin Deps
	ldc2 -i source/ddasm.d -I source -betterC -g  -fvisibility=hidden $(OPT) -L-dead_strip -of $@ -makedeps=Deps/ddasm.deps

Bin/dsdasm: source/dsdasm.d | Bin Deps
	ldc2 -i source/dsdasm.d -I source -betterC -g  $(OPT) -L-dead_strip -of $@ -makedeps=Deps/dsdasm.deps

.PHONY: dsdasm
ddasm: Bin/ddasm

.PHONY: ddasm
dsdasm: Bin/dsdasm

.PHONY: all
all: dsdasm ddasm

.PHONY: clean
clean:
	$(RM) $(wildcard Bin/*)

.DEFAULT_GOAL:= all
