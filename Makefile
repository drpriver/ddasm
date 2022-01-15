# See https://github.com/ldc-developers/ldc/issues/3864
export MACOSX_DEPLOYMENT_TARGET:=11
.PHONY: ddasm
OPT?=-O0
# too lazy to make this work for windows
MKDIR=mkdir -p
Bin: ; $(MKDIR) $@
ddasm: | Bin
	ldc2 -i source/ddasm.d -I source -betterC -g  -fvisibility=hidden $(OPT) -L-dead_strip -of Bin/$@

dsdasm: | Bin
	ldc2 -i source/dsdasm.d -I source -betterC -g  $(OPT) -of Bin/$@
.PHONY: dsdasm

.PHONY: all
all: dsdasm ddasm

.DEFAULT_GOAL:= all
