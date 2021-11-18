.PHONY: ddasm
ddasm:
	ldc2 -i source/ddasm.d -I source -betterC -g -O3 -L-dead_strip -fvisibility=hidden

dsdasm:
	ldc2 -i source/dsdasm.d -I source -betterC -g -fsanitize=address
.PHONY: dsdasm
.DEFAULT_GOAL:= dsdasm

.PHONY: all
all: dsdasm ddasm
