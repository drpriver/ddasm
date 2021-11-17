.PHONY: ddasm
ddasm:
	ldc2 -i source/ddasm.d -I source -betterC -g

dsdasm:
	ldc2 -i source/dsdasm.d -I source -betterC  -g
.PHONY: dsdasm
.DEFAULT_GOAL:= dsdasm
