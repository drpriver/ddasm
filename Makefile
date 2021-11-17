.PHONY: ddasm
ddasm:
	ldc2 -i source/ddasm.d -I source -betterC

highlevel:
	ldc2 -i source/highlevel.d -I source -g -betterC
.PHONY: highlevel
.DEFAULT_GOAL:= highlevel
