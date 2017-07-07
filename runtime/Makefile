# PIC stands for position independent code, explained here
# https://stackoverflow.com/questions/5311515/gcc-fpic-option

olifant.so: olifant.c
	clang -Wall -Wpedantic -fPIC -shared olifant.c -o libolifant.so
