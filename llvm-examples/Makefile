# http://llvm.org/docs/GettingStarted.html#an-example-using-the-llvm-tool-chain

hello.bc: hello.c
	clang -O0 -emit-llvm hello.c -c -o hello.bc

hello.ll: hello.c
	clang -O0 -emit-llvm hello.c -S -o hello.ll

hello.s: hello.c
	clang -O0 -masm=intel -S -fno-asynchronous-unwind-tables hello.c

.DEFAULT_GOAL := hello
hello: hello.s hello.ll
	clang -O0 hello.s -o hello -lm

.PHONY: clean
clean:
	git clean -xfd
