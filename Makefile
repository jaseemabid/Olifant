a.out: a.ll
	clang ./runtime/libolifant.o a.ll

a.s: a.ll
	clang -S -fno-asynchronous-unwind-tables ./runtime/libolifant.o a.ll

.PHONY: docs
docs: src
	stack haddock --no-haddock-deps --open

.PHONY: pretty
pretty:
	find src test exe -name "*.hs" | xargs stylish-haskell -i
