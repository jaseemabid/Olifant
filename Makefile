a.out: a.ll runtime/olifant.o
	clang ./runtime/olifant.o a.ll

a.s: a.ll
	clang -S -fno-asynchronous-unwind-tables ./runtime/olifant.o a.ll

# PIC stands for position independent code, explained here
# https://stackoverflow.com/questions/5311515/gcc-fpic-option
runtime/olifant.o: runtime/olifant.c
	clang -Wall -Wpedantic -fPIC -c runtime/olifant.c -o runtime/olifant.o

.PHONY: docs
docs: src
	stack haddock --no-haddock-deps --open

.PHONY: pretty
pretty:
	find src test exe -name "*.hs" | xargs stylish-haskell -i

.PHONY: clean
clean:
	@rm -f a.ll a.s a.out runtime/olifant.o

.PHONY: container
container:
	docker build . \
		-t jaseemabid/olifant:latest \
		-t jaseemabid/olifant:$(shell git rev-parse HEAD | cut -b1-7) \
		--cache-from jaseemabid/olifant:latest

.PHONY: ctest
ctest: container
	docker run -it jaseemabid/olifant stack test

.PHONY: test
test:
	stack test
