#
# This Makefile uses a bunch of magic variables, as explained here in the
# manual.
# https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html#Automatic-Variables
#
# $@ : The file name of the target of the rule.
# $< : The first prerequisite; usually the input file
#

RUNTIME = runtime/olifant.o
CCFLAGS = -g -ggdb3 -m64 -Wall -Wpedantic -Wno-override-module -fno-asynchronous-unwind-tables

# PIC stands for position independent code, explained here
# https://stackoverflow.com/questions/5311515/gcc-fpic-option
$(RUNTIME): runtime/olifant.c
	clang $(CCFLAGS) -fPIC -c $< -o $@

%: %.ll $(RUNTIME)
	clang $(CCFLAGS) $(RUNTIME) $< -o $@

%.s: %.ll $(RUNTIME)
	clang $(CCFLAGS) $(RUNTIME) -S $< -o $@

.PHONY: docs
docs: src
	stack haddock --no-haddock-deps --open

.PHONY: pretty
pretty:
	find src test exe -name "*.hs" | xargs stylish-haskell -i

.PHONY: clean
clean:
	@rm -f *.ll *.s *.out $(RUNTIME)

.PHONY: container
container:
	docker build . \
		-t olifant/olifant:latest \
		-t olifant/olifant:$(shell git rev-parse HEAD | cut -b1-7) \
		--cache-from olifant/olifant:latest

.PHONY: push
push:
	docker push olifant/olifant:latest
	docker push olifant/olifant:$(shell git rev-parse HEAD | cut -b1-7)

.PHONY: ctest
ctest: container
	docker run -it olifant/olifant stack test

.PHONY: test
test:
	stack test
