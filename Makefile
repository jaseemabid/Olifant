.PHONY: docs
docs: src
	stack haddock --haddock --no-haddock-deps

.PHONY: pretty
pretty:
	find src test exe -name "*.hs" | xargs stylish-haskell -i
