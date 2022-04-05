.cask: Cask
	cask install

compile: .cask
	cask build

test: .cask
	if [ "$$CI" != true ]; then make compile; fi # Locally, always rebuild
	cask exec buttercup tests/

.PHONY: test compile
