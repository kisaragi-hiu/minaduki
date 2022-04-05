.cask: Cask
	cask install

compile: .cask
	cask build

test: .cask
	cask exec buttercup tests/

.PHONY: test compile
