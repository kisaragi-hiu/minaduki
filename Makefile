.cask: Cask
	cask install

compile: .cask
	cask build

test: .cask compile
	cask exec buttercup tests/

.PHONY: test compile
