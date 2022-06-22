export HOME := /tmp/test

.cask: Cask
	cask install

minaduki.elc: .cask $(wildcard *.el)
	cask build

compile: minaduki.elc

test: .cask
	@if [ "$$CI" != true ]; then make compile; fi # Locally, always rebuild
	cask exec buttercup tests/

.PHONY: test compile
