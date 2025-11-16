export HOME := /tmp/test

.eask: Eask
	eask install-deps --dev
	touch .eask # needed because Eask doesn't update the time

minaduki.elc: .eask $(wildcard *.el)
	eask compile

compile: minaduki.elc

test: .eask
	@if [ "$$CI" != true ]; then make compile; fi # Locally, always rebuild
	eask test buttercup

.PHONY: test compile
