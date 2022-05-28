.cask: Cask
	cask install

compile: .cask
	@if env | grep eshell -q; then echo "Running tests in eshell will touch the database in ~/.emacs.d for some reason. Please use vterm instead."; exit 1; fi
	cask build

test: .cask
	@if [ "$$CI" != true ]; then make compile; fi # Locally, always rebuild
	cask exec buttercup tests/

.PHONY: test compile
