export HOME := /tmp/test

test: Eldev
	eldev test -u off

# For some reason tests fail in bizarre ways when (and only when) coverage is
# enabled. But coverage still gets generated...
coverage-text: Eldev
	@-eldev test -u on,text,dontsend -U /tmp/out.txt >/dev/null 2>/dev/null
	@cat /tmp/out.txt
coverage-local: Eldev
	@-eldev test -u on,coveralls,dontsend -U ./coverage-final.json >/dev/null 2>/dev/null
	@echo Coverage info should be available now, try cov-mode in Emacs
coverage-ci: Eldev
	@-eldev test -u on,coveralls,send >/dev/null 2>/dev/null

.PHONY: test coverage
