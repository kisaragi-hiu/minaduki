export HOME := /tmp/test

test: Eldev
	eldev test -u off

# For some reason tests fail in bizarre ways when (and only when) coverage is
# enabled. But coverage still gets generated...
coverage-local: Eldev
	@-eldev test -u on,coveralls,dontsend -U ./coverage-final.json >/dev/null 2>/dev/null || true
	@-eldev test -u on,text,dontsend -U /tmp/out.txt >/dev/null 2>/dev/null || true
	@cat /tmp/out.txt
coverage-ci: Eldev
	@-eldev test -u on,coveralls,send >/dev/null 2>/dev/null

.PHONY: test coverage
