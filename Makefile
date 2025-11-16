export HOME := /tmp/test

test: Eldev
	eldev test -u off

# For some reason tests fail in bizarre ways when (and only when) coverage is
# enabled. But coverage still gets generated...
coverage-summary: Eldev
	@-eldev test -u on,text,dontsend -U /tmp/out >/dev/null 2>/dev/null
	@cat /tmp/out

.PHONY: test coverage
