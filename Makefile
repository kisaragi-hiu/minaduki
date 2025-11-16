export HOME := /tmp/test

test: Eldev
	eldev test

# For some reason tests fail in bizarre ways when (and only when) coverage is
# enabled. But coverage still gets generated...
coverage-text: Eldev
	@-env K_COVERAGE=text,/tmp/out.txt eldev test >/dev/null 2>/dev/null
	@cat /tmp/out.txt
coverage-local: Eldev
	@-env K_COVERAGE=coveralls,./coverage-final.json eldev test >/dev/null 2>/dev/null
	@cat ./coverage-final.json
coverage-ci: Eldev
	env K_COVERAGE=coveralls eldev test >/dev/null 2>/dev/null
	@cat /tmp/out

.PHONY: test coverage
