SANDBOX_DIR ?= .sandbox

$(SANDBOX_DIR):
	mkdir $(SANDBOX_DIR)
	./makem.sh -vv --sandbox=$(SANDBOX_DIR) --install-deps --install-linters

sandbox: $(SANDBOX_DIR)

compile: $(SANDBOX_DIR)
	./makem.sh -vv --sandbox=$(SANDBOX_DIR) --exclude kisaragi-notes-cite.el --exclude kisaragi-notes-marginalia.el --exclude kisaragi-notes-embark.el compile

test: $(SANDBOX_DIR)
	./makem.sh -vv --sandbox=$(SANDBOX_DIR) --exclude kisaragi-notes-cite.el --exclude kisaragi-notes-marginalia.el --exclude kisaragi-notes-embark.el test

.PHONY: test sandbox compile
