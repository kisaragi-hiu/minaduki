SANDBOX_DIR ?= .sandbox

$(SANDBOX_DIR):
	mkdir $(SANDBOX_DIR)
	./makem.sh -vv --sandbox=$(SANDBOX_DIR) --install-deps --install-linters

sandbox: $(SANDBOX_DIR)

test: $(SANDBOX_DIR)
	./makem.sh -vv --sandbox=$(SANDBOX_DIR) --exclude orb-helm.el --exclude orb-ivy.el test --exclude kisaragi-notes-cite.el

.PHONY: test sandbox
