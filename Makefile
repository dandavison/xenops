EMACS = emacs

deps:
	mkdir -p /tmp/xenops-packages
	bash tests/setup/install-auctex.sh /tmp/xenops-packages
	$(EMACS) \
		-batch \
		-l tests/setup/init.el \
		-l tests/setup/install-deps.el

test:
	$(EMACS) \
		-batch \
		-l tests/setup/init.el \
		--eval "(add-to-list 'load-path \"lisp\")" \
		--eval "(add-to-list 'load-path \"/tmp/xenops-packages/auctex\")" \
		--eval '(setq ert-batch-backtrace-right-margin 9999)' \
		-l lisp/xenops.el \
		-l tests/init.el \
		-l tests/xenops.el \
		-l tests/xenops-apply.el \
		-l tests/xenops-util.el \
		-l tests/xenops-element.el \
		-l tests/xenops-image.el \
		-l tests/xenops-math.el \
		-l tests/xenops-src.el \
		-l tests/xenops-xen.el \
		-f ert-run-tests-batch-and-exit

lint:
	emacs -Q --batch -l elisp-lint.el -f elisp-lint-files-batch *.el

build:
	@:

.PHONY: deps test lint build
