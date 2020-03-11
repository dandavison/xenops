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
		-l tests/xenops-test--utils.el \
		-l tests/xenops-test--mocks.el \
		-l tests/xenops-test-elements.el \
		-l tests/xenops-test-footnote.el \
		-l tests/xenops-test-image.el \
		-l tests/xenops-test-math-latex.el \
		-l tests/xenops-test-math.el \
		-l tests/xenops-test-src.el \
		-l tests/xenops-test-util.el \
		-l tests/xenops-test-xen.el \
		-f ert-run-tests-batch-and-exit

lint:
	emacs -Q --batch -l elisp-lint.el -f elisp-lint-files-batch *.el

build:
	@:

.PHONY: deps test lint build
