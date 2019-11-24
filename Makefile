test:
	emacs \
		-batch \
		--eval '(package-initialize)' \
		--eval '(require `use-package)' \
		--eval '(use-package xenops :load-path "~/src/xenops")' \
		--eval '(setq ert-batch-backtrace-right-margin nil)' \
		-l tests/xenops.el \
		-l tests/xenops-element.el \
		-l tests/xenops-math.el \
		-l tests/xenops-image.el \
		-l tests/xenops-util.el \
		-f ert-run-tests-batch-and-exit

build:
	@:
