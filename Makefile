test:
	emacs \
		-batch \
		-l ~/.emacs.d/init.el \
		-l tests/xenops-math.el \
		-l tests/xenops-image.el \
		-l tests/xenops-util.el \
		-f ert-run-tests-batch-and-exit
