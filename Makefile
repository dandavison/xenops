deps: install_gnutls
	emacs \
		-batch \
		-l tests/setup/init.el \
		-l tests/setup/install-deps.el

test:
	emacs \
		-batch \
		-l tests/setup/init.el \
		--eval "(add-to-list 'load-path \".\")" \
		--eval '(setq ert-batch-backtrace-right-margin nil)' \
		-l xenops.el \
		-l tests/xenops.el \
		-l tests/xenops-apply.el \
		-l tests/xenops-util.el \
		-l tests/xenops-element.el \
		-l tests/xenops-image.el \
		-l tests/xenops-math.el \
		-l tests/xenops-src.el \
		-l tests/xenops-xen.el \
		-f ert-run-tests-batch-and-exit

build:
	@:

install_gnutls:
ifeq ($(TRAVIS_OS_NAME),osx)
#	GnuTLS can be upgraded with Homebrew instead of apt-get (which obviously does not exist on macOS)
	@echo "Upgrade GnuTLS 3"
	@if brew outdated --quiet | grep --quiet "gnutls"; then brew upgrade gnutls; fi
else
	@echo "Install GnuTLS 3"
	@sudo apt-get -qq update
	@sudo apt-get install -y build-essential nettle-dev libgmp-dev
	@wget ftp://ftp.gnutls.org/gcrypt/gnutls/v3.1/gnutls-3.1.23.tar.xz
	@tar -xf gnutls-3.1.23.tar.xz
	@cd gnutls-3.1.23 \
	  && ./configure $(SILENT) \
	  && make -j$(MAKE_JOBS) $(SILENT) \
	  && sudo make install $(SILENT) \
	  && sudo ln -s /usr/local/lib/libgnutls.so.28 /usr/lib/libgnutls.so.28
endif

.PHONY: deps test build install_gnutls
