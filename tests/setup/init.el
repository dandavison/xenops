(setq melpa-stable (getenv "MELPA_STABLE"))
(setq xenops-package-dir "/tmp/xenops-packages"
      package-user-dir (concat xenops-package-dir
                               "/"
                               emacs-version
                               "/"
                               (if melpa-stable "stable/" "")
                               "elpa"))
(message "installing in %s ...\n" package-user-dir)
(package-initialize)
(load-file (concat xenops-package-dir "/auctex/auctex.el"))
(load-file (concat xenops-package-dir "/auctex/preview-latex.el"))
