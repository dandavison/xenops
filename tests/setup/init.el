(setq melpa-stable (getenv "MELPA_STABLE"))
(setq package-user-dir
      (format "/tmp/xenops-packages/%s/elpa"
              (concat emacs-version (when melpa-stable "-stable"))))
(message "installing in %s ...\n" package-user-dir)
(package-initialize)
