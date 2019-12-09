;; Based on https://github.com/abo-abo/swiper/blob/master/targets/install-deps.el

(defconst xenops-dependencies
  ;; auctex is installed separately
  '(avy
    dash
    dash-functional
    f
    org
    s
    use-package))

(defun xenops-dependencies-setup-package-repositories ()
  (setq package-archives
        (list (if melpa-stable
                  '("melpa-stable" . "https://stable.melpa.org/packages/")
                '("melpa" . "https://melpa.org/packages/"))
              '("org" . "https://orgmode.org/elpa/")))
  (package-refresh-contents))

(defun xenops-package-install (package)
  (interactive "Spackage: ")
  (if (package-installed-p package)
      (message "%S: OK" package)
    (progn
      (package-install package)
      (message "%S: ...OK" package))))

(defun xenops-dependencies-check-packages ()
  (unless (string-match "^9\." org-version)
    (message "org-version is %S but >= 9 is required" org-version)
    (kill-emacs 1))
  (save-window-excursion
    (package-list-packages t)
    (condition-case nil
        (progn
          (package-menu-mark-upgrades)
          (package-menu-execute t))
      (error
       (message "All packages up to date")))))

(xenops-dependencies-setup-package-repositories)
(mapc #'xenops-package-install xenops-dependencies)
(use-package org :ensure org-plus-contrib :pin org)
(xenops-dependencies-check-packages)
