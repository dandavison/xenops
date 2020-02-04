;; -*- lexical-binding: t -*-

;; Based on https://github.com/abo-abo/swiper/blob/master/targets/install-deps.el

(defconst xenops-dependencies
  ;; auctex is installed separately
  '(aio
    avy
    dash
    dash-functional
    f
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
(xenops-dependencies-check-packages)
