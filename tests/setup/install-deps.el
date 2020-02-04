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

(xenops-dependencies-setup-package-repositories)
(mapc #'xenops-package-install xenops-dependencies)
