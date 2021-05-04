;; -*- lexical-binding: t -*-

;; Based on https://github.com/abo-abo/swiper/blob/master/targets/install-deps.el

(defconst xenops-dependencies
  ;; auctex is installed separately
  '(aio
    avy
    dash
    f
    s
    use-package))

(defun xenops-package-install (package)
  (interactive "Spackage: ")
  (if (package-installed-p package)
      (message "%S: OK" package)
    (progn
      (package-install package)
      (message "%S: ...OK" package))))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(when xenops-install-auctex-from-elpa
  (push '("gnu" . "https://elpa.gnu.org/packages/") package-archives)
  (push 'auctex xenops-dependencies))
(package-refresh-contents)
(mapc #'xenops-package-install xenops-dependencies)

(require 'org)
(unless (string-match "^9\." org-version)
  (message "org-version is %S but >= 9 is required" org-version)
  (kill-emacs 1))
