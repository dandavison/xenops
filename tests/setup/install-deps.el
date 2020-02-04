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
        (if melpa-stable
            '(("melpa-stable" . "https://stable.melpa.org/packages/"))
          '(("melpa" . "https://melpa.org/packages/")))))

(defun xenops-package-install (package)
  (interactive "Spackage: ")
  (if (package-installed-p package)
      (message "%S: OK" package)
    (progn
      (package-install package)
      (message "%S: ...OK" package))))

(xenops-dependencies-setup-package-repositories)
(when xenops-install-auctex-from-elpa
  (push '("gnu" . "https://elpa.gnu.org/packages/") package-archives)
  (push 'auctex xenops-dependencies))
(package-refresh-contents)
(mapc #'xenops-package-install xenops-dependencies)
