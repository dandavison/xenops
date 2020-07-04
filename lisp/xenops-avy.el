;;; xenops-avy.el --- Operate on elements using avy -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:

(defun xenops-goto-element ()
  "Jump to a xenops element."
  (interactive)
  (let (avy-action)
    (xenops-avy-do-at-element)))

(defun xenops-copy-and-paste-element ()
  "Copy a xenops element and paste ijt at point."
  (interactive)
  (xenops-math-copy-and-paste-element))

(defun xenops-avy-do-at-element (&optional types)
  "Perform an avy action on an element of a type in TYPES."
  (avy-jump (xenops-elements-delimiter-start-regexp types)))

(provide 'xenops-avy)

;;; xenops-avy.el ends here
