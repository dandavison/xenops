;;; xenops-avy.el --- Operate on elements using avy -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:

(defun xenops-goto-element ()
  (interactive)
  (let (avy-action)
    (xenops-avy-do-at-element)))

(defun xenops-copy-and-paste-element ()
  (interactive)
  (xenops-math-copy-and-paste-element))

(defun xenops-avy-do-at-element (&optional types)
  "Perform an avy action on an element."
  (avy-jump (xenops-elements-delimiter-start-regexp types)))

(provide 'xenops-avy)

;;; xenops-avy.el ends here
