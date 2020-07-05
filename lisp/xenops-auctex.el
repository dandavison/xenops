;;; xenops-auctex.el --- Xenops-auctex integration -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:

(defvar xenops-auctex-electric-insert-commands
  #'(LaTeX-insert-left-brace TeX-insert-sub-or-superscript))

(defun xenops-auctex-activate ()
  "Activate auctex-related functionality."
  (dolist (fn xenops-auctex-electric-insert-commands)
    (if (fboundp fn)
        (advice-add fn :around #'xenops-auctex-electric-insert-around-advice))))

(defun xenops-auctex-deactivate ()
  "Deactivate auctex-related functionality."
  (dolist (fn xenops-auctex-electric-insert-commands)
    (if (fboundp fn)
        (advice-remove fn #'xenops-auctex-electric-insert-around-advice))))

(defun xenops-auctex-electric-insert-around-advice (orig-fn &rest args)
  "Around-advice function for electric-insert commands.

'Electric' insert commands such as `LaTeX-insert-left-brace' and
`TeX-insert-sub-or-superscript' should not trigger
render-on-exit.

ORIG-FN is the function to advise, taking arguments ARGS."
  (let ((orig-value cursor-sensor-inhibit))
    (setq cursor-sensor-inhibit (append cursor-sensor-inhibit '(electric-insert)))
    (apply orig-fn args)
    (run-with-idle-timer 0 nil (lambda () (setq cursor-sensor-inhibit orig-value)))))

(provide 'xenops-auctex)

;;; xenops-auctex.el ends here
