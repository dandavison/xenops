;;; xenops-minted.el --- Utilities for working with elements of type 'minted -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Elements of type 'minted represent LaTeX minted environments (https://github.com/gpoore/minted).
;; They contain code. Xenops enables execution (via org-babel) and syntax highlighting of elements
;; of type 'minted and 'src.

;;; Code:

(declare-function xenops-parse-element-at-point "xenops-parse")
(declare-function xenops-util-plist-update "xenops-util")

(defun xenops-minted-parse-at-point ()
  "Parse a minted element at point."
  (-if-let* ((element (xenops-parse-element-at-point 'minted)))
      (let* ((language (xenops-minted-get-babel-language (match-string 2)))
             (parameters (match-string 3))
             (body (buffer-substring (plist-get element :begin-content)
                                     (plist-get element :end-content)))
             (org-element `(src-block (:language ,language :parameters ,parameters :value ,body)))
             (org-babel-info (org-babel-get-src-block-info 'light org-element)))
        (xenops-util-plist-update
         element
         :language language
         :org-babel-info org-babel-info))))

(defvar xenops-minted-language-to-babel-language
  '(("python3" . "python")
    ("wolfram" . "mathematica")))

(defun xenops-minted-get-babel-language (language)
  "Return org-babel language name corresponding to LANGUAGE."
  (or (cdr (assoc language xenops-minted-language-to-babel-language))
      language))

(provide 'xenops-minted)

;;; xenops-minted.el ends here
