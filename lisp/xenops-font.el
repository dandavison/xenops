;;; xenops-font.el --- Functions for setting fonts in a Xenops buffer -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:

;; Silence compiler: defined elsewhere
(defvar xenops-font-family)
(defvar xenops-font-height 160
  "The font height for the main font.")
(defvar xenops-font-height-code 140
  "The font height for the font used for LaTeX code, and code in code blocks.")

(defun xenops-font-activate ()
  "Perform xenops-font responsibilities during minor mode activation."
  (if xenops-font-family (xenops-font-set-faces)))

(defun xenops-font-set-faces ()
  "Set certain faces in a Xenops buffer to use `xenops-font-family'."
  (face-remap-add-relative 'variable-pitch :family xenops-font-family)
  (face-remap-add-relative 'default :family xenops-font-family :height xenops-font-height)
  (face-remap-add-relative 'font-latex-math-face 'fixed-pitch :height xenops-font-height-code)
  (face-remap-add-relative 'font-latex-verbatim-face 'fixed-pitch :height xenops-font-height-code)
  (buffer-face-mode))

(defun xenops-font-get-fontified-family-strings (face-spec)
  "Return font family names, fontified for display using FACE-SPEC."
  (mapcar (lambda (family)
            (setq family (copy-sequence family))
            (add-face-text-property 0 (length family)
                                    (append `(:family ,family) face-spec)
                                    nil family)
            family)
          (font-family-list)))

(defun xenops-select-font-family (&rest face-spec)
  "Select and activate a new font family for the current Xenops buffer.

For example, this can be used to choose a font for normal Emacs
buffer text that matches the font in typeset LaTeX SVG output.

Optional argument FACE-SPEC is base face to use when displaying
font family names."
  (interactive)
  (unless face-spec (setq face-spec '(:height 1.1 :foreground "blue4" :weight 'bold)))
  (let* ((completing-read-function
          (if (fboundp 'ivy-completing-read) #'ivy-completing-read completing-read-function))
         (family
          (substring-no-properties
           (completing-read "Font family: " (xenops-font-get-fontified-family-strings face-spec)))))
    (setq xenops-font-family family)
    (xenops-font-set-faces)
    (message "Changed font to %s. To persist this change, set the variable `xenops-font-family' in your emacs init file."
             xenops-font-family)))

(provide 'xenops-font)

;;; xenops-font.el ends here
