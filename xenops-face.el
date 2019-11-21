(defvar xenops-face-font-family "CMU Serif"
  "The font family used for all text other than math and source
  code elements in a Xenops buffer.")

(defun xenops-face-set-faces (&optional family)
  (unless family (setq family xenops-face-font-family))
  (face-remap-add-relative 'variable-pitch :family family)
  (face-remap-add-relative 'default :family family :height 160)
  (face-remap-add-relative 'font-latex-math-face 'fixed-pitch :height 140)
  (face-remap-add-relative 'font-latex-verbatim-face 'fixed-pitch :height 140)
  (buffer-face-mode))

(provide 'xenops-face)

