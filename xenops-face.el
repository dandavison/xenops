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

(defun xenops-face-get-fontified-family-strings (face-spec)
  (mapcar (lambda (family)
            (setq family (substring family))
            (add-face-text-property 0 (length family)
                                    (append `(:family ,family) face-spec)
                                    nil family)
            family)
          (font-family-list)))

(defun xenops-select-font-family (&rest face-spec)
  (interactive)
  (unless face-spec (setq face-spec '(:height 1.1 :foreground "blue4" :weight 'bold)))
  (let* ((completing-read-function
          (if (fboundp 'ivy-completing-read) #'ivy-completing-read completing-read-function))
         (family
          (completing-read "Font family: " (xenops-face-get-fontified-family-strings face-spec))))
    (xenops-face-set-faces family)))

(provide 'xenops-face)

