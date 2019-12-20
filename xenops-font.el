;; -*- lexical-binding: t -*-

(defvar xenops-font-font-family nil
  "The font family used for all text other than math and source
  code elements in a Xenops buffer. To make this take effect,
  call `xenops-font-set-faces' or restart `xenops-mode'.")

(defun xenops-font-activate ()
  (if xenops-font-font-family (xenops-font-set-faces)))

(defun xenops-font-set-faces ()
  (interactive)
  (face-remap-add-relative 'variable-pitch :family xenops-font-font-family)
  (face-remap-add-relative 'default :family xenops-font-font-family :height 160)
  (face-remap-add-relative 'font-latex-math-face 'fixed-pitch :height 140)
  (face-remap-add-relative 'font-latex-verbatim-face 'fixed-pitch :height 140)
  (buffer-face-mode))

(defun xenops-font-get-fontified-family-strings (face-spec)
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
          (substring-no-properties
           (completing-read "Font family: " (xenops-font-get-fontified-family-strings face-spec)))))
    (setq xenops-font-font-family family)
    (xenops-font-set-faces)))

(provide 'xenops-font)
