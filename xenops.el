(require 'cl)
(require 'org)
(require 'dash)
(require 'xenops-display-image)
(require 'xenops-display-math)
(require 'xenops-display-text)

(defvar xenops-cache-directory "/tmp/xenops-cache/"
  "Path to a directory in which xenops can save files.")

(defvar xenops-math-delimiters
  '(("\\$" . "\\$")
    ("^[ \t]*\\\\begin{align\\*?}" . "^[ \t]*\\\\end{align\\*?}")))

(defvar xenops-mode-map (make-sparse-keymap))

(define-minor-mode xenops-mode
  "A LaTeX editing environment.

\\{xenops-mode-map}"
  nil " xenops" nil
  (cond
   (xenops-mode
    (define-key xenops-mode-map "\C-c\C-c" 'xenops-dwim)
    (xenops-display-image-activate)
    (xenops-display-math-activate)
    (xenops-display-text-activate))
   ;; TODO: deactivate
))

(defun xenops-dwim (&optional arg)
  (interactive "P")
  (cond
    ((equal arg '(16))
     (xenops-display-math-hide))
    ((equal arg '(4))
     (xenops-display-math-regenerate))
    (t (xenops-display-math-dwim))))

(defun xenops-parse-element-at-point ()
  (-any #'funcall '(xenops-display-image-parse-image-at-point)))

(provide 'xenops)
