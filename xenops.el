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

(defun xenops-parse-element-at-point ()
  (-any #'funcall '(xenops-display-image-parse-image-at-point)))

(provide 'xenops)
