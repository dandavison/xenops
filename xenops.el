(require 'cl)
(require 'org)
(require 'dash)
(require 'xenops-display-text)
(require 'xenops-preview-latex)
(require 'xenops-image)

(defvar xenops-cache-directory "/tmp/xenops-cache/"
  "Path to a directory in which xenops can save files.")

(defun xenops-parse-element-at-point ()
  (-any #'funcall '(xenops-image-parse-image-at-point)))

(provide 'xenops)
