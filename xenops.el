(require 'cl)
(require 'org)
(require 'dash)
(require 'xenops-prettify-symbols)
(require 'xenops-preview-latex)
(require 'xenops-image)

(defun xenops-parse-element-at-point ()
  (-any #'funcall '(xenops-image-parse-image-at-point)))

(provide 'xenops)
