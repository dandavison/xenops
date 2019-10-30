(require 'cl)
(require 'dash)
(require 'f)
(require 'org)
(require 'xenops-display-image)
(require 'xenops-display-math)
(require 'xenops-display-text)
(require 'xenops-process)

(defvar xenops-cache-directory "/tmp/xenops-cache/"
  "Path to a directory in which xenops can save files.")

(defvar xenops-mode-map (make-sparse-keymap))

(define-minor-mode xenops-mode
  "A LaTeX editing environment.

\\{xenops-mode-map}"
  nil " xenops" nil
  (cond
   (xenops-mode
    (define-key xenops-mode-map "\C-c\C-c" 'xenops)
    (xenops-display-image-activate)
    (xenops-display-math-activate)
    (xenops-display-text-activate))
   ;; TODO: deactivate
))

(defun xenops (&optional arg)
  (interactive "P")
  (cond
    ((equal arg '(16))
     (xenops-hide))
    ((equal arg '(4))
     (xenops-regenerate))
    (t (xenops-display))))

(defvar xenops-ops
  '((math . (:ops
             (xenops-display-math-
              xenops-display-math-regenerate-
              xenops-display-math-hide-)
             :delimiters
             (("\\$" .
               "\\$")
              ("^[ \t]*\\\\begin{align\\*?}" .
               "^[ \t]*\\\\end{align\\*?}"))))
    (image . (:ops
              (xenops-display-image-
               xenops-display-image-hide-)
              :delimiters
              (("[ \t]*\\\\includegraphics\\(\\[[^]]+\\]\\)?{\\([^}]+\\)}"))))))

(defun xenops-display ()
  (interactive)
  (xenops-process '(xenops-display-math- xenops-display-image-)))

(defun xenops-regenerate ()
  (interactive)
  (xenops-process '(xenops-display-math-regenerate-)))

(defun xenops-hide ()
  (interactive)
  (xenops-process '(xenops-display-math-hide- xenops-display-image-hide-)))

(provide 'xenops)
