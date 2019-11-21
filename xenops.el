(require 'avy)
(require 'cl)
(require 'dash)
(require 'f)
(require 'org)
(require 's)
(require 'xenops-element)
(require 'xenops-execute)
(require 'xenops-face)
(require 'xenops-image)
(require 'xenops-math)
(require 'xenops-text)
(require 'xenops-util)

(defvar xenops-cache-directory "/tmp/xenops-cache/"
  "Path to a directory in which xenops can save files.")

(defvar xenops-mode-map (make-sparse-keymap))

(defvar xenops-tooltip-delay 0.2
  "`tooltip-delay' when xenops-mode is active.")

(define-minor-mode xenops-mode
  "A LaTeX editing environment.

\\{xenops-mode-map}"
  :lighter " Xenops"
  (cond
   (xenops-mode
    (define-key xenops-mode-map "\C-c\C-c" #'xenops)
    (define-key xenops-mode-map [(double-down-mouse-1)] #'xenops-reveal-at-point)
    (define-key xenops-mode-map [(down)] (lambda () (interactive) (xenops-math-toggle-on-transition #'next-line)))
    (define-key xenops-mode-map [(down-mouse-1)] #'xenops-math-handle-first-click)
    (define-key xenops-mode-map [(left)] (lambda () (interactive) (xenops-math-toggle-on-transition #'left-char)))
    (define-key xenops-mode-map [(mouse-1)] #'xenops-math-handle-mouse-1)
    (define-key xenops-mode-map [(right)] (lambda () (interactive) (xenops-math-toggle-on-transition #'right-char)))
    (define-key xenops-mode-map [(up)] (lambda () (interactive) (xenops-math-toggle-on-transition #'previous-line)))
    (xenops-util-define-key-with-fallback "\C-y" #'xenops-handle-paste)
    (xenops-util-define-key-with-fallback "\M-w" #'xenops-copy-at-point)
    (define-key xenops-mode-map (kbd "s-0") #'xenops-reset-size)
    (define-key xenops-mode-map (kbd "s-+") #'xenops-increase-size)
    (define-key xenops-mode-map (kbd "s--") #'xenops-decrease-size)
    (define-key xenops-mode-map (kbd "s-=") #'xenops-increase-size)
    (define-key xenops-mode-map (kbd "s-_") #'xenops-decrease-size)
    (xenops-util-define-key-with-fallback [(backspace)] #'xenops-delete-at-point)
    (xenops-util-define-key-with-fallback [(return)] #'xenops-reveal-at-point)
    (xenops-util-define-key-with-fallback [(super c)] #'xenops-copy-at-point "\M-w")
    (xenops-util-define-key-with-fallback [(super v)] #'xenops-handle-paste "\C-y")
    (xenops-util-define-key-with-fallback [(super x)] #'xenops-delete-at-point)

    (if xenops-face-font-family (xenops-face-set-faces))

    (xenops-math-activate)
    (xenops-text-activate)

    ;; Display math and tables as images
    (xenops-render-async)
    (save-excursion
      (goto-char (point-min))
      (xenops-render-if-cached)))

   ;; Deactivate
   (t
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (xenops-apply 'reveal)))
    (xenops-math-deactivate)
    (xenops-text-deactivate))))

(defun xenops (&optional arg)
  (interactive "P")
  (cond
   ((equal arg '(16))
    (xenops-apply 'regenerate))
   ((equal arg '(4))
    (xenops-apply 'reveal))
   (t (xenops-apply 'render))))

(defvar xenops-ops
  '((render . (xenops-math-render
               xenops-image-render
               xenops-text-footnote-render))
    (reveal . (xenops-math-reveal
               xenops-image-reveal
               xenops-element-reveal))
    (regenerate . (xenops-math-regenerate))
    (copy . (xenops-element-copy))
    (delete . (xenops-element-delete))
    (increase-size . (xenops-math-image-increase-size))
    (decrease-size . (xenops-math-image-decrease-size))
    (reset-size . (xenops-math-image-reset-size)))
  "Element-specific operation functions grouped by operation type.")

(defvar xenops-elements
  `((math . (:ops
             (xenops-math-render
              xenops-math-regenerate
              xenops-math-reveal
              xenops-math-image-increase-size
              xenops-math-image-decrease-size
              xenops-math-image-reset-size
              xenops-element-copy
              xenops-element-delete)
             :delimiters
             (("^[ \t]*\\\\begin{align\\*?}" .
               "^[ \t]*\\\\end{align\\*?}")
              ("^[ \t]*\\\\begin{tabular}" .
               "^[ \t]*\\\\end{tabular}"))
             :parse-at-point
             xenops-math-parse-element-at-point
             :parse-match
             xenops-math-parse-match))
    (inline-math . (:ops
                    math
                    :delimiters
                    (("\\$" . "\\$"))
                    :parse-match
                    math))
    (image . (:ops
              (xenops-image-render
               xenops-image-reveal
               xenops-element-copy
               xenops-element-delete)
              :delimiters
              (("[ \t]*\\\\includegraphics\\(\\[[^]]+\\]\\)?{\\([^}]+\\)}"))
              :parse-match
              xenops-image-parse-match))
    (footnote . (:ops
                 (xenops-text-footnote-render
                  xenops-element-reveal
                  xenops-element-copy
                  xenops-element-delete)
                 :delimiters
                 ((,(concat "\\\\footnote"
                            xenops-text-brace-delimited-multiline-expression-regexp)))
                 :parse-match
                 xenops-text-footnote-parse-match)))
  "Element-specific operation functions, regexps, and parsers, grouped by element type.")

(defmacro xenops-define-apply-command (op-type docstring)
  `(defun ,(intern (concat "xenops-" (symbol-name op-type))) ()
     ,(concat docstring " "
              "The elements operated on are determined by trying the following:
1. The element at point, if any.
2. Elements in the active region, if there is an active region.
3. All elements in the buffer.")
     (interactive)
     (xenops-apply ',op-type)))

(defmacro xenops-define-apply-at-point-command (op-type docstring)
  `(defun ,(intern (concat "xenops-" (symbol-name op-type) "-at-point")) ()
     ,docstring
     (interactive)
     (-when-let (el (xenops-element-parse-at-point))
       (-when-let (op (xenops-element-op-of-type-for-el el ',op-type))
         (funcall op el)))))

(xenops-define-apply-command render
                             "Render elements: display LaTeX math, tables and included image files as images, and hide footnotes with tooltips.")
(xenops-define-apply-command reveal
                             "Reveal elements: this is the opposite of `xenops-render'. For LaTeX math, tables, and footnotes, reveal the LaTeX code for editing")
(xenops-define-apply-command regenerate
                             "Regenerate elements: for LaTeX math and footnotes, send the LaTeX to an external process to regenerate the image file, and display the new image.")
(xenops-define-apply-command increase-size
                             "Increase size of images.")
(xenops-define-apply-command decrease-size
                             "Decrease size of images.")
(xenops-define-apply-command reset-size
                             "Reset size of images.")

(xenops-define-apply-at-point-command reveal
                                      "Reveal the element at point.")

(xenops-define-apply-at-point-command copy
                                      "Copy the element at point.")

(xenops-define-apply-at-point-command delete
                                      "Delete the element at point.")

(defun xenops-render-if-cached ()
  (let ((fn (symbol-function 'xenops-math-render)))
    (cl-letf (((symbol-function 'xenops-math-render)
               (lambda (element) (funcall fn element 'cached-only))))
      (xenops-apply 'render))))

(defun xenops-handle-paste ()
  (interactive)
  (or (xenops-math-handle-paste)
      (xenops-image-handle-paste)))

(defun xenops-render-async ()
  "Run `xenops-render' on the current buffer's file, asynchronously."
  (interactive)
  (message "Xenops: processing images asynchronously")
  (async-start `(lambda ()
                  (package-initialize)
                  (add-to-list 'load-path
                               ,(file-name-directory (find-library-name "xenops")))
                  (require 'xenops)
                  (find-file ,(buffer-file-name))
                  (xenops-mode)
                  (cl-letf (((symbol-function 'xenops-math-file-name-static-hash-data)
                             (lambda () ',(xenops-math-file-name-static-hash-data))))
                    (xenops-generate-images-in-headless-process)))
               (lambda (result)
                 (run-with-idle-timer 0 nil
                                      (lambda () (save-excursion (goto-char (point-min))
                                                            (xenops-render-if-cached)
                                                            (message "Xenops: done")))))))

(defun xenops-generate-images-in-headless-process ()
  "Generate cached images on disk for all math elements in
buffer, when running in a headless emacs process."
  (cl-letf (((symbol-function 'org--get-display-dpi) (lambda () 129))
            ((symbol-function 'org-latex-color)
             (lambda (attr)
               (case attr
                 (:foreground "0,0,0")
                 (:background "1,1,1")
                 (t (error "Unexpected input: %s" attr))))))
    (xenops-apply 'render)))

(defun xenops-avy-goto-math ()
  (interactive)
  (let (avy-action) (xenops-avy-do-at-math)))

(defun xenops-avy-copy-math-and-paste ()
  (interactive)
  (let ((element)
        (avy-action
         (lambda (pt)
           (save-excursion
             (goto-char
              ;; TODO: hack: This should be just `pt`, but inline
              ;; math elements are not recognized when point is on
              ;; match for first delimiter.
              (1+ pt))
             (setq element (xenops-math-parse-element-at-point))
             (when element (xenops-math-copy element)))
           (when element
             (save-excursion (xenops-math-paste))))))
    (xenops-avy-do-at-math)))

(defun xenops-avy-do-at-math ()
  (avy-jump (xenops-math-get-math-element-begin-regexp)))

(provide 'xenops)
