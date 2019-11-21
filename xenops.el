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
    (define-key xenops-mode-map [(double-down-mouse-1)] #'xenops-math-handle-second-click)
    (define-key xenops-mode-map [(down)] (lambda () (interactive) (xenops-math-toggle-on-transition #'next-line)))
    (define-key xenops-mode-map [(down-mouse-1)] #'xenops-math-handle-first-click)
    (define-key xenops-mode-map [(left)] (lambda () (interactive) (xenops-math-toggle-on-transition #'left-char)))
    (define-key xenops-mode-map [(mouse-1)] #'xenops-math-handle-mouse-1)
    (define-key xenops-mode-map [(right)] (lambda () (interactive) (xenops-math-toggle-on-transition #'right-char)))
    (define-key xenops-mode-map [(up)] (lambda () (interactive) (xenops-math-toggle-on-transition #'previous-line)))
    (xenops-util-define-key-with-fallback "\C-y" #'xenops-handle-paste)
    (xenops-util-define-key-with-fallback "\M-w" #'xenops-math-handle-copy)
    (define-key xenops-mode-map (kbd "s-0") #'xenops-image-reset)
    (define-key xenops-mode-map (kbd "s-+") #'xenops-image-increase-size)
    (define-key xenops-mode-map (kbd "s--") #'xenops-image-decrease-size)
    (define-key xenops-mode-map (kbd "s-=") #'xenops-image-increase-size)
    (define-key xenops-mode-map (kbd "s-_") #'xenops-image-decrease-size)
    (xenops-util-define-key-with-fallback [(backspace)] #'xenops-math-handle-delete)
    (xenops-util-define-key-with-fallback [(return)] #'xenops-math-handle-return)
    (xenops-util-define-key-with-fallback [(super c)] #'xenops-math-handle-copy "\M-w")
    (xenops-util-define-key-with-fallback [(super v)] #'xenops-handle-paste "\C-y")
    (xenops-util-define-key-with-fallback [(super x)] #'xenops-math-handle-delete)

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
        (xenops-reveal)))
    (xenops-math-deactivate)
    (xenops-text-deactivate))))

(defun xenops (&optional arg)
  (interactive "P")
  (cond
   ((equal arg '(16))
    (xenops-regenerate))
   ((equal arg '(4))
    (xenops-reveal))
   (t (xenops-render))))

(defvar xenops-elements
  `((math . (:ops
             (xenops-math-render
              xenops-math-regenerate
              xenops-math-reveal
              xenops-math-image-increase-size
              xenops-math-image-decrease-size
              xenops-math-image-reset)
             :delimiters
             (("\\$" .
               "\\$")
              ("^[ \t]*\\\\begin{align\\*?}" .
               "^[ \t]*\\\\end{align\\*?}")
              ("^[ \t]*\\\\begin{tabular}" .
               "^[ \t]*\\\\end{tabular}"))
             :parser
             xenops-math-parse-match))
    (image . (:ops
              (xenops-image-render
               xenops-image-reveal)
              :delimiters
              (("[ \t]*\\\\includegraphics\\(\\[[^]]+\\]\\)?{\\([^}]+\\)}"))
              :parser
              xenops-image-parse-match))
    (footnote . (:ops
                 (xenops-text-footnote-render
                  xenops-element-reveal)
                 :delimiters
                 ((,(concat "\\\\footnote"
                            xenops-text-brace-delimited-multiline-expression-regexp)))
                 :parser
                 xenops-text-footnote-parse-match))))

(defun xenops-render ()
  (interactive)
  (xenops-apply '(xenops-math-render
                  xenops-image-render
                  xenops-text-footnote-render)))

(defun xenops-render-if-cached ()
  (let ((fn (symbol-function 'xenops-math-render)))
    (cl-letf (((symbol-function 'xenops-math-render)
               (lambda (element) (funcall fn element 'cached-only))))
      (xenops-render))))

(defun xenops-regenerate ()
  (interactive)
  (xenops-apply '(xenops-math-regenerate)))

(defun xenops-reveal ()
  (interactive)
  (xenops-apply '(xenops-math-reveal
                  xenops-image-reveal
                  xenops-element-delete-overlays)))

(defun xenops-image-increase-size ()
  (interactive)
  (xenops-apply '(xenops-math-image-increase-size)))

(defun xenops-image-decrease-size ()
  (interactive)
  (xenops-apply '(xenops-math-image-decrease-size)))

(defun xenops-image-reset ()
  (interactive)
  (xenops-apply '(xenops-math-image-reset)))

(defun xenops-parse-element-at-point ()
  (xenops-math-parse-element-at-point))

(defun xenops-element-delete-overlays (element)
  (let ((beg (plist-get element :begin))
        (end (plist-get element :end)))
    (dolist (ov (overlays-in beg end))
      (when (overlay-get ov 'xenops-overlay-type)
        (delete-overlay ov)))))

(defalias 'xenops-element-reveal #'xenops-element-delete-overlays)

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
    (xenops-render)))

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
