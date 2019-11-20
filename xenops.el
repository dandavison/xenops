(require 'avy)
(require 'cl)
(require 'dash)
(require 'f)
(require 'org)
(require 's)
(require 'xenops-execute)
(require 'xenops-image)
(require 'xenops-math)
(require 'xenops-apply)
(require 'xenops-text)
(require 'xenops-util)

(defvar xenops-cache-directory "/tmp/xenops-cache/"
  "Path to a directory in which xenops can save files.")

(defvar xenops-mode-map (make-sparse-keymap))

(defvar xenops-tooltip-delay 0.2
  "`tooltip-delay' when xenops-mode is active.")

(defvar xenops-variable-pitch-face-family "CMU Serif"
  "The face family used for variable-pitch
  faces (i.e. proportional fonts, as opposed to fixed-width fonts) in
  a Xenops buffer.")

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

    (face-remap-add-relative 'variable-pitch :family xenops-variable-pitch-face-family)
    (face-remap-add-relative 'default :family xenops-variable-pitch-face-family :height 160)
    (face-remap-add-relative 'font-latex-math-face 'fixed-pitch :height 140)
    (face-remap-add-relative 'font-latex-verbatim-face 'fixed-pitch :height 140)
    (buffer-face-mode)

    (xenops-math-activate)
    (xenops-text-activate)

    ;; Display math and tables as images
    (xenops-display-images-async)
    (save-excursion
      (goto-char (point-min))
      (xenops-display-images-if-cached)))

   ;; Deactivate
   (t
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (xenops-hide-images)))
    (xenops-math-deactivate)
    (xenops-text-deactivate))))

(defun xenops (&optional arg)
  (interactive "P")
  (cond
   ((equal arg '(16))
    (xenops-hide-images))
   ((equal arg '(4))
    (xenops-regenerate-images))
   (t (xenops-display-images))))

(defvar xenops-ops
  `((math . (:ops
             (xenops-math-display-image
              xenops-math-regenerate-image
              xenops-math-hide-image
              xenops-math-image-increase-size
              xenops-math-image-decrease-size
              xenops-math-image-reset)
             :delimiters
             (("\\$" .
               "\\$")
              ("^[ \t]*\\\\begin{align\\*?}" .
               "^[ \t]*\\\\end{align\\*?}")
              ("^[ \t]*\\\\begin{tabular}" .
               "^[ \t]*\\\\end{tabular}"))))
    (image . (:ops
              (xenops-image-display-image
               xenops-image-hide-image)
              :delimiters
              (("[ \t]*\\\\includegraphics\\(\\[[^]]+\\]\\)?{\\([^}]+\\)}"))))
    (footnote . (:ops
                 (xenops-text-render-footnote
                  xenops-element-delete-overlays)
                 :delimiters
                 ((,(concat "\\\\footnote"
                            xenops-text-brace-delimited-multiline-expression-regexp)))))))

(defun xenops-display-images ()
  (interactive)
  (xenops-apply '(xenops-math-display-image
                  xenops-image-display-image
                  xenops-text-render-footnote)))

(defun xenops-display-images-if-cached ()
  (let ((fn (symbol-function 'xenops-math-display-image)))
    (cl-letf (((symbol-function 'xenops-math-display-image)
               (lambda (element) (funcall fn element 'cached-only))))
      (xenops-display-images))))

(defun xenops-regenerate-images ()
  (interactive)
  (xenops-apply '(xenops-math-regenerate-image)))

(defun xenops-hide-images ()
  (interactive)
  (xenops-apply '(xenops-math-hide-image
                  xenops-image-hide-image
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
    (dolist (o (overlays-in beg end))
      (when (eq (overlay-get o 'org-overlay-type)
                'org-latex-overlay)
        (delete-overlay o)))))

(defun xenops-handle-paste ()
  (interactive)
  (or (xenops-math-handle-paste)
      (xenops-image-handle-paste)))

(defun xenops-display-images-async ()
  "Run `xenops-display-images' on the current buffer's file, asynchronously."
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
                                                            (xenops-display-images-if-cached)
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
    (xenops-display-images)))

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
