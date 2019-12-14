(require 'cl-lib)
(require 'org)
(require 'subr-x)

(require 'avy)
(require 'dash)
(require 'dash-functional)
(require 'f)
(require 'preview)
(require 's)

(require 'xenops-apply)
(require 'xenops-auctex)
(require 'xenops-compat)
(require 'xenops-element)
(require 'xenops-elements)
(require 'xenops-font)
(require 'xenops-footnote)
(require 'xenops-image)
(require 'xenops-math)
(require 'xenops-minted)
(require 'xenops-overlay)
(require 'xenops-parse)
(require 'xenops-src)
(require 'xenops-util)
(require 'xenops-xen)

(defvar xenops-cache-directory "/tmp/xenops-cache/"
  "Path to a directory in which xenops can save files.")

(defvar xenops-mode-map (make-sparse-keymap))

(defvar xenops-tooltip-delay 0.2
  "`tooltip-delay' when xenops-mode is active.")

(defvar xenops-rendered-element-keymap (make-sparse-keymap)
  "A keymap that is active when point is on a rendered element,
  such as a math/table image.")

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
(xenops-define-apply-command rotate
                             "Rotate image by 90 degrees.")
(xenops-define-apply-command save
                             "Save image to file.")

(xenops-define-apply-at-point-command reveal
                                      "Reveal the element at point.")

(xenops-define-apply-at-point-command copy
                                      "Copy the element at point.")

(xenops-define-apply-at-point-command delete
                                      "Delete the element at point.")

(xenops-define-apply-at-point-command execute
                                      "Execute the org-src code block at point.")

(define-minor-mode xenops-mode
  "A LaTeX editing environment.

\\{xenops-mode-map}"
  :lighter " Xenops"
  (cond
   (xenops-mode

    (define-key xenops-mode-map "\C-c\C-c" #'xenops-dwim)
    (define-key xenops-mode-map (kbd "s-0") #'xenops-reset-size)
    (define-key xenops-mode-map (kbd "s-+") #'xenops-increase-size)
    (define-key xenops-mode-map (kbd "s--") #'xenops-decrease-size)
    (define-key xenops-mode-map (kbd "s-=") #'xenops-increase-size)
    (define-key xenops-mode-map (kbd "s-_") #'xenops-decrease-size)

    (define-key xenops-rendered-element-keymap [(return)] #'xenops-reveal-at-point)
    (define-key xenops-rendered-element-keymap [remap kill-ring-save] #'xenops-copy-at-point)

    (dolist (cmd '(delete-char delete-backward-char kill-line kill-region))
      (define-key xenops-rendered-element-keymap `[remap ,cmd] #'xenops-delete-at-point))

    (define-key xenops-rendered-element-keymap "+" #'xenops-increase-size)
    (define-key xenops-rendered-element-keymap "-" #'xenops-decrease-size)
    (define-key xenops-rendered-element-keymap "0" #'xenops-reset-size)
    (define-key xenops-rendered-element-keymap "r" #'xenops-rotate)
    (define-key xenops-rendered-element-keymap "o" #'xenops-save)

    (define-key xenops-rendered-element-keymap [(double-down-mouse-1)] #'xenops-reveal-at-point)

    (xenops-util-define-key-with-fallback [(super v)] #'xenops-handle-paste "\C-y")
    (xenops-util-define-key-with-fallback "\C-y" #'xenops-handle-paste)
    (xenops-util-define-key-with-fallback "\"" #'xenops-insert-quote)

    (xenops-font-activate)
    (xenops-math-activate)
    (xenops-auctex-activate)
    (xenops-xen-mode +1)
    (xenops-font-lock-activate)

    ;; Display math and tables as images
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
    (xenops-auctex-deactivate)
    (xenops-xen-mode -1))))

(defun xenops-dwim (&optional arg)
  (interactive "P")
  (cond
   ((equal arg '(16))
    (xenops-regenerate))
   ((equal arg '(4))
    (xenops-reveal))
   (t (xenops-apply-handlers
       (append (xenops-ops-get 'render :handlers)
               (xenops-ops-get 'execute :handlers))))))

(defvar xenops-ops
  '((render
     . ((:handlers . (xenops-math-render
                      xenops-image-render
                      xenops-text-footnote-render))))
    (reveal
     . ((:handlers . (xenops-math-reveal
                      xenops-image-reveal
                      xenops-element-reveal))))
    (regenerate
     . ((:handlers . (xenops-math-regenerate))))
    (copy
     . ((:handlers . (xenops-element-copy))))
    (delete
     . ((:handlers . (xenops-element-delete))))
    (increase-size
     . ((:handlers . (xenops-math-image-increase-size
                      xenops-image-increase-size))))
    (decrease-size
     . ((:handlers . (xenops-math-image-decrease-size
                      xenops-image-decrease-size))))
    (reset-size
     . ((:handlers . (xenops-math-image-reset-size))))
    (rotate
     . ((:handlers . (xenops-image-rotate))))
    (save
     . ((:handlers . (xenops-image-save))))
    (execute
     . ((:handlers . (xenops-src-execute)))))
  "Element-specific operation handlers grouped by operation type.")

(defvar xenops-elements
  `((block-math
     .  ((:delimiters . (("^[ \t]*\\\\begin{align\\*?}"
                          "^[ \t]*\\\\end{align\\*?}")))
         (:parser . xenops-math-parse-block-element-at-point)
         (:handlers . (xenops-math-render
                       xenops-math-regenerate
                       xenops-math-reveal
                       xenops-math-image-increase-size
                       xenops-math-image-decrease-size
                       xenops-math-image-reset-size
                       xenops-element-copy
                       xenops-element-delete))))
    (inline-math
     . ((:delimiters . (("\\$" "\\$")))
        (:font-lock-keywords . (((((0 (xenops-math-inline-math-font-lock-handler)))))))
        (:parser . xenops-math-parse-inline-element-at-point)
        (:handlers . block-math)))
    (table
     . ((:delimiters . (("^[ \t]*\\\\begin{table}"
                         "^[ \t]*\\\\end{table}")
                        ("^[ \t]*\\\\begin{tabular}"
                         "^[ \t]*\\\\end{tabular}")))
        (:parser . xenops-math-parse-table-at-point)
        (:handlers . block-math)))
    (image
     . ((:delimiters . (("[ \t]*\\\\includegraphics\\(\\[[^]]+\\]\\)?{\\([^}]+\\)}")))
        (:parser . xenops-image-parse-at-point)
        (:handlers . (xenops-image-render
                      xenops-image-reveal
                      xenops-image-increase-size
                      xenops-image-decrease-size
                      xenops-element-copy
                      xenops-element-delete
                      xenops-image-rotate
                      xenops-image-save))))
    (footnote
     . ((:delimiters . ((,(concat "\\\\footnote"
                                  xenops-brace-delimited-multiline-expression-regexp))))
        (:parser . xenops-text-footnote-parse-at-point)
        (:handlers .(xenops-text-footnote-render
                     xenops-element-reveal
                     xenops-element-copy
                     xenops-element-delete))))
    (minted
     . ((:delimiters . (("^[ \t]*\\\\begin{minted}\\({\\([^}]+\\)}[ \t]*\\)?\\([^\n]*\\)"
                         "^[ \t]*\\\\end{minted}")))
        (:font-lock-keywords . src)
        (:parser . xenops-minted-parse-at-point)
        (:handlers . (xenops-src-execute))))
    (src
     . ((:delimiters . (("^[ \t]*#\\+begin_src[ \t]+\\([^ \t\n]+\\)"
                         "^[ \t]*#\\+end_src")))
        (:font-lock-keywords . (((((0 (xenops-src-apply-syntax-highlighting)))))))
        (:parser . xenops-src-parse-at-point)
        (:handlers . (xenops-src-execute)))))
  "Element-specific operation handlers, regexps, and parsers, grouped by element type.")

(defun xenops-font-lock-activate ()
  "Configure font-lock for all element types by adding entries to
`font-lock-keywords`."
  (cl-loop for (type . _) in xenops-elements
           do
           (-if-let* ((keywords (xenops-elements-get type :font-lock-keywords)))
               (cl-loop for (regexps keywords) in (-zip (xenops-elements-get type :delimiters)
                                                        keywords)
                        do
                        (cl-loop for (regexp keyword) in (-zip regexps keywords)
                                 do
                                 (font-lock-add-keywords nil `((,regexp ,keyword))))))))

(defun xenops-ops-get (type key)
  "Return the value associated with KEY for operation type TYPE."
  (cdr (assq key (cdr (assq type xenops-ops)))))

(defun xenops-render-if-cached ()
  (let ((fn (symbol-function 'xenops-math-render)))
    (cl-letf (((symbol-function 'xenops-math-render)
               (lambda (element) (funcall fn element 'cached-only))))
      (xenops-render))))

(defun xenops-handle-paste ()
  (interactive)
  (or (xenops-math-handle-paste)
      (xenops-image-handle-paste)
      (xenops-handle-paste-default)))

(defun xenops-handle-paste-default ()
  (let ((pos (point)))
    (call-interactively #'yank)
    (unless (xenops-apply-parse-at-point)
      (save-excursion
        (push-mark (point) t t)
        (goto-char pos)
        (xenops-render)))
    t))

(defun xenops-insert-quote ()
  (interactive)
  (if (xenops-apply-parse-at-point)
      (insert "\"")))

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
             (when element (xenops-element-copy element)))
           (when element
             (save-excursion (xenops-math-paste))))))
    (xenops-avy-do-at-math)))

(defun xenops-avy-do-at-math ()
  (avy-jump (xenops-elements-delimiter-start-regexp '(block-math inline-math))))

(provide 'xenops)
