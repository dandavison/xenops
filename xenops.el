;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'org)
(require 'subr-x)

(require 'aio)
(require 'avy)
(require 'dash)
(require 'dash-functional)
(require 'f)
(require 'preview)
(require 's)

(require 'xenops-apply)
(require 'xenops-auctex)
(require 'xenops-compat)
(require 'xenops-doctor)
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

(defvar xenops-font-family nil
  "The font family used for all text other than math and source
  code elements in a Xenops buffer. To make this take effect,
  restart `xenops-mode'. You can use `xenops-select-font-family'
  to try out different fonts interactively.")

(defvar xenops-mode-map (make-sparse-keymap)
  "The main Xenops keymap. Xenops is a minor-mode, so it is not
  allowed to override major-mode keybindings. Therefore, all
  Xenops commands are bound in `xenops-secondary-keymap'. A few
  important commands are also made available in this keymap.")

(defvar xenops-secondary-keymap (make-sparse-keymap)
  "All Xenops commands are available in this keymap.")

(defvar xenops-rendered-element-keymap (make-sparse-keymap)
  "A keymap that is active when point is on a rendered element,
  such as a math/table image.")

(defvar xenops-tooltip-delay 0.2
  "`tooltip-delay' when xenops-mode is active.")

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

(xenops-define-apply-at-point-command render
                                      "Render the element at point.")

(xenops-define-apply-at-point-command reveal
                                      "Reveal the element at point.")

(xenops-define-apply-at-point-command regenerate
                                      "Regenerate the element at point.")

(xenops-define-apply-at-point-command copy
                                      "Copy the element at point.")

(xenops-define-apply-at-point-command delete
                                      "Delete the element at point.")

(xenops-define-apply-at-point-command execute
                                      "Execute the org-src code block at point.")

(defalias 'xenops-execute #'xenops-execute-at-point)

(define-minor-mode xenops-mode
  "A LaTeX editing environment.

\\{xenops-mode-map}"
  :lighter " Xenops"
  (cond

   (xenops-mode
    (-if-let* ((problems (xenops-doctor 'quiet)))
        (error problems))
    (xenops-define-keys)
    (xenops-font-activate)
    (xenops-math-activate)
    (xenops-auctex-activate)
    (xenops-font-lock-activate)
    (save-excursion
      (goto-char (point-min))
      (xenops-render-if-cached)))

   ('deactivate
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (xenops-reveal)
        ;; Ensure overlays are deleted e.g. for malformed elements.
        (xenops-overlay-delete-overlays)))
    (xenops-math-deactivate)
    (xenops-auctex-deactivate)
    (xenops-xen-mode -1))))

(defun xenops-define-keys ()
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Keymaps-and-Minor-Modes.html
  ;;
  ;; Minor modes may bind commands to key sequences consisting of C-c followed by a punctuation
  ;; character. However, sequences consisting of C-c followed by one of {}<>:; or a control
  ;; character or digit, are reserved for major modes. Also, C-c letter is reserved for users.

  ;; Top-level bindings
  (cl-loop for (key . cmd) in `(("," . ,xenops-secondary-keymap)
                                ("!" . xenops-dwim)
                                ("/" . xenops-xen-mode))
           do
           (define-key xenops-mode-map (concat "\C-c" key) cmd))

  ;; Sub-keymap bound to C-c,
  (cl-loop for (key . cmd) in `(("!" . xenops-dwim)
                                ("/" . xenops-xen-mode)
                                (,(kbd "s-+") . xenops-increase-size)
                                (,(kbd "s--") . xenops-decrease-size)
                                (,(kbd "s-=") . xenops-increase-size)
                                (,(kbd "s-_") . xenops-decrease-size))
           do
           (define-key xenops-secondary-keymap key cmd))

  ;; Keymap for rendered element overlays
  (cl-loop for (key . cmd) in '(([(return)] . xenops-reveal-at-point)
                                ([remap kill-ring-save] . xenops-copy-at-point)
                                ("+" . xenops-increase-size)
                                ("-" . xenops-decrease-size)
                                ("=" . xenops-increase-size)
                                ("_" . xenops-decrease-size)
                                ([(double-down-mouse-1)] . (lambda () (interactive) nil))
                                ([(double-mouse-1)] . xenops-reveal-at-point))
           do
           (define-key xenops-rendered-element-keymap key cmd))

  (dolist (cmd '(delete-char delete-backward-char kill-line kill-region))
    (define-key xenops-rendered-element-keymap `[remap ,cmd] #'xenops-delete-at-point))

  (xenops-util-define-key-with-fallback [(super v)] #'xenops-handle-paste "\C-y")
  (xenops-util-define-key-with-fallback "\C-y" #'xenops-handle-paste)
  (xenops-util-define-key-with-fallback "\"" #'xenops-insert-quote))

(defun xenops-dwim (&optional arg)
  "Operate on the element at point, if there is one, or on the whole buffer.

By default this command displays LaTeX math/tables/TikZ as
images, or executes the code block at point. This is equivalent
to using `xenops-render' or `xenops-execute'.

With a single C-u prefix argument, it reveals the element at
point for editing (removes the image). This is equivalent to
`xenops-reveal'.

With two C-u prefix arguments, it regenerates the image (i.e.
re-runs LaTeX, refusing to use a cached image). This is
equivalent to `xenops-regenerate'.
"
  (interactive "P")
  (cond
   ((equal arg '(16))
    (or (xenops-regenerate-at-point)
        (xenops-regenerate)))
   ((equal arg '(4))
    (xenops-reveal))
   (t (or (xenops-apply-at-point '(render execute))
          (xenops-render)))))

(defvar xenops-ops
  '((render
     . ((:handlers . (xenops-math-render
                      xenops-image-render
                      xenops-footnote-render))))
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
    (execute
     . ((:handlers . (xenops-src-execute)))))
  "Element-specific operation handlers grouped by operation type.")

(defvar xenops-elements
  `((block-math
     .  ((:delimiters . (("^[ \t]*\\\\begin{\\(align\\|equation\\|tikzpicture\\)\\*?}"
                          "^[ \t]*\\\\end{\\(align\\|equation\\|tikzpicture\\)\\*?}")))
         (:parser . xenops-math-parse-block-element-at-point)
         (:handlers . (xenops-math-render
                       xenops-math-regenerate
                       xenops-math-reveal
                       xenops-math-image-increase-size
                       xenops-math-image-decrease-size
                       xenops-element-copy
                       xenops-element-delete))))
    (inline-math
     . ((:delimiters . (("\\$" "\\$")
                        ("\\\\(" "\\\\)")))
        (:font-lock-keywords . (((((0 (xenops-math-inline-math-font-lock-handler)))))
                                ((((0 (xenops-math-inline-math-font-lock-handler)))))))
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
                      xenops-element-delete))))
    (footnote
     . ((:delimiters . ((,(concat "\\\\footnote"
                                  xenops-brace-delimited-multiline-expression-regexp))))
        (:parser . xenops-footnote-parse-at-point)
        (:handlers .(xenops-footnote-render
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

(defun xenops-get (data type key)
  "Return the value associated with KEY for entry TYPE in DATA."
  (let ((value (cdr (assq key (cdr (assq type data))))))
    (if (and (symbolp value) (assq value data))
        ;; Instead of a real value, a type may name another type, meaning: use that type's entry.
        (xenops-get data value key)
      value)))

(defun xenops-get-for-types (data types key)
  "Concatenated list of all items under key KEY for any type in
TYPES. If TYPES is 'all, then all items under key KEY for any
type."
  (-uniq
   (apply #'append
          (cl-loop for (type _) in data
                   collecting (and (or (eq types 'all) (memq type types))
                                   (let ((val (xenops-get data type key)))
                                     (if (listp val) val (list val))))))))

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

(defun xenops-ops-get (op key)
  "The value associated with KEY for operation OP."
  (xenops-get xenops-ops op key))

(defun xenops-ops-get-for-ops (ops key)
  "Concatenated list of values associated with KEY for operations OPS."
  (xenops-get-for-types xenops-ops ops key))

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
