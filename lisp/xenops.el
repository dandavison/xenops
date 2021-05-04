;;; xenops.el --- A LaTeX editing environment for mathematical documents -*- lexical-binding: t; -*-

;; Author: Dan Davison <dandavison7@gmail.com>
;; URL: https://github.com/dandavison/xenops
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.1") (aio "1.0") (auctex "12.2.0") (avy "0.5.0") (dash "2.18.0") (f "0.20.0") (s "1.12.0"))

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The main user actions in Xenops are:
;;
;; xenops-render
;; xenops-reveal
;; xenops-regenerate
;; xenops-execute
;;
;; Those four verbs are examples of *operations*. The `xenops-ops' data structure defines the set
;; of Xenops operations.
;;
;; Operations are done on *elements*. An element is a special substring of the buffer such as a
;; math block, a table, a minted code block, a footnote, an \includegraphics link, etc. The set of
;; Xenops element types is defined by the `xenops-elements' data structure. An element string is
;; parsed into a plist data structure, that we also refer to as an element. The :type key of the
;; plist holds the type of the element (`'block-math`, `'table`, `'minted`, `footnote`, `'image`,
;; etc).
;;
;; The organizing principle of Xenops is that a user action consists of applying an operation to a
;; set of elements. The set of elements is determined by the context under which the action was
;; invoked: either a single element at point, or all elements in the active region, or all elements
;; in the buffer.
;;
;; Xenops carries out such a user action as follows:
;;
;; 1. Identify the set of *handlers* corresponding to the operation. A handler is a function that
;;    takes an element plist as its first argument. The mapping from operations to handlers is
;;    defined in the `xenops-ops' data structure.
;;
;; 2. Visit each element in sequence:
;;
;;    2.1 At an element, select a single handler which is valid for that element type. (There will
;;        usually be only one choice.) The mapping from element types to valid handlers is defined
;;        in the `xenops-elements' data structure.
;;
;;    2.2 Call the selected handler on the element.
;;
;; This traversal and dispatch-to-handler logic is implemented in xenops-apply.el.
;;
;; The handler for the `render` operation on elements of type `block-math`, `inline-math`, and
;; `table` involves calling external latex and dvisvgm processes to generate an SVG image. This is
;; done asynchronously, using emacs-aio.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'subr-x)

(require 'aio)
(require 'avy)
(require 'dash)
(require 'f)
(require 'preview)
(require 's)

(require 'xenops-apply)
(require 'xenops-avy)
(require 'xenops-auctex)
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
(require 'xenops-png)
(require 'xenops-src)
(require 'xenops-util)
(require 'xenops-xen)

(defvar xenops-cache-directory (expand-file-name (f-join "xenops" "cache") user-emacs-directory)
  "Path to a directory in which xenops can save files.")

(defvar xenops-font-family nil
  "Font family used for all text other than math and source code elements.

To make this take effect, use toggle command `xenops-mode'. You
can use `xenops-select-font-family' to try out different fonts
interactively.")

(defvar xenops-mode-map (make-sparse-keymap)
  "The main Xenops keymap.

Xenops is a minor-mode, so it is not allowed to override major
mode keybindings. Therefore, all Xenops commands are bound in
`xenops-secondary-keymap'. A few important commands are also made
available in this keymap.")

(defvar xenops-secondary-keymap (make-sparse-keymap)
  "All Xenops commands are available in this keymap.")

(defvar xenops-rendered-element-keymap (make-sparse-keymap)
  "A keymap that is active when point is on a rendered element, such as a math/table image.")

(defvar xenops-reveal-on-entry nil
  "Should an element be revealed for editing automatically when point moves inside it?")

(defvar xenops-tooltip-delay 0.2
  "The value of the variable `tooltip-delay' when variable `xenops-mode' is active.")

(defmacro xenops-define-apply-command (op docstring)
  "Define an apply command for OP with DOCSTRING."
  `(defun ,(intern (concat "xenops-" (symbol-name op))) ()
     ,(concat docstring "\n\n"
              "The elements operated on are determined by trying the following:
1. The element at point, if any.
2. Elements in the active region, if there is an active region.
3. All elements in the buffer.")
     (interactive)
     (xenops-apply-operations '(,op))))

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

(defmacro xenops-define-apply-at-point-command (op docstring)
  "Define an apply-at-point command for OP with DOCSTRING."
  `(defun ,(intern (concat "xenops-" (symbol-name op) "-at-point")) ()
     ,docstring
     (interactive)
     (xenops-apply-operations-at-point '(,op))))

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

;;;###autoload
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
    (xenops-cancel-waiting-tasks)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (xenops-reveal)))
    (xenops-math-deactivate)
    (xenops-auctex-deactivate)
    (xenops-xen-mode -1))))

(defun xenops-define-keys ()
  "Define key bindings for xenops mode."
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
                                ("c" . xenops-copy-and-paste-element)
                                ("g" . xenops-goto-element)
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
  (xenops-util-define-key-with-fallback "\"" #'xenops-insert-double-quote))

(defun xenops-dwim (&optional arg)
  "Operate on the element at point, if there is one, or on the whole buffer.

By default this command displays LaTeX math/tables/TikZ as
images, or executes the code block at point. This is equivalent
to using `xenops-render' or `xenops-execute'.

With a single prefix argument ARG, it reveals the element at
point for editing (removes the image). This is equivalent to
`xenops-reveal'.

With a double prefix argument ARG, it regenerates the image (i.e.
re-runs LaTeX, refusing to use a cached image). This is
equivalent to `xenops-regenerate'."
  (interactive "P")
  (cond
   ((equal arg '(16))
    (or (xenops-regenerate-at-point)
        (xenops-regenerate)))
   ((equal arg '(4))
    (xenops-reveal))
   (t (or (xenops-apply-operations-at-point '(render execute))
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
     .  ((:delimiters . (("^[ \t]*\\\\begin{\\(align\\|equation\\|tikzpicture\\|gather\\)\\*?}"
                          "^[ \t]*\\\\end{\\(align\\|equation\\|tikzpicture\\|gather\\)\\*?}")
                         ("^[ \t]*\\\\\\["
                          "^[ \t]*\\\\\\]")))
         (:parser . xenops-math-parse-block-element-at-point)
         (:handlers . (xenops-math-render
                       xenops-math-regenerate
                       xenops-math-reveal
                       xenops-math-image-increase-size
                       xenops-math-image-decrease-size
                       xenops-element-copy
                       xenops-element-delete))))
    (inline-math
     . ((:delimiters . (,xenops-math-dollar-delimited-inline-math-delimiters
                        ,xenops-math-paren-delimited-inline-math-delimiters
                        ,xenops-math-square-bracket-delimited-inline-math-delimiters
                        ,xenops-math-tikz-inline-math-delimiters
                        ,xenops-math-environment-delimited-inline-math-delimiters))
        (:font-lock-keywords . (((((0 (xenops-math-inline-math-font-lock-handler)))))
                                ((((0 (xenops-math-inline-math-font-lock-handler)))))
                                ((((0 (xenops-math-inline-math-font-lock-handler)))))
                                ((((0 (xenops-math-inline-math-font-lock-handler)))))
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
  "Concatenated list of all items in DATA under key KEY for any type in TYPES.

If TYPES is 'all, then all items under key KEY for any type."
  (-uniq
   (apply #'append
          (cl-loop for (type _) in data
                   collecting (and (or (eq types 'all) (memq type types))
                                   (let ((val (xenops-get data type key)))
                                     (if (listp val) val (list val))))))))

(defun xenops-dispatch-handlers (handlers element)
  "Call the first handler in HANDLERS that is valid for an element of this type.

ELEMENT is the element whose type is used."
  (-when-let* ((handler (car (-intersection handlers
                                            (xenops-elements-get (plist-get element :type) :handlers)))))
    (funcall handler element)
    t))

(defun xenops-dispatch-operation (op element)
  "Call a valid handler for operation OP on element ELEMENT."
  (xenops-dispatch-handlers (xenops-ops-get op :handlers) element))

(defun xenops-font-lock-activate ()
  "Configure font-lock for element types defined in `xenops-elements'."
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
  "Render elements if they have a cached image available."
  (let ((fn (symbol-function 'xenops-math-render)))
    (cl-letf (((symbol-function 'xenops-math-render)
               (lambda (element) (funcall fn element 'cached-only))))
      (xenops-render))))

(defun xenops-handle-paste ()
  "Handle a paste event, if the clipboard data contain an element that Xenops can paste."
  (interactive)
  (or (xenops-math-handle-paste)
      (xenops-image-handle-paste)
      (xenops-handle-paste-default)))

(defun xenops-handle-paste-default ()
  "Default Xenops paste handler."
  (let ((pos (point)))
    (call-interactively #'yank)
    (unless (xenops-parse-any-element-at-point)
      (save-excursion
        (push-mark (point) t t)
        (goto-char pos)
        (xenops-render)))
    t))

(defun xenops-insert-double-quote ()
  "Insert double-quote specially in a Xenops element."
  (interactive)
  (if (xenops-parse-any-element-at-point)
      (insert "\"")))

(provide 'xenops)

;;; xenops.el ends here
