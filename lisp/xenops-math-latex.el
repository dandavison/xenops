;;; xenops-math-latex.el --- Asynchronous processing of LaTeX fragments to SVG -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:
(require 'org)

(require 'aio)
(require 'dash)
(require 'f)
(require 'tex)
(require 'tex-buf nil t)

(require 'xenops-aio)

(declare-function xenops-math-deactivate-marker-on-element "xenops-math")
(declare-function xenops-math-display-error-badge "xenops-math")
(declare-function xenops-math-parse-element-at "xenops-math")
(declare-function xenops-math-set-marker-on-element "xenops-math")
(declare-function xenops-png-set-phys-chunk "xenops-png")

(defvar xenops-math-latex-process 'dvisvgm
  "The process used for producing images from LaTeX fragments.

Possible values are

'dvisvgm     - LaTeX DVI output is converted to SVG and Emacs displays the SVG.
'imagemagick - LaTeX PDF output is converted to PNG and Emacs displays the PNG.
'dvipng      - LaTeX DVI output is converted to PNG and Emacs displays the PNG.

See `xenops-math-latex-process-alist' for specifications of
the commands used to run these processes.")

(defvar xenops-math-latex-process-alist
  '((dvipng
     :programs ("latex" "dvipng")
     :description "dvi > png"
     :message "you need to install the programs: latex and dvipng."
     :image-input-type "dvi"
     :image-output-type "png"
     :image-size-adjust (1.0 . 1.0)
     :latex-compiler ("latex -interaction nonstopmode -shell-escape -output-format dvi -output-directory %o %f")
     :image-converter ("dvipng -D %D -T tight -o %O %f"))
    (dvisvgm
     :programs ("latex" "dvisvgm")
     :description "dvi > svg"
     :message "you need to install the programs: latex and dvisvgm."
     :image-input-type "dvi"
     :image-output-type "svg"
     :image-size-adjust (1.7 . 1.5)
     :latex-compiler ("latex -interaction nonstopmode -shell-escape -output-format dvi -output-directory %o %f")
     :image-converter ("dvisvgm %f -n -b %B -c %S -o %O"))
    (imagemagick
     :programs ("latex" "convert")
     :description "pdf > png"
     :message "you need to install the programs: latex and imagemagick."
     :image-input-type "pdf"
     :image-output-type "png"
     :image-size-adjust (1.0 . 1.0)
     :latex-compiler ("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f")
     :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O")))
  "Definitions of external processes for LaTeX previewing.

See the documentation of Org-mode variable `org-preview-latex-process-alist'.
This variable plays exactly the same role for Xenops.")

(defvar-local xenops-math-latex-tasks-semaphore nil)

(defvar xenops-math-latex-max-tasks-in-flight 32
  "Maximum number of latex processing tasks allowed to be simultaneously active.

Your operating system will schedule these processes onto
available CPU cores. Any other waiting Xenops tasks will remain
in the Xenops task queue until one of the active tasks
completes.")

(defvar xenops-math-latex-tasks-semaphore-value-copy nil)

(defvar xenops-math-latex-excluded-preamble-line-regexps
  '("^\\\\numberwithin")
  "List of preamble-line exclusion regular expressions.

Any preamble line in the original document that matches one of
these will be excluded when constructing the LaTeX document for
individual math elements.")

;; Silence compiler: defined elsewhere
(defvar xenops-mode)
(defvar xenops-math-image-scale-factor)
(defvar xenops-math-latex-create-image)

(defun xenops-math-latex-make-latex-document (latex colors)
  "Make the LaTeX document for a single math image.

LATEX is the string of LaTeX code. COLORS is a pair (FG, BG)
containing the foreground and background colors."
  (cl-flet ((get-latex-header () (org-latex-make-preamble
                                  (org-export-get-environment (org-export-get-backend 'latex))
                                  org-format-latex-header
                                  'snippet)))
    (let ((latex-header
           (if (eq major-mode 'org-mode)
               (get-latex-header)
             (cl-destructuring-bind
                 (org-latex-packages-alist org-latex-default-packages-alist)
                 (list (cdr (xenops-math-latex-get-preamble-lines)) nil)
               (get-latex-header)))))
      (cl-destructuring-bind (fg bg) colors
        (concat latex-header
                "\n\\begin{document}\n"
                "\\definecolor{fg}{rgb}{" fg "}\n"
                "\\definecolor{bg}{rgb}{" bg "}\n"
                "\n\\pagecolor{bg}\n"
                "\n{\\color{fg}\n"
                latex
                "\n}\n"
                "\n\\end{document}\n")))))

(defun xenops-math-latex-make-commands (element dir tex-file image-input-file image-output-file)
  "Construct external process invocations used to render a LaTeX fragment.

ELEMENT is the latex element to be rendered. DIR is a directory
to hold output. TEX-FILE is a file to which the LaTeX code will
be written. IMAGE-INPUT-FILE is a file to which the LaTeX
compilation output (.dvi) will be written IMAGE-OUTPUT-FILE is
the name of the file to contain the output image."
  ;; See `org-preview-latex-process-alist'
  (let* ((dpi (xenops-math-latex-calculate-dpi))
         (bounding-box (if (eq 'inline-math (plist-get element :type)) 1 10))
         (format-data
          `((?o . ,dir)
            (?B . ,(number-to-string bounding-box))
            (?D . ,(number-to-string dpi))
            (?S . ,(number-to-string (/ dpi 140))))))
    (append (xenops-math-latex-format-commands (xenops-math-latex-process-get :latex-compiler)
                                               tex-file image-input-file format-data)
            (xenops-math-latex-format-commands (xenops-math-latex-process-get :image-converter)
                                               image-input-file image-output-file format-data))))

(defun xenops-math-latex-calculate-dpi ()
  "Calculate DPI to be used during fragment image generation."
  (* (org--get-display-dpi)
     (car (xenops-math-latex-process-get :image-size-adjust))
     xenops-math-image-scale-factor))

(defun xenops-math-latex-process-get (key)
  "Return the value of KEY in `xenops-math-latex-process-alist' for `xenops-math-latex-process'."
  (plist-get (cdr (assq xenops-math-latex-process xenops-math-latex-process-alist)) key))

(defun xenops-math-latex-format-commands (command-templates input-file output-file format-data)
  "Return a formatted command as a list of strings, suitable for `make-process'.

COMMAND-TEMPLATES are formatting templates for the commands.
FORMAT-DATA, INPUT-FILE, and OUTPUT-FILE contain data used to
format the commands."
  (let ((format-data (append format-data `((?f . ,input-file)
                                           (?O . ,output-file)))))
    (mapcar (lambda (template)
              (--map (format-spec it format-data) (s-split " " template)))
            command-templates)))

(aio-defun xenops-math-latex-create-image (element latex colors cache-file display-image)
  "Process LaTeX string to image via external processes, asynchronously."
  (let ((buffer (current-buffer)))
    (aio-await (aio-sem-wait xenops-math-latex-tasks-semaphore))
    (with-current-buffer buffer
      (xenops-math-set-marker-on-element element))
    (let* ((dir (f-join temporary-file-directory "xenops"))
           (base-name (f-base cache-file))
           (make-file-name (lambda (ext) (f-join dir (concat base-name "." ext))))
           (tex-file (funcall make-file-name "tex"))
           (image-input-file (funcall make-file-name (xenops-math-latex-process-get :image-input-type)))
           (image-output-file (funcall make-file-name  (xenops-math-latex-process-get :image-output-type)))
           (commands (xenops-math-latex-make-commands element dir tex-file image-input-file image-output-file)))
      (condition-case error
          (progn
            (make-directory dir 't)
            (aio-await
             (xenops-aio-with-async-with-buffer
              buffer
              (let ((latex-document (xenops-math-latex-make-latex-document latex colors)))
                (with-temp-file tex-file
                  (insert latex-document)))))
            (dolist (command commands)
              (aio-await (xenops-aio-subprocess command)))
            (aio-await (aio-with-async
                         (if (and (eq (xenops-math-latex-process-get :image-output-type) "png")
                                  (xenops-math-latex-process-get :image-output-ppi))
                             (let ((png-bytes (xenops-png-set-phys-chunk
                                               (f-read-bytes image-output-file)
                                               (xenops-math-latex-process-get :image-output-ppi))))
                               (f-write-bytes png-bytes cache-file))
                           (copy-file image-output-file cache-file 'replace))))
            (aio-await
             (xenops-aio-with-async-with-buffer
              buffer
              (-if-let* ((marker (plist-get element :begin-marker))
                         (element (xenops-math-parse-element-at marker)))
                  (funcall display-image element commands)
                (if marker (message "Failed to parse element at marker: %S" marker)
                  (message "Expected element to have marker: %S" element)))))
            (xenops-math-deactivate-marker-on-element element))
        (error (aio-await
                (xenops-aio-with-async-with-buffer
                 buffer
                 (-when-let* ((element (xenops-math-parse-element-at (plist-get element :begin-marker))))
                   (xenops-math-display-error-badge
                    element error
                    ;; Only display the error if a single element is being rendered.
                    (and (not xenops-apply-user-point)
                         (<= (xenops-math-latex-waiting-tasks-count) 0)))
                   (xenops-math-deactivate-marker-on-element element))))))
      (with-current-buffer buffer
        (aio-sem-post xenops-math-latex-tasks-semaphore)))))

(defun xenops-math-latex-waiting-tasks-count ()
  "Return the number of processing tasks currently waiting in the queue."
  (if xenops-mode
      (- xenops-math-latex-max-tasks-in-flight
         (aref xenops-math-latex-tasks-semaphore 1))
    0))

(defun xenops-show-waiting-tasks ()
  "Display number of waiting latex processing tasks."
  (interactive)
  (when xenops-mode
    (message "%S latex processing tasks waiting" (xenops-math-latex-waiting-tasks-count))))

(defun xenops-cancel-waiting-tasks ()
  "Cancel waiting latex processing tasks."
  (interactive)
  (when xenops-mode
    (xenops-aio-sem-cancel-waiting-tasks xenops-math-latex-tasks-semaphore
                                         xenops-math-latex-max-tasks-in-flight)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (if (eq (overlay-get ov 'xenops-overlay-type) 'xenops-math-waiting)
          (delete-overlay ov)))))

(defun xenops-math-latex-get-colors ()
  "Return the foreground and background colors to be used for SVG images of LaTeX content."
  (let* ((face (face-at-point))
         (fg
          (let ((color (plist-get org-format-latex-options :foreground)))
            (if (eq color 'auto)
                (and face (face-attribute face :foreground nil 'default))
              color)))
         (bg
          (let ((color (plist-get org-format-latex-options :background)))
            (if (eq color 'auto)
                (and face (face-attribute face :background nil 'default))
              color)))

         (fg (or fg "Black"))
         (bg (or bg "Transparent"))

         (fg (if (eq fg 'default)
                 (org-latex-color :foreground)
               (org-latex-color-format fg)))
         (bg (if (eq bg 'default)
                 (org-latex-color :background)
               (org-latex-color-format
                (if (string= bg "Transparent") "white" bg)))))
    (list fg bg)))

(defvar xenops-math-latex-preamble-cache nil
  "Internal cache for per-file LaTeX preamble.")

(defun xenops-math-latex-make-preamble-cache-key ()
  "Return key for the current file in the LaTeX preamble cache."
  (sha1 (prin1-to-string (list (buffer-file-name) TeX-master))))

(defun xenops-math-latex-get-preamble-lines ()
  "Return LaTeX preamble lines used when rendering a single math element.

The first element of the returned list is the \\documentclass;
subsequent elements are \\usepackage lines, macro definitions,
etc."
  (let ((key (xenops-math-latex-make-preamble-cache-key)))
    (unless (assoc key xenops-math-latex-preamble-cache)
      (push (cons key (xenops-math-latex-make-preamble-lines))
            xenops-math-latex-preamble-cache))
    (cdr (assoc key xenops-math-latex-preamble-cache))))

(defun xenops-math-latex-make-preamble-lines ()
  "Make the preamble for a LaTeX document for a single math element."
  (let ((file (make-temp-file "xenops-math-TeX-region-create" nil ".tex")))
    (TeX-region-create file "" (buffer-file-name) 0)
    (with-temp-buffer
      (insert-file-contents file)
      (-remove
       (lambda (line) (--any (string-match it line) xenops-math-latex-excluded-preamble-line-regexps))
       (split-string
        (buffer-substring (progn
                            (re-search-forward "\\\\documentclass.+$")
                            (match-beginning 0))
                          (progn (search-forward "\\begin{document}")
                                 (match-beginning 0)))
        "\n" t "[ \t\n]+")))))

(defun xenops-clear-latex-preamble-cache ()
  "Clear the LaTeX preamble cache."
  (interactive)
  (let ((key (xenops-math-latex-make-preamble-cache-key)))
    (setq xenops-math-latex-preamble-cache
          (--remove (equal (car it) key)
                    xenops-math-latex-preamble-cache))))

(defun xenops-math-latex-pre-apply-copy-semaphore-value (handlers &rest _)
  "Copy current semaphore value to a global variable.

This allows the number of started tasks to be shown by
`xenops-math-latex-post-apply-show-started-tasks'.

HANDLERS are the currently active handlers and are used to
determine whether the action is appropriate."
  (if (memq 'xenops-math-render handlers)
      (setq xenops-math-latex-tasks-semaphore-value-copy
            (aref xenops-math-latex-tasks-semaphore 1))))

(defun xenops-math-latex-post-apply-show-started-tasks (handlers &rest _)
  "Show number of asynchronous processing tasks started by `xenops-render'.

HANDLERS are the currently active handlers and are used to
determine whether the action is appropriate."
  (if (memq 'xenops-math-render handlers)
      (message "Started %d latex processing tasks"
               (- xenops-math-latex-tasks-semaphore-value-copy
                  (aref xenops-math-latex-tasks-semaphore 1)))))

(add-hook 'xenops-apply-pre-apply-hook #'xenops-math-latex-pre-apply-copy-semaphore-value)
(add-hook 'xenops-apply-post-apply-hook #'xenops-math-latex-post-apply-show-started-tasks)

(provide 'xenops-math-latex)

;;; xenops-math-latex.el ends here
