;;; xenops-math-latex.el --- Asynchronous processing of LaTeX fragments to SVG -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'xenops-aio)

(defvar-local xenops-math-latex-tasks-semaphore nil)

(defvar xenops-math-latex-max-tasks-in-flight 32
  "The maximum number of latex processing tasks that are permitted
to be simultaneously active. Your operating system will schedule
these processes onto available CPU cores. Any other waiting
Xenops tasks will remain in the Xenops task queue until one of
the active tasks completes.")

(setq xenops-math-latex-tasks-semaphore-value-copy nil)

(defvar xenops-math-latex-excluded-preamble-line-regexps
  '("^\\\\numberwithin")
  "List of preamble-line exclusion regular expressions.

Any preamble line in the original document that matches one of
these will be excluded when constructing the LaTeX document for
individual math elements.")

(defun xenops-math-latex-make-latex-document (latex colors)
  "Make the LaTeX document for a single math image."
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

(defun xenops-math-latex-make-commands (element dir tex-file dvi-file img-file)
  "Construct the external process invocations used to convert a single LaTeX fragment to SVG."
  ;; See `org-preview-latex-process-alist'
  (let* ((image-type (xenops-math-image-type))
         (processing-info (cdr (assq xenops-math-dvi-to-image-process org-preview-latex-process-alist)))
         (dpi (* (org--get-display-dpi)
                 (car (plist-get processing-info :image-size-adjust))
                 xenops-math-image-scale-factor))
         (scale (/ dpi 140))
         (bounding-box (if (eq 'inline-math (plist-get element :type)) 1 10))
         (latex-command
          `("latex" "-shell-escape" "-interaction" "nonstopmode" "-output-directory" ,dir ,tex-file))
         (dvi-to-image-command
          (cond
           ((string-equal image-type "svg")
            `("dvisvgm" ,dvi-file
              "-n"
              "-b" ,(number-to-string bounding-box)
              "-c" ,(number-to-string scale)
              "-o" ,img-file))
           ((string-equal image-type "png")
            `("dvipng"
              "-D" ,(number-to-string dpi)
              "-T" "tight"
              "-o" ,img-file
              ,dvi-file))
           (t (error "Invalid image type: %S" image-type)))))
    (list latex-command dvi-to-image-command)))

(aio-defun xenops-math-latex-create-image (element latex colors cache-file display-image)
  "Process LaTeX string to image via external processes, asynchronously."
  (let ((buffer (current-buffer)))
    (aio-await (aio-sem-wait xenops-math-latex-tasks-semaphore))
    (with-current-buffer buffer
      (xenops-element-create-marker element))
    (let* ((dir temporary-file-directory)
           (base-name (f-base cache-file))
           (make-file-name (lambda (ext) (f-join dir (concat base-name "." ext))))
           (tex-file (funcall make-file-name ".tex"))
           (dvi-file (funcall make-file-name ".dvi"))
           (img-file (funcall make-file-name (xenops-math-image-type)))
           (commands (xenops-math-latex-make-commands element dir tex-file dvi-file img-file)))
      (condition-case error
          (progn
            (aio-await
             (xenops-aio-with-async-with-buffer
              buffer
              (let ((latex-document (xenops-math-latex-make-latex-document latex colors)))
                (with-temp-file tex-file
                  (insert latex-document)))))
            (dolist (command commands)
              (aio-await (xenops-aio-subprocess command)))
            (aio-await (aio-with-async (copy-file img-file cache-file 'replace)))
            (aio-await
             (xenops-aio-with-async-with-buffer
              buffer
              (-if-let* ((marker (plist-get element :begin-marker))
                         (element (xenops-math-parse-element-at marker)))
                  (funcall display-image element commands)
                (if marker (message "Failed to parse element at marker: %S" marker)
                  (message "Expected element to have marker: %S" element)))))
            (xenops-element-deactivate-marker element))
        (error (aio-await
                (xenops-aio-with-async-with-buffer
                 buffer
                 (-when-let* ((element (xenops-math-parse-element-at (plist-get element :begin-marker))))
                   (xenops-math-display-error element error)
                   (xenops-element-deactivate-marker element))))))
      (with-current-buffer buffer
        (aio-sem-post xenops-math-latex-tasks-semaphore)))))

(defun xenops-math-latex-waiting-tasks-count ()
  "Return the number of processing tasks currently waiting in the queue."
  (when xenops-mode
    (- xenops-math-latex-max-tasks-in-flight
       (aref xenops-math-latex-tasks-semaphore 1))))

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
  (sha1 (prin1-to-string (list (buffer-file-name) TeX-master))))

(defun xenops-math-latex-get-preamble-lines ()
  "Return preamble lines used for the LaTeX document used to render a single math element.

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
  (setq xenops-math-latex-preamble-cache
        (assoc-delete-all (xenops-math-latex-make-preamble-cache-key)
                          xenops-math-latex-preamble-cache)))

(defun xenops-math-latex-pre-apply-copy-semaphore-value (handlers &rest args)
  "Copy current semaphore value to a global variable.

This allows the number of started tasks to be shown by
`xenops-math-latex-post-apply-show-started-tasks'."
  (if (memq 'xenops-math-render handlers)
      (setq xenops-math-latex-tasks-semaphore-value-copy
            (aref xenops-math-latex-tasks-semaphore 1))))

(defun xenops-math-latex-post-apply-show-started-tasks (handlers &rest args)
  "Show number of asynchronous processing tasks started by `xenops-render'."
  (if (memq 'xenops-math-render handlers)
      (message "Started %d latex processing tasks"
               (- xenops-math-latex-tasks-semaphore-value-copy
                  (aref xenops-math-latex-tasks-semaphore 1)))))

(add-hook 'xenops-apply-pre-apply-hook #'xenops-math-latex-pre-apply-copy-semaphore-value)
(add-hook 'xenops-apply-post-apply-hook #'xenops-math-latex-post-apply-show-started-tasks)

(provide 'xenops-math-latex)

;;; xenops-math-latex.el ends here
