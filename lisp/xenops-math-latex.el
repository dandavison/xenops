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

(defvar xenops-math-latex-documentclass-honored-options
  '("leqno" "reqno" "fleqno")
  "List of strings which, if they occur as documentclass options,
  will be passed to the \\documentclass used when render
  individual math elements.

By default, Xenops only honors \"leqno\", \"reqno\", and
\"fleqno\". In that case, for example, if your document starts
with this line:

\\documentclass[12pt,a4paper,reqno]{amsart}

 Then the \\documentclass that will be used when processing
 individual math elements will look be

\\documentclass[reqno]{article}
")

(setq xenops-math-latex-tasks-semaphore-value-copy nil)

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
                 (org-format-latex-header org-latex-packages-alist org-latex-default-packages-alist)
                 (xenops-math-latex-get-org-format-latex-header-variables)
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

;; (let ((s "\\documentclass[12pt,a4paper,reqno]{amsart}"))
;;   (and (string-match "\\\\documentclass\\[\\([^]]+\\)\\]"  s)
;;        (substring-no-properties (match-string 1 s))))

(defun xenops-math-latex-get-org-format-latex-header-variables ()
  "Return list of variables used by `org-latex-make-preamble'.

The returned list supplies the value of
\(`org-format-latex-header' `org-latex-packages-alist' `org-latex-default-packages-alist'\).

We assume that the first line of `org-format-latex-header' is the \\documentclass."
  (cl-destructuring-bind
      (documentclass . packages)
      (xenops-math-latex-get-preamble-lines)
    (let* ((documentclass-options
            (if (string-match "\\\\documentclass\\[\\([^]]+\\)\\]" documentclass)
                (-intersection xenops-math-latex-documentclass-honored-options
                               (-map #'s-trim (s-split "," (match-string 1 documentclass))))))
           (documentclass
            (format "\\documentclass[%s]{article}" (s-join "," documentclass-options)))
           (latex-header
            (s-join "\n" (cons documentclass (cdr (s-split "\n" org-format-latex-header))))))
      (list latex-header packages nil))))

(defun xenops-math-latex-make-commands (element dir tex-file dvi-file svg-file)
  "Construct the external process invocations used to convert a single LaTeX fragment to SVG."
  (let* ((processing-type 'dvisvgm)
         (processing-info (cdr (assq processing-type org-preview-latex-process-alist)))
         (dpi (* (org--get-display-dpi)
                 (car (plist-get processing-info :image-size-adjust))
                 xenops-math-image-scale-factor))
         (scale (/ dpi 140))
         (bounding-box (if (eq 'inline-math (plist-get element :type)) 1 10)))
    `(("latex" "-shell-escape" "-interaction" "nonstopmode" "-output-directory" ,dir ,tex-file)
      ("dvisvgm" ,dvi-file
       "-n"
       "-b" ,(number-to-string bounding-box)
       "-c" ,(number-to-string scale)
       "-o" ,svg-file))))

(aio-defun xenops-math-latex-create-image (element latex image-type colors cache-file display-image)
  "Process LaTeX string to SVG via external processes, asynchronously."
  (let ((buffer (current-buffer)))
    (aio-await (aio-sem-wait xenops-math-latex-tasks-semaphore))
    (with-current-buffer buffer
      (xenops-element-create-marker element))
    (let* ((dir temporary-file-directory)
           (base-name (f-base cache-file))
           (make-file-name (lambda (ext) (f-join dir (concat base-name ext))))
           (tex-file (funcall make-file-name ".tex"))
           (dvi-file (funcall make-file-name ".dvi"))
           (svg-file (funcall make-file-name ".svg"))
           (commands (xenops-math-latex-make-commands element dir tex-file dvi-file svg-file)))
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
            (aio-await (aio-with-async (copy-file svg-file cache-file 'replace)))
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
      (split-string
       (buffer-substring (progn
                           (re-search-forward "\\\\documentclass.+$")
                           (match-beginning 0))
                         (progn (search-forward "\\begin{document}")
                                (match-beginning 0)))
       "\n" t "[ \t\n]+"))))

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
