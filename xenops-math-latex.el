;; -*- lexical-binding: t -*-
(require 'xenops-aio)

(setq xenops-math-latex-max-tasks-in-flight 56)

(defvar-local xenops-math-latex-tasks-semaphore nil)

(defun xenops-math-latex-make-latex-document (latex colors)
  "Make the LaTeX document for a single math image."
  (let* ((org-latex-packages-alist (xenops-math-latex-get-preamble-lines))
         (org-latex-default-packages-alist)
         (latex-header (org-latex-make-preamble
                        (org-export-get-environment (org-export-get-backend 'latex))
                        org-format-latex-header
                        'snippet)))
    (destructuring-bind (fg bg) colors
      (concat latex-header
              "\n\\begin{document}\n"
              "\\definecolor{fg}{rgb}{" fg "}\n"
              "\\definecolor{bg}{rgb}{" bg "}\n"
              "\n\\pagecolor{bg}\n"
              "\n{\\color{fg}\n"
              latex
              "\n}\n"
              "\n\\end{document}\n"))))

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
      (aio-await
       (xenops-aio-with-async-with-buffer
        buffer
        (let ((latex-document (xenops-math-latex-make-latex-document latex colors)))
          (with-temp-file tex-file
            (insert latex-document)))))
      (condition-case error
          (progn
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
                   (xenops-math-latex-display-error element error)
                   (xenops-element-deactivate-marker element))))))
      (with-current-buffer buffer
        (aio-sem-post xenops-math-latex-tasks-semaphore)))))

(defun xenops-math-latex-display-waiting (element)
  "Style a math element to indicate that its processing task is
waiting in the queue to be executed."
  (xenops-element-overlays-delete element)
  (let* ((beg (plist-get element :begin))
         (end (plist-get element :end))
         (ov (xenops-overlay-create beg end)))
    (overlay-put ov 'face `(:background ,(if (eq (frame-parameter nil 'background-mode) 'light)
                                             "OldLace" "#362b2b")))
    (overlay-put ov 'xenops-overlay-type 'xenops-math-latex-waiting)
    (overlay-put ov 'help-echo "Image-generation task in-progress. \
Use `M-x xenops-cancel-waiting-tasks` to make this element editable.") ov))

(defun xenops-math-latex-display-image (element commands help-echo cache-file -image-type)
  "Display SVG image resulting from successful LaTeX compilation."
  (let ((margin (if (eq 'inline-math (plist-get element :type))
                    0 `(,xenops-math-image-margin . 0)))
        (ov (xenops-math-latex-make-overlay element commands help-echo)))
    (overlay-put ov 'display
                 `(image :type ,(intern -image-type)
                         :file ,cache-file :ascent center :margin ,margin)))
  (unless (equal xenops-math-image-current-scale-factor 1.0)
    (xenops-math-image-change-size element xenops-math-image-current-scale-factor)))

(defun xenops-math-latex-display-error (element error)
  "Style a math element to indicate that an error occurred during
execution of its processing task; make error details available
via hover-over text and contextual menu."
  (xenops-element-overlays-delete element)
  (let* ((beg (plist-get element :begin))
         (end (plist-get element :begin-content))
         (ov (xenops-overlay-create beg end))
         (keymap (overlay-get ov 'keymap))
         (error-badge "⚠️")
         help-echo)
    (-if-let* ((error-data (plist-get (cdr error) :xenops-error-data)))
        (cl-destructuring-bind (failing-command failure-description output) error-data
          (let* ((xenops-math-image-overlay-menu
                  (lambda (event)
                    (interactive "e")
                    (popup-menu
                     `("Xenops"
                       ["View failing command output" (xenops-math-latex-display-process-output ,output)]
                       ["Copy failing command" (kill-new ,failing-command)]))
                    event)))
            (setq help-echo (format "External process failure: %s
Right-click on the warning badge to copy the failing command or view its output.

%s"
                                    failure-description
                                    failing-command))
            (define-key keymap [mouse-3] xenops-math-image-overlay-menu)
            ov))
      (setq help-echo (format "External process failure:\n\n%s"
                              (s-join "\n\n" (--map (format "%S" it) (cdr error))))))
    (add-text-properties 0 (length error-badge)
                         `(help-echo ,help-echo keymap ,keymap)
                         error-badge)
    (overlay-put ov 'after-string error-badge)
    (overlay-put ov 'help-echo help-echo)
    ov))

(defun xenops-math-latex-make-overlay (element commands help-echo)
  "Make an overlay used to style a math element and display
images and error information."
  (xenops-element-overlays-delete element)
  (let* ((beg (plist-get element :begin))
         (end (plist-get element :end))
         (ov (xenops-overlay-create beg end))
         (keymap (overlay-get ov 'keymap))
         (xenops-math-image-overlay-menu
          (lambda (event)
            (interactive "e")
            (popup-menu
             `("Xenops"
               ["Edit" (progn (goto-char ,beg) (xenops-reveal-at-point))]
               ["Copy LaTeX command" (xenops-math-latex-copy-latex-command ,ov)]))
            event)))
    (overlay-put ov 'help-echo help-echo)
    (overlay-put ov 'commands commands)
    (set-keymap-parent keymap xenops-rendered-element-keymap)
    (define-key keymap [mouse-3] xenops-math-image-overlay-menu)
    ov))

(defun xenops-math-latex-display-process-output (output)
  "Display external process output OUTPUT in a buffer"
  (let ((buf (get-buffer-create "*Xenops external command output*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert output))
    (display-buffer buf)))

(defun xenops-math-latex-copy-latex-command (overlay)
  "Copy external latex command to clipboard (kill-ring)."
  (let ((latex-command (car (overlay-get overlay 'commands))))
    (kill-new (s-join " " latex-command))))

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
      (if (eq (overlay-get ov 'xenops-overlay-type) 'xenops-math-latex-waiting)
          (delete-overlay ov)))))

(defun xenops-math-latex-get-colors ()
  "Return the foreground and background colors to be used for SVG
images of LaTeX content."
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

(defun xenops-math-latex-get-preamble-lines ()
  "Get the preamble for a LaTeX document for a single math
element."
  (let ((key (sha1 (prin1-to-string (list (buffer-file-name) TeX-master)))))
    (unless (assoc key xenops-math-latex-preamble-cache)
      (push (cons key (xenops-math-latex-make-preamble-lines))
            xenops-math-latex-preamble-cache))
    (cdr (assoc key xenops-math-latex-preamble-cache))))

(defun xenops-math-latex-make-preamble-lines ()
  "Make the preamble for a LaTeX document for a single math
element."
  (let ((file (make-temp-file "xenops-math-TeX-region-create" nil ".tex")))
    (TeX-region-create file "" (buffer-file-name) 0)
    (with-temp-buffer
      (insert-file-contents file)
      (split-string
       (buffer-substring (re-search-forward "\\documentclass.+$")
                         (progn (search-forward "\\begin{document}")
                                (match-beginning 0)))
       "\n" t "[ \t\n]+"))))

(provide 'xenops-math-latex)
