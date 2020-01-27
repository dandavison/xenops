;; -*- lexical-binding: t -*-

;; Terminology
;;
;; | math element   | either an inline element or a block element    |
;; | inline element | inline math delimited by $...$                 |
;; | block element  | e.g. a \begin{align}...\end{align} environment |


(defvar xenops-math-process 'dvisvgm)

(defvar xenops-math-image-change-size-factor 1.1
  "The factor by which the image's size will be changed under
  `xenops-math-image-increase-size' and
  `xenops-math-image-decrease-size'.")

(defvar xenops-math-image-current-scale-factor 1.0
  "The current relative scaling factor of images, i.e. the net
  scale factor resulting from multiple applications of
  `xenops-math-image-increase-size' and
  `xenops-math-image-decrease-size'.")

(defvar xenops-math-image-scale-factor 0.8
  "Scaling factor for SVG math images. This determines the size
  of the image in the image file that is cached on disk.")

(defvar xenops-math-image-margin 20
  "Number of pixels to be used as left margin for non-inline math images")

(setq xenops-math-inline-math-delimiters '("\\$" "\\$"))

(defun xenops-math-font-lock-keywords ()
  `((,(xenops-math-block-delimiter-lines-regexp)
     (0
      (xenops-math-block-math-font-lock-handler)))))

(defun xenops-math-activate ()
  (make-directory xenops-cache-directory t)
  (setq mouse-drag-and-drop-region t)
  (advice-add #'mouse-drag-region :around #'xenops-math-mouse-drag-region-around-advice)
  (advice-add fill-paragraph-function :after #'xenops-math-fill-paragraph-after-advice)
  (font-lock-add-keywords nil (xenops-math-font-lock-keywords))
  (cursor-sensor-mode +1)
  (add-to-list 'fill-nobreak-predicate #'xenops-math-parse-inline-element-at-point))

(defun xenops-math-deactivate ()
  (advice-remove #'mouse-drag-and-drop-region #'xenops-math-mouse-drag-region-around-advice)
  (advice-remove fill-paragraph-function #'xenops-math-fill-paragraph-after-advice)
  (cursor-sensor-mode -1)
  (font-lock-remove-keywords nil (xenops-math-font-lock-keywords)))

(defun xenops-math-render (element &optional cached-only)
  (unless (or (xenops-element-get-image element)
              (string-equal "" (s-trim (buffer-substring (plist-get element :begin-content)
                                                         (plist-get element :end-content)))))
    (let ((latex (buffer-substring-no-properties (plist-get element :begin)
                                                 (plist-get element :end))))
      ;; The name "image-type" is bound by image-mode and this interferes with the closure.
      (let* ((-image-type (plist-get (cdr (assq xenops-math-process
                                                org-preview-latex-process-alist))
                                     :image-output-type))
             (colors (xenops-math-get-latex-colors))
             (cache-file (xenops-math-compute-file-name latex -image-type colors))
             (cache-file-exists? (file-exists-p cache-file))
             (display-image
              (lambda (element &optional commands)
                (xenops-math-display-latex-image element commands latex cache-file -image-type))))
        (cond
         (cache-file-exists?
          (funcall display-image element))
         ((not cached-only)
          (xenops-math-create-latex-image element latex -image-type colors cache-file display-image)))))))

(aio-defun xenops-math-create-latex-image (element latex image-type colors cache-file display-image)
  "Process latex string to SVG via external processes, asynchronously."
  (cl-incf xenops-apply-in-flight-counter)
  (xenops-element-create-marker element)
  (let* ((dir temporary-file-directory)
         (base-name (f-base cache-file))
         (make-file-name (lambda (ext) (f-join dir (concat base-name ext))))
         (tex-file (funcall make-file-name ".tex"))
         (dvi-file (funcall make-file-name ".dvi"))
         (svg-file (funcall make-file-name ".svg"))
         (processing-type 'dvisvgm)
         (processing-info
          (cdr (assq processing-type org-preview-latex-process-alist)))
         (dpi (* (org--get-display-dpi)
                 (car (plist-get processing-info :image-size-adjust))
                 xenops-math-image-scale-factor))
         (scale (/ dpi 140))
         (bounding-box (if (eq 'inline-math (plist-get element :type)) 1 10))
         (commands
          `(("latex" "-shell-escape" "-interaction" "nonstopmode" "-output-directory" ,dir ,tex-file)
            ("dvisvgm" ,dvi-file
             "-n"
             "-b" ,(number-to-string bounding-box)
             "-c" ,(number-to-string scale)
             "-o" ,svg-file))))
    (aio-await
     (aio-with-async
       (let* ((org-latex-packages-alist (xenops-math-get-latex-preamble))
              (org-latex-default-packages-alist)
              (latex-header (org-latex-make-preamble
                             (org-export-get-environment (org-export-get-backend 'latex))
                             org-format-latex-header
                             'snippet)))
         (with-temp-file tex-file
           (destructuring-bind (fg bg) colors
             (insert latex-header
                     "\n\\begin{document}\n"
                     "\\definecolor{fg}{rgb}{" fg "}\n"
                     "\\definecolor{bg}{rgb}{" bg "}\n"
                     "\n\\pagecolor{bg}\n"
                     "\n{\\color{fg}\n"
                     latex
                     "\n}\n"
                     "\n\\end{document}\n"))))))
    (condition-case error
        (progn
          (dolist (command commands)
            (aio-await (xenops-aio-subprocess command)))
          (aio-await (aio-with-async (copy-file svg-file cache-file 'replace)))
          (aio-await
           (aio-with-async
             (-when-let* ((element (xenops-math-parse-element-at (plist-get element :begin-marker))))
               (funcall display-image element commands)
               (xenops-element-deactivate-marker element)
               (cl-decf xenops-apply-in-flight-counter)))))
      (error (aio-await
              (aio-with-async
                (-when-let* ((element (xenops-math-parse-element-at (plist-get element :begin-marker))))
                  (xenops-math-display-latex-error element error)
                  (xenops-element-deactivate-marker element)
                  (cl-decf xenops-apply-in-flight-counter))))))))

(defun xenops-aio-subprocess (command &optional output-buffer error-buffer)
  "Start asynchronous subprocess; return a promise.

Resolve the promise when the process exits. The value function
does nothing if the exit is successful, but if the process exits
with an error status, then the value function signals the error."
  (let* ((promise (aio-promise))
         (name (format "xenops-aio-subprocess-%s"
                       (sha1 (prin1-to-string command))))
         (output-buffer (generate-new-buffer name))
         (sentinel
          (lambda (process event)
            (unless (process-live-p process)
              (aio-resolve
               promise
               (lambda ()
                 (if (eq 0 (process-exit-status process))
                     (kill-buffer output-buffer)
                   (signal 'error
                           (prog1 (list :xenops-error-data
                                    (list (s-join " " command)
                                          event
                                          (with-current-buffer output-buffer
                                            (buffer-string))))
                             (kill-buffer output-buffer))))))))))
    (prog1 promise
      (make-process
       :name name
       :buffer output-buffer
       :command command
       :sentinel sentinel))))

(defun xenops-math-get-latex-colors ()
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

(defun xenops-math-get-latex-preamble ()
  (let ((key (sha1 (prin1-to-string (list (buffer-file-name) TeX-master)))))
    (unless (assoc key xenops-math-latex-preamble-cache)
      (push (cons key (xenops-math-compute-latex-preamble))
            xenops-math-latex-preamble-cache))
    (cdr (assoc key xenops-math-latex-preamble-cache))))

(defun xenops-math-compute-latex-preamble ()
  (let ((file (make-temp-file "xenops-math-TeX-region-create" nil ".tex")))
    (TeX-region-create file "" (buffer-file-name) 0)
    (with-temp-buffer
      (insert-file-contents file)
      (split-string
       (buffer-substring (re-search-forward "\\documentclass.+$")
                         (progn (search-forward "\\begin{document}")
                                (match-beginning 0)))
       "\n" t "[ \t\n]+"))))

(defun xenops-math-regenerate (element)
  (let ((cache-file (xenops-math-get-cache-file element)))
    (when cache-file
      (delete-file cache-file)
      (clear-image-cache cache-file)
      (message "Xenops: deleted file: %s" cache-file))
    (xenops-element-overlays-delete element)
    (xenops-math-render element)))

(defun xenops-math-reveal (element)
  (xenops-element-overlays-delete element)
  (goto-char (plist-get element :begin-content)))

(defun xenops-math-image-increase-size (element)
  (xenops-math-image-change-size element xenops-math-image-change-size-factor))

(defun xenops-math-image-decrease-size (element)
  (xenops-math-image-change-size element (/ 1 xenops-math-image-change-size-factor)))

(defun xenops-math-image-change-size (element factor)
  (-if-let* ((image (xenops-element-get-image element)))
      (when (eq (image-property image :type) 'svg)
        (image-flush image)
        (let* ((data (or (eval (image-property image :data))
                         (and (f-exists? (image-property image :file))
                              (prog1 (f-read-text (image-property image :file))
                                (setf (image-property image :file) nil))))))
          (if data
              (setf (image-property image :data) (xenops-util-svg-resize data factor)))))))

(defun xenops-math-image-reset-size (element)
  (xenops-math-reveal element)
  (xenops-math-render element))

(defun xenops-math-block-delimiter-lines-regexp ()
  "A regexp matching the start or end line of any block math element."
  (format "\\(%s\\)"
          (s-join "\\|"
                  (apply #'append (xenops-elements-get-for-types '(block-math table) :delimiters)))))

(defun xenops-math-block-math-font-lock-handler ()
  (add-face-text-property (match-beginning 0) (match-end 0) 'fixed-pitch)
  (xenops-math-add-cursor-sensor-property))

(defun xenops-math-inline-math-font-lock-handler ()
  (xenops-math-add-cursor-sensor-property))

(defun xenops-math-add-cursor-sensor-property ()
  "Arrange for math elements to be rendered whenever the cursor leaves the element.

Suppose we have inline element 1$345$7 where the integers are the
buffer positions of the corresponding characters. The following
tables shows required behavior for cursor position transitions.

| old pos | new pos | behavior      | notes                                   | implementation |
|---------+---------+---------------+-----------------------------------------+----------------|
|       4 |       3 | do not render | 3 is pos for inserting at element start |                |
|       3 |       1 | render        |                                         |                |
|       5 |       6 | do not render | 6 is pos for inserting at element end   |                |
|       5 |       7 | render        |                                         |                |
|       5 |       6 | do not render |                                         |                |

The above is achieved by setting the `cursor-sensor-functions'
property on positions 3-6 inclusive (which are the :begin-content
and :end-content indices).

In addition, we require the following text property inheritance behavior on insertion
| pos | behavior                  | implementation                           |
|-----+---------------------------+------------------------------------------|
|   2 | do not inherit from right | front-nonsticky: default Emacs behaviour |
| 3-6 | inherit from left         | rear sticky: default Emacs behaviour     |
|   7 | do not inherit from left  | set rear-nonsticky on 6                  |
"
  (-if-let* ((element (xenops-math-parse-element-at-point)))
      (let ((beg (plist-get element :begin-content))
            (end (1+ (plist-get element :end-content)))
            (props '(cursor-sensor-functions (xenops-math-handle-element-transgression))))
        (add-text-properties beg end props)
        (add-text-properties (1- end) end '(rear-nonsticky (cursor-sensor-functions))))))

(defun xenops-math-handle-paste ()
  "If the text to be pasted is a math element then handle the paste.
If we are in a math element, then paste without the delimiters"
  (let ((copied-text (current-kill 0 'do-not-rotate)))
    (-if-let* ((element (xenops-math-parse-element-from-string copied-text)))
        (if (xenops-math-parse-element-at-point)
            (progn
              (insert-for-yank
               (substring copied-text
                          ;; `xenops-math-parse-element-from-string' returns 1-based indexes,
                          ;; suitable for indexing into a buffer; string is 0-based.
                          (1- (plist-get element :begin-content))
                          (1- (plist-get element :end-content))))
              (rotate-yank-pointer 1))
          (save-excursion (yank))
          (xenops-math-render (xenops-math-parse-element-at-point))
          t))))

(defun xenops-math-paste ()
  (or (xenops-math-handle-paste) (yank)))

(defun xenops-math-fill-paragraph-after-advice (&rest args)
  (let ((forward-paragraph-fn (if (fboundp 'LaTeX-forward-paragraph)
                                  'LaTeX-forward-paragraph
                                'forward-paragraph))
        (backward-paragraph-fn (if (fboundp 'LaTeX-backward-paragraph)
                                   'LaTeX-backward-paragraph
                                 'backward-paragraph)))
    (save-excursion
      ;; If point is at the start of a paragraph, LaTeX-fill-paragraph fills the paragraph
      ;; ahead. Therefore we move to the end before going back to locate the beginning.
      (funcall forward-paragraph-fn)
      (push-mark (point) t t)
      (funcall backward-paragraph-fn)
      (if (region-active-p)
          (xenops-render-if-cached)))))

(defun xenops-math-parse-element-from-string (element-string)
  (with-temp-buffer
    (save-excursion (insert element-string))
    (-if-let* ((element (xenops-math-parse-element-at-point)))
        (when (eq (- (plist-get element :end)
                     (plist-get element :begin))
                  (length element-string))
          element))))

(defun xenops-math-handle-element-transgression (window oldpos event-type)
  "Render a math element when point leaves it."
  ;; TODO: check window
  (message "xenops-math-handle-element-transgression")
  (if (eq event-type 'left)
      (-if-let* ((was-in (xenops-math-parse-element-at oldpos)))
          (unless (xenops-element-get-image was-in)
            (xenops-math-render was-in)))))

(defun xenops-math-mouse-drag-region-around-advice (mouse-drag-region-fn start-event)
  "If point is in a math element, then cause mouse drag to appear to drag the associated image:
1. Select the math element as the currently active region.
2. Temporarily alter tooltip-show so that it displays the image."
  (-if-let* ((element (xenops-math-parse-element-at (posn-point (event-start start-event)))))
      (progn
        (push-mark (plist-get element :begin))
        (goto-char (plist-get element :end))
        (let ((tooltip-show-fn (symbol-function 'tooltip-show))
              (image-tooltip (propertize " "
                                         'display
                                         ;; TODO: the file path should be stored somewhere, not recomputed.
                                         ;; TODO: detect image type
                                         `(image . (:file ,(xenops-math-get-cache-file element) :type svg)))))
          (cl-letf (((symbol-function 'mouse-posn-property)
                     (lambda (&rest args) 'region))
                    ((symbol-function 'tooltip-show)
                     (lambda (text &rest args)
                       (apply tooltip-show-fn image-tooltip args))))
            (funcall mouse-drag-region-fn start-event))))
    (funcall mouse-drag-region-fn start-event)))

(defun xenops-math-parse-element-at (pos)
  (save-excursion
    (goto-char pos)
    (xenops-math-parse-element-at-point)))

(defun xenops-math-parse-element-at-point ()
  (or (xenops-math-parse-inline-element-at-point)
      (xenops-math-parse-block-element-at-point)
      (xenops-math-parse-table-at-point)))

(defun xenops-math-parse-block-element-at-point ()
  (xenops-parse-element-at-point 'block-math))

(defun xenops-math-parse-table-at-point ()
  (xenops-parse-element-at-point 'table))

(defun xenops-math-parse-inline-element-at-point ()
  "If point is in inline math element, return plist describing match"
  (let ((delimiter (caar (xenops-elements-get 'inline-math :delimiters))))
    (save-excursion
      (let ((odd-count (cl-oddp (count-matches delimiter (point-at-bol) (point)))))
        (when (and (not odd-count) (looking-at (caar (xenops-elements-get 'inline-math :delimiters))))
          (forward-char)
          (setq odd-count t))
        (and odd-count
             (xenops-parse-element-at-point-matching-delimiters
              'inline-math (list delimiter delimiter)
              (point-at-bol) (or (save-excursion (re-search-forward delimiter nil t)) (point-max))))))))

(defun xenops-math-concatenate (beg end)
  (interactive "r")
  (let* ((delimiters )
         (boundary-regexp
          (format "\\(\n?%s\n?\\)"
                  (s-join "\\|"
                          (cl-loop
                           for pair in (xenops-elements-get 'block-math :delimiters)
                           collecting (s-join "[ \t\n]+" (--map (s-chop-prefix "^" it) (reverse pair)))))))
         (concatenated?))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward boundary-regexp end t)
        (setq concatenated? t)
        (replace-match " \\\\\\\\\n")))
    (when concatenated?
      (push-mark)
      (save-excursion
        (goto-char beg)
        (xenops-apply '(render)))
      (pop-mark))))

(defun xenops-math-display-latex-image (element commands help-echo cache-file -image-type)
  "Display SVG image resulting from successful LaTeX compilation."
  (let ((margin (if (eq 'inline-math (plist-get element :type))
                    0 `(,xenops-math-image-margin . 0)))
        (ov (xenops-math-make-overlay element commands help-echo)))
    (overlay-put ov 'display
                 `(image :type ,(intern -image-type)
                         :file ,cache-file :ascent center :margin ,margin)))
  (unless (equal xenops-math-image-current-scale-factor 1.0)
    (xenops-math-image-change-size element xenops-math-image-current-scale-factor)))

(defun xenops-math-make-overlay (element commands help-echo)
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
               ["Copy LaTeX command" (xenops-math-image-overlay-copy-latex-command ,ov)]))
            event)))
    (overlay-put ov 'help-echo help-echo)
    (overlay-put ov 'commands commands)
    (define-key keymap [mouse-3] xenops-math-image-overlay-menu)
    ov))

(defun xenops-math-display-latex-error (element error)
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
                       ["View failing command output" (xenops-math-display-process-output ,output)]
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

(defun xenops-math-display-process-output (output)
  (let ((buf (get-buffer-create "*Xenops external command output*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert output))
    (display-buffer buf)))

(defun xenops-math-image-overlay-copy-latex-command (overlay)
  (let ((latex-command (car (overlay-get overlay 'commands))))
    (kill-new (s-join " " latex-command))))

(defun xenops-math-get-cache-file (element)
  ;; TODO: the file path should be stored somewhere, not recomputed.
  (let* ((beg (plist-get element :begin))
         (end (plist-get element :end))
         (latex (buffer-substring-no-properties beg end))
         (image-type (plist-get (cdr (assq xenops-math-process
                                           org-preview-latex-process-alist))
                                :image-output-type))
         (colors (save-excursion
                   (goto-char beg)
                   (xenops-math-get-latex-colors))))
    (xenops-math-compute-file-name latex image-type colors)))

(defun xenops-math-file-name-static-hash-data ()
  (list org-format-latex-header
        org-latex-default-packages-alist
        org-latex-packages-alist
        org-format-latex-options))

(defun xenops-math-compute-file-name (latex image-type colors)
  (let* ((data (append (xenops-math-file-name-static-hash-data) (list latex colors)))
         (hash (sha1 (prin1-to-string data))))
    (format "%s.%s" (f-join (f-expand xenops-cache-directory) hash) image-type)))

(provide 'xenops-math)
