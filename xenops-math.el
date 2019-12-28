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

(defun xenops-math-render (element)
  (unless (or (xenops-element-get-image element)
              (string-equal "" (s-trim (buffer-substring (plist-get element :begin-content)
                                                         (plist-get element :end-content)))))
    (let ((latex (buffer-substring-no-properties (plist-get element :begin)
                                                 (plist-get element :end))))
      ;; The name "image-type" is bound by image-mode and this interferes with the closure.
      (let* ((-image-type (plist-get (cdr (assq xenops-math-process
                                                org-preview-latex-process-alist))
                                     :image-output-type))
             (margin (if (eq 'inline-math (plist-get element :type))
                         0
                       `(,xenops-math-image-margin . 0)))
             (cache-file (xenops-math-compute-file-name latex -image-type))
             (cache-file-exists? (file-exists-p cache-file))
             (insert-image (lambda (element)
                             (xenops-element-delete-overlays element)
                             (xenops-math-make-overlay element cache-file -image-type margin latex))))
        (cond
         (cache-file-exists?
          (funcall insert-image element))
         (t
          (xenops-math-create-latex-image element latex -image-type cache-file insert-image)))))))

(aio-defun xenops-math-create-latex-image (element latex image-type cache-file insert-image)
  "Process latex string to SVG via external processes, asynchronously."
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
          `(("latex" "-interaction" "nonstopmode" "-output-directory" ,dir ,tex-file)
            ("dvisvgm" ,dvi-file
             "-n"
             "-b" ,(number-to-string bounding-box)
             "-c" ,(number-to-string scale)
             "-o" ,svg-file))))
    (aio-await
     (aio-with-async
       (let* ((org-latex-packages-alist (xenops-math-get-latex-preamble-lines))
              (org-latex-default-packages-alist)
              (latex-header (org-latex-make-preamble
                             (org-export-get-environment (org-export-get-backend 'latex))
                             org-format-latex-header
                             'snippet)))
         (with-temp-file tex-file
           (destructuring-bind (fg bg) (xenops-math-get-latex-colors)
             (insert latex-header
                     "\n\\begin{document}\n"
                     "\\definecolor{fg}{rgb}{" fg "}\n"
                     "\\definecolor{bg}{rgb}{" bg "}\n"
                     "\n\\pagecolor{bg}\n"
                     "\n{\\color{fg}\n"
                     latex
                     "\n}\n"
                     "\n\\end{document}\n"))))))
    (dolist (command commands)
      (aio-await (funcall #'xenops-aio-subprocess command)))
    (aio-await (aio-with-async (copy-file svg-file cache-file 'replace)))
    (aio-await
     (aio-with-async
       (-when-let* ((element (xenops-math-parse-element-at (plist-get element :begin-marker))))
         (funcall insert-image element))))))

(defun xenops-aio-subprocess (command &optional output-buffer error-buffer)
  (let ((promise (aio-promise))
        (value-function (lambda () (message "xenops-aio: promise resolved: %s" (s-join " " command)))))
    (let ((sentinel (lambda (process event)
                      (aio-resolve promise value-function)))
          (name (format "xenops-aio-subprocess-%s"
                        (sha1 (prin1-to-string command)))))
      (prog1 promise
        (make-process
         :name name
         :command command
         :sentinel sentinel)))))

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

(defun xenops-math-get-latex-preamble-lines ()
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
    (xenops-element-delete-overlays element)
    (xenops-math-render element)))

(defun xenops-math-reveal (element)
  (xenops-element-delete-overlays element)
  (goto-char (plist-get element :begin-content)))

(defun xenops-math-image-increase-size (element)
  (xenops-math-image-change-size element xenops-math-image-change-size-factor))

(defun xenops-math-image-decrease-size (element)
  (xenops-math-image-change-size element (/ 1 xenops-math-image-change-size-factor)))

(defun xenops-math-image-change-size (element factor)
  (-if-let* ((image (xenops-element-get-image element)))
      (when (eq (image-property image :type) 'svg)
        ;; TODO: other image types
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
          (xenops-render)))))

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

(defun xenops-math-make-overlay (element image-file image-type margin help-echo)
  (let ((ov (xenops-element-make-overlay element)))
    (overlay-put ov 'display
                 `(image :type ,(intern image-type)
                         :file ,image-file :ascent center :margin ,margin))
    ov))

(defun xenops-math-get-cache-file (element)
  (let* ((beg (plist-get element :begin))
         (end (plist-get element :end))
         (latex (buffer-substring-no-properties beg end))
         (image-type (plist-get (cdr (assq xenops-math-process
                                           org-preview-latex-process-alist))
                                :image-output-type)))
    (xenops-math-compute-file-name latex image-type colors)))

(defun xenops-math-file-name-static-hash-data ()
  (list org-format-latex-header
        org-latex-default-packages-alist
        org-latex-packages-alist
        org-format-latex-options))

(defun xenops-math-compute-file-name (latex image-type)
  (let* ((data (append (xenops-math-file-name-static-hash-data) (list latex)))
         (hash (sha1 (prin1-to-string data))))
    (format "%s.%s" (f-join (f-expand xenops-cache-directory) hash) image-type)))

(provide 'xenops-math)
