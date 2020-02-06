;; -*- lexical-binding: t -*-

;; Terminology
;;
;; | math element   | either an inline element or a block element    |
;; | inline element | inline math delimited by $...$                 |
;; | block element  | e.g. a \begin{align}...\end{align} environment |
(require 'xenops-math-latex)

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

(defun xenops-math-font-lock-keywords ()
  `((,(xenops-math-block-delimiter-lines-regexp)
     (0
      (xenops-math-block-math-font-lock-handler)))))

(defun xenops-math-activate ()
  (setq-local xenops-math-latex-tasks-semaphore
              (aio-sem xenops-math-latex-max-tasks-in-flight))
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
              (xenops-element-overlay-get element 'xenops-math-latex-waiting)
              (string-equal "" (s-trim (buffer-substring (plist-get element :begin-content)
                                                         (plist-get element :end-content)))))
    (let ((latex (buffer-substring-no-properties (plist-get element :begin)
                                                 (plist-get element :end))))
      ;; The name "image-type" is bound by image-mode and this interferes with the closure.
      (let* ((-image-type (plist-get (cdr (assq xenops-math-process
                                                org-preview-latex-process-alist))
                                     :image-output-type))
             (colors (xenops-math-latex-get-colors))
             (cache-file (xenops-math-compute-file-name latex -image-type colors))
             (cache-file-exists? (file-exists-p cache-file))
             (display-image
              (lambda (element &optional commands)
                (xenops-math-latex-display-image element commands latex cache-file -image-type))))
        (cond
         (cache-file-exists?
          (funcall display-image element))
         ((not cached-only)
          (xenops-math-latex-display-waiting element)
          (xenops-math-latex-create-image element latex -image-type colors cache-file display-image)))))))

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

| old pos | new pos | behavior      | notes                                   |
|---------+---------+---------------+-----------------------------------------+
|       4 |       3 | do not render | 3 is pos for inserting at element start |
|       3 |       1 | render        |                                         |
|       5 |       6 | do not render | 6 is pos for inserting at element end   |
|       5 |       7 | render        |                                         |
|       5 |       6 | do not render |                                         |

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
  (or (xenops-math-parse-dollar-delimited-inline-element-at-point)
      (xenops-math-parse-paren-delimited-inline-element-at-point)))

(defun xenops-math-parse-paren-delimited-inline-element-at-point ()
  "If point is in backslash-paren-delimited inline math element, return plist describing match."
  (cl-letf (((symbol-function 'xenops-elements-get)
             (lambda (type key)
               (if (and (eq type 'inline-math) (eq key :delimiters))
                   '(("\\\\(" "\\\\)"))))))
    (xenops-parse-element-at-point 'inline-math)))

(defun xenops-math-parse-dollar-delimited-inline-element-at-point ()
  "If point is in dollar-delimited inline math element, return plist describing match."
  ;; This is a bit awkward since the start and end delimiters are the same. It
  ;; will fail if dollar-delimited inline math extends over multiple lines.
  (let ((delimiter "\\$"))
    (save-excursion
      (let ((odd-count (cl-oddp (count-matches delimiter (point-at-bol) (point)))))
        (when (and (not odd-count) (looking-at delimiter))
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
                   (xenops-math-latex-get-colors))))
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
