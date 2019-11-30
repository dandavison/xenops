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
      (xenops-math-block-delimiter-lines-set-face)))))

(defun xenops-math-activate ()
  (setq mouse-drag-and-drop-region t)
  (advice-add #'mouse-drag-region :around #'xenops-math-mouse-drag-region-around-advice)
  (advice-add fill-paragraph-function :after #'xenops-math-fill-paragraph-after-advice)
  (font-lock-add-keywords nil (xenops-math-font-lock-keywords))
  (add-to-list 'fill-nobreak-predicate #'xenops-math-parse-inline-element-at-point))

(defun xenops-math-deactivate ()
  (advice-remove #'mouse-drag-and-drop-region #'xenops-math-mouse-drag-region-around-advice)
  (advice-remove fill-paragraph-function #'xenops-math-fill-paragraph-after-advice)
  (font-lock-remove-keywords nil (xenops-math-font-lock-keywords)))

(defun xenops-math-render (element &optional cached-only)
  (xenops-math-set-org-preview-latex-process-alist! element)
  (let ((beg (plist-get element :begin))
        (end (plist-get element :end)))
    (goto-char beg)
    (unless (xenops-element-get-image-at-point)
      (let* ((latex (buffer-substring-no-properties beg end))
             (image-type (plist-get (cdr (assq xenops-math-process
                                               org-preview-latex-process-alist))
                                    :image-output-type))
             (margin (if (eq 'inline-math (plist-get element :type))
                         0
                       `(,xenops-math-image-margin . 0)))
             (cache-file (xenops-math-compute-file-name latex image-type))
             (cache-file-exists? (file-exists-p cache-file)))
        (when (or cache-file-exists? (not cached-only))
          (unless cache-file-exists?
            (message "Xenops: creating file: %s" cache-file)
            (let ((org-latex-packages-alist (xenops-math-get-latex-preamble-lines)))
              (org-create-formula-image
               latex cache-file org-format-latex-options 'forbuffer xenops-math-process)))
          (xenops-element-delete-overlays element)
          (xenops-math-make-overlay element cache-file image-type margin latex))))))

(defun xenops-math-get-latex-preamble-lines ()
  (let ((file (make-temp-file "xenops-math-" nil ".tex")))
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
  (goto-char (plist-get element :begin-content))
  (cursor-sensor-mode +1)
  (add-text-properties (plist-get element :begin)
                       (plist-get element :end)
                       '(cursor-sensor-functions (xenops-math-handle-element-exit))))

(defun xenops-math-image-increase-size (element)
  (xenops-math-image-change-size element xenops-math-image-change-size-factor))

(defun xenops-math-image-decrease-size (element)
  (xenops-math-image-change-size element (/ 1 xenops-math-image-change-size-factor)))

(defun xenops-math-image-change-size (element factor)
  (-when-let (image (xenops-element-get-image element))
    (when (eq (image-property image :type) 'svg)
      (image-flush image)
      (let* ((data (or (eval (image-property image :data))
                       (prog1 (f-read-text (image-property image :file))
                         (setf (image-property image :file) nil)))))
        (setf (image-property image :data) (xenops-util-svg-resize data factor))))))

(defun xenops-math-image-reset-size (element)
  (xenops-math-reveal element)
  (xenops-math-render element))

(defun xenops-math-block-delimiter-lines-regexp ()
  "A regexp matching the start or end line of any block math element."
  (format "\\(%s\\)"
          (s-join "\\|"
                  (apply #'append (xenops-elements-get 'block-math :delimiters)))))

(defun xenops-math-block-delimiter-lines-set-face ()
  (add-face-text-property (match-beginning 0) (match-end 0) 'fixed-pitch))

(defun xenops-math-handle-paste ()
  "If the text to be pasted is a math element then handle the paste.
If we are in a math element, then paste without the delimiters"
  (let ((copied-text (current-kill 0 'do-not-rotate)))
    (-when-let (element (xenops-math-parse-element-from-string copied-text))
      (if (xenops-math-parse-element-at-point)
          (progn
            (insert-for-yank
             (substring copied-text
                        (plist-get element :begin-content)
                        (plist-get element :end-content)))
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
    (-when-let (element (xenops-math-parse-element-at-point))
      (when (eq (- (plist-get element :end)
                   (plist-get element :begin))
                (length element-string))
        element))))

(defun xenops-math-handle-element-exit (window oldpos event-type)
  "Render a math element when point leaves it."
  ;; TODO: check window
  (if (eq event-type 'left)
      (-when-let (was-in (xenops-math-parse-element-at oldpos))
        (cursor-sensor-mode -1)
        (xenops-math-render was-in))))

(defun xenops-math-mouse-drag-region-around-advice (mouse-drag-region-fn start-event)
  "If point is in a math element, then cause mouse drag to appear to drag the associated image:
1. Select the math element as the currently active region.
2. Temporarily alter tooltip-show so that it displays the image."
  (-if-let (element (xenops-math-parse-element-at (posn-point (event-start start-event))))
      (progn
        (push-mark (plist-get element :begin))
        (goto-char (plist-get element :end))
        (let ((tooltip-show-fn (symbol-function 'tooltip-show))
              (image-tooltip (propertize " "
                                         'display
                                         ;; TODO: the file path should be stored somewhere, not recomputed.
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
      (xenops-math-parse-block-element-at-point)))

(defun xenops-math-parse-block-element-at-point ()
  (xenops-parse-element-at-point 'block-math))

(defun xenops-math-parse-inline-element-at-point ()
  "If point is in inline math element, return plist describing match"
  (let ((delimiter (caar (xenops-elements-get 'inline-math :delimiters))))
    (save-excursion
      (let ((odd-count (oddp (count-matches delimiter (point-at-bol) (point)))))
        (when (and (not odd-count) (looking-at (caar (xenops-elements-get 'inline-math :delimiters))))
          (forward-char)
          (setq odd-count t))
        (and odd-count
             (xenops-parse-element-at-point-matching-delimiters
              'inline-math (list delimiter delimiter) (point-at-bol) (point-at-eol)))))))

(defun xenops-math-set-org-preview-latex-process-alist! (element)
  (let* ((bounding-box (if (eq 'inline-math (plist-get element :type)) "1" "10"))
         (dvisvgm-process-plist (cdr (assoc 'dvisvgm org-preview-latex-process-alist)))
         (dvisvgm-image-converter (car (plist-get dvisvgm-process-plist
                                                  :image-converter))))
    ;; TODO: this mutates the global variable!
    (plist-put org-format-latex-options :scale xenops-math-image-scale-factor)
    (assert (and (string-match " -b \\([^ ]+\\) " dvisvgm-image-converter)
                 (plist-put dvisvgm-process-plist
                            :image-converter `(,(replace-match bounding-box t t
                                                               dvisvgm-image-converter 1)))))))

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
    (xenops-math-compute-file-name latex image-type)))

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
