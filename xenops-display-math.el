(defvar xenops-display-math-process 'dvisvgm)

(defun xenops-display-math-activate ()
  (define-key xenops-mode-map [(left)] (lambda () (interactive) (xenops-display-math-on-entry #'left-char)))
  (define-key xenops-mode-map [(right)] (lambda () (interactive) (xenops-display-math-on-entry #'right-char)))
  (define-key xenops-mode-map [(down)] (lambda () (interactive) (xenops-display-math-on-entry #'next-line)))
  (define-key xenops-mode-map [(up)] (lambda () (interactive) (xenops-display-math-on-entry #'previous-line)))
  ;; TODO: DNW
  (add-to-list 'fill-nobreak-predicate (lambda () (xenops-display-math-in-inline-math-element-p "\\$"))))

(defun xenops-display-math-dwim ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (or (xenops-display-math-at-point)
          (xenops-display-math-all)))))

(defun xenops-display-math-at-point ()
  (interactive)
  (let ((coords (xenops-display-math-parse-element-at-point)))
    (when coords (progn
                   (xenops-display-math coords)
                   (forward-line)))))

(defun xenops-display-math-regenerate-math-at-point ()
  (interactive)
  (let ((cache-file (xenops-display-math-get-cache-file-at-point)))
    (when cache-file (delete-file cache-file))
    (xenops-display-math-at-point)))

(defun xenops-display-math-hide ()
  (interactive)
  (let ((coords (or (and (use-region-p)
                         `(:begin ,(region-beginning) :end ,(region-end)))
                    (xenops-display-math-parse-element-at-point))))
    (if coords (progn (org-remove-latex-fragment-image-overlays (plist-get coords :begin)
                                                                (plist-get coords :end))
                      (when (use-region-p)
                        (deactivate-mark)))
      (org-remove-latex-fragment-image-overlays))))

(defun xenops-display-math-on-entry (move-point-command)
  (if (region-active-p)
      (funcall move-point-command)
    (let ((was-in (xenops-display-math-parse-element-at-point)))
      (funcall move-point-command)
      (save-excursion
        (let ((now-in (xenops-display-math-parse-element-at-point)))
          (let ((entered (and (not was-in) now-in))
                (exited (and was-in (not (equal now-in was-in)))))
            (cond
             (entered (if (org--list-latex-overlays (plist-get now-in :begin)
                                                    (plist-get now-in :end))
                          (xenops-display-math-hide)))
             (exited (xenops-display-math was-in)))))))))

(defun xenops-display-math-all ()
  "Display all math content in the buffer."
  (if (use-region-p)
      (progn
        (save-restriction (narrow-to-region (region-beginning)
                                            (region-end))
                          (goto-char (point-min))
                          (xenops-display-math-all-))
        (deactivate-mark))
    (xenops-display-math-all-)))

(defun xenops-display-math-all- ()
  (cl-flet ((next-match-pos (regexp)
                            (save-excursion
                              (or (and (re-search-forward regexp nil t) (point))
                                  (point-max)))))
    (catch 'exit
      (while t
        (let ((delimiters (-min-by (lambda (pair1 pair2) (> (next-match-pos (car pair1))
                                                       (next-match-pos (car pair2))))
                                   xenops-math-delimiters))
              coords)
          (setq coords (plist-put coords :delimiters delimiters))
          (unless (re-search-forward (car delimiters) nil t)
            (throw 'exit nil))
          (setq coords (plist-put coords :begin (match-beginning 0)))
          (re-search-forward (cdr delimiters))
          (setq coords (plist-put coords :end (match-end 0)))
          (xenops-display-math coords)
          ;; TODO: This shouldn't be necessary but currently it
          ;; sometimes gets stuck attempting to process the same
          ;; block repeatedly.
          (goto-char (plist-get coords :end)))))))

(defun xenops-display-math-parse-element-at-point ()
  "If point is in previewable block, return plist describing match"
  (let ((inline-delimiter (car xenops-math-delimiters)))
    (assert (xenops-display-math-inline-delimiters-p inline-delimiter))
    (or (xenops-display-math-in-inline-math-element-p (car inline-delimiter))
        (-any #'identity (mapcar
                          (lambda (pair)
                            (xenops-display-math-between-regexps-p (car pair)
                                                                   (cdr pair)
                                                                   (point-min)
                                                                   (point-max)))
                          (cdr xenops-math-delimiters))))))

(defun xenops-display-math-in-inline-math-element-p (delimiter)
  "Is point within an inline block delimited by `delimiter'?"
  (and (oddp (count-matches delimiter (point-at-bol) (point)))
       (xenops-display-math-between-regexps-p delimiter delimiter (point-at-bol)
                                              (point-at-eol))))

(defun xenops-display-math-inline-delimiters-p (delimiters)
  (equal delimiters '("\\$" . "\\$")))

(defun xenops-display-math-between-regexps-p (start-re end-re lim-up lim-down)
  "If point is between regexps, return plist describing match"
  (let ((coords (if (looking-at end-re)
                    ;; This function will return nil if point is between delimiters
                    ;; separated by zero characters.
                    (save-excursion (left-char)
                                    (org-between-regexps-p start-re end-re lim-up lim-down))
                  (org-between-regexps-p start-re end-re lim-up lim-down))))
    (when coords `(:begin ,(car coords) :end ,(cdr coords) :delimiters (,start-re . ,end-re)))))

(defun xenops-display-math-set-org-preview-latex-process-alist! (coords)
  (let* ((inline-p (xenops-display-math-inline-delimiters-p (plist-get coords :delimiters)))
         (bounding-box (if inline-p "1" "10"))
         (dvisvgm-process-plist (cdr (assoc 'dvisvgm org-preview-latex-process-alist)))
         (dvisvgm-image-converter (car (plist-get dvisvgm-process-plist
                                                  :image-converter))))
    ;; TODO: this mutates the global variable!
    (plist-put org-format-latex-options :scale 0.8)
    (assert (and (string-match " -b \\([^ ]+\\) " dvisvgm-image-converter)
                 (plist-put dvisvgm-process-plist
                            :image-converter `(,(replace-match bounding-box t t
                                                               dvisvgm-image-converter 1)))))))

(defun xenops-display-math (coords)
  (xenops-display-math-set-org-preview-latex-process-alist! coords)
  (let ((beg (plist-get coords :begin))
        (end (plist-get coords :end)))
    (goto-char beg)
    (unless (eq (get-char-property (point) 'org-overlay-type)
                'org-latex-overlay)
      (let* ((latex (buffer-substring-no-properties beg end))
             (image-type (plist-get (cdr (assq xenops-display-math-process
                                               org-preview-latex-process-alist))
                                    :image-output-type))
             (cache-file (xenops-display-math-compute-file-name latex image-type)))
        (unless (file-exists-p cache-file)
          (message "xenops: creating image file: %s" cache-file)
          (org-create-formula-image
           latex cache-file org-format-latex-options 'forbuffer xenops-display-math-process))
        (dolist (o (overlays-in beg end))
          (when (eq (overlay-get o 'org-overlay-type)
                    'org-latex-overlay)
            (delete-overlay o)))
        (org--format-latex-make-overlay beg end cache-file image-type)))))

(defun xenops-display-math-get-cache-file-at-point ()
  (let ((context (xenops-display-math-parse-element-at-point)))
    (if context
      (let* ((beg (plist-get context :begin))
             (end (plist-get context :end))
             (latex (buffer-substring-no-properties beg end))
             (image-type (plist-get (cdr (assq xenops-display-math-process
                                               org-preview-latex-process-alist))
                                    :image-output-type)))
        (xenops-display-math-compute-file-name latex image-type)))))

(defun xenops-display-math-compute-file-name (latex image-type)
  (let ((hash (sha1 (prin1-to-string
                     (list org-format-latex-header
                           org-latex-default-packages-alist
                           org-latex-packages-alist
                           org-format-latex-options
                           latex)))))
    (format "%s.%s" (f-join (f-expand xenops-cache-directory) hash) image-type)))

(provide 'xenops-display-math)
