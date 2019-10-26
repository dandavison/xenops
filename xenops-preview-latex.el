(defvar xenops-preview-latex-default-process 'dvisvgm)

(defvar xenops-preview-latex-delimiters
  '(
    ("\\$" . "\\$")
    ("^[ \t]*\\\\begin{align\\*?}" . "^[ \t]*\\\\end{align\\*?}")))

(defun xenops-preview-latex-add-previews-dwim ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (or (xenops-preview-latex-add-preview-at-point)
          (xenops-preview-latex-add-previews)))))


(defun xenops-preview-latex-add-preview-at-point ()
  (interactive)
  (let ((coords (xenops-preview-latex-preview-at-point-coords)))
    (when coords (progn
                   (xenops-preview-latex-org-format-latex coords)
                   (forward-line)))))


(defun xenops-preview-latex-remove-previews-dwim ()
  (interactive)
  (let ((coords (or (and (use-region-p)
                         `(:begin ,(region-beginning) :end ,(region-end)))
                    (xenops-preview-latex-preview-at-point-coords))))
    (if coords (progn (org-remove-latex-fragment-image-overlays (plist-get coords :begin)
                                                                (plist-get coords :end))
                      (when (use-region-p)
                        (deactivate-mark)))
      (org-remove-latex-fragment-image-overlays))))


(defun xenops-preview-latex-toggle-on-entry (move-point-command)
  (if (region-active-p)
      (funcall move-point-command)
    (let ((was-in (xenops-preview-latex-preview-at-point-coords)))
      (funcall move-point-command)
      (save-excursion
        (let ((now-in (xenops-preview-latex-preview-at-point-coords)))
          (let ((entered (and (not was-in) now-in))
                (exited (and was-in (not (equal now-in was-in)))))
            (cond
             (entered (if (org--list-latex-overlays (plist-get now-in :begin)
                                                    (plist-get now-in :end))
                          (xenops-preview-latex-remove-previews-dwim)))
             (exited (xenops-preview-latex-org-format-latex was-in)))))))))


(defun xenops-preview-latex-add-previews ()
  (if (use-region-p)
      (progn
        (save-restriction (narrow-to-region (region-beginning)
                                            (region-end))
                          (goto-char (point-min))
                          (xenops-preview-latex-add-previews-))
        (deactivate-mark))
    (xenops-preview-latex-add-previews-)))


(defun xenops-preview-latex-add-previews- ()
  "Create all latex previews in buffer"
  (cl-flet ((next-match-pos (regexp)
                            (save-excursion
                              (or (and (re-search-forward regexp nil t) (point))
                                  (point-max)))))
    (catch 'exit
      (while t
        (let ((delimiters (-min-by (lambda (pair1 pair2) (> (next-match-pos (car pair1))
                                                       (next-match-pos (car pair2))))
                                   xenops-preview-latex-delimiters))
              coords)
          (setq coords (plist-put coords :delimiters delimiters))
          (unless (re-search-forward (car delimiters) nil t)
            (throw 'exit nil))
          (setq coords (plist-put coords :begin (match-beginning 0)))
          (re-search-forward (cdr delimiters))
          (setq coords (plist-put coords :end (match-end 0)))
          (xenops-preview-latex-org-format-latex coords)
          ;; TODO: This shouldn't be necessary but currently it
          ;; sometimes gets stuck attempting to process the same
          ;; block repeatedly.
          (goto-char (plist-get coords :end)))))))


(defun xenops-preview-latex-preview-at-point-coords ()
  "If point is in previewable block, return plist describing match"
  (let ((inline-delimiter (car xenops-preview-latex-delimiters)))
    (assert (xenops-preview-latex-delimiters-inline-p inline-delimiter))
    (or (xenops-within-inline-block-p (car inline-delimiter))
        (-any #'identity (mapcar
                          (lambda (pair)
                            (xenops-preview-latex-org-between-regexps-p (car pair)
                                                                        (cdr pair)
                                                                        (point-min)
                                                                        (point-max)))
                          (cdr xenops-preview-latex-delimiters))))))

(defun xenops-within-inline-block-p (delimiter)
  "Is point within an inline block delimited by `delimiter'?"
  (and (oddp (count-matches delimiter (point-at-bol) (point)))
       (xenops-preview-latex-org-between-regexps-p delimiter delimiter (point-at-bol)
                                                   (point-at-eol))))

(defun xenops-preview-latex-delimiters-inline-p (delimiters)
  (equal delimiters '("\\$" . "\\$")))


(defun xenops-preview-latex-org-between-regexps-p (start-re end-re lim-up lim-down)
  "If point is between regexps, return plist describing match"
  (let ((coords (if (looking-at end-re)
                    ;; This function will return nil if point is between delimiters
                    ;; separated by zero characters.
                    (save-excursion (left-char)
                                    (org-between-regexps-p start-re end-re lim-up lim-down))
                  (org-between-regexps-p start-re end-re lim-up lim-down))))
    (when coords `(:begin ,(car coords) :end ,(cdr coords) :delimiters (,start-re . ,end-re)))))


(defun xenops-set-org-preview-latex-process-alist! (coords)
  (let* ((inline-p (xenops-preview-latex-delimiters-inline-p (plist-get coords :delimiters)))
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


(defun xenops-preview-latex-org-format-latex (coords)
  (xenops-set-org-preview-latex-process-alist! coords)
  (let ((beg (plist-get coords :begin)) (end (plist-get coords :end)))
    (flet ((org-element-context ()
                                `(latex-fragment
                                  (:begin ,beg :end ,end :value ,(buffer-substring-no-properties beg end))))
           (clear-image-cache ()))
      (condition-case nil
          (org-format-latex "/tmp/preview-latex/"
                            beg end
                            default-directory
                            'overlays
                            nil
                            'forbuffer xenops-preview-latex-default-process)
        (error nil)))))

(provide 'xenops-preview-latex)
