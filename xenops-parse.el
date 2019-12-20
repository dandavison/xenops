(defun xenops-parse-element-at-point (type)
  "A base `parse-at-point` implementation that is used by some
concrete element types. It is not expected to work for all
types.

If point is in element, return parsed element as a plist."
  (xenops-util-first-result
   (lambda (pair)
     (xenops-parse-element-at-point-matching-delimiters
      type
      pair
      (point-min)
      (point-max)))
   (xenops-elements-get type :delimiters)))

(defun xenops-parse-image-at (pos)
  (let ((display (get-char-property pos 'display )))
    (and (eq (car display) 'image) display)))

(defun xenops-parse-element-at-point-matching-delimiters (type delimiters lim-up lim-down)
  "If point is between regexps, return plist describing
  match. Like `org-between-regexps-p', but modified to return
  more match data."
  (-if-let* ((coords
              (save-excursion
                (when (looking-at (car (last delimiters)))
                  ;; This function will return nil if point is between delimiters separated by
                  ;; zero characters.
                  (left-char))
                (xenops-parse-between-regexps? delimiters lim-up lim-down (point)))))
      (append coords `(:type ,type :delimiters ,delimiters))))

(defun xenops-parse-between-regexps? (delimiters lim-up lim-down pos)
  "Based on `org-between-regexps-p'."
  (save-excursion
    (let (beg-beg beg-end end-beg end-end)
      (and (or (org-in-regexp (car delimiters))
               (re-search-backward (car delimiters) lim-up t))
           (save-match-data
             (and
              (setq beg-beg (match-beginning 0))
              (goto-char (match-end 0))
              (setq beg-end (point))
              (skip-chars-forward " \t\n")
              (re-search-forward (car (last delimiters)) lim-down t)
              (> (setq end-end (match-end 0)) pos)
              (goto-char (match-beginning 0))
              (setq end-beg (point))
              (skip-chars-backward " \t\n")
              (not (re-search-backward (car delimiters) (1+ beg-beg) t))
              (list :begin beg-beg :begin-content beg-end :end-content end-beg :end end-end)))))))

(provide 'xenops-parse)
