(defun xenops-parse-element-at-point (type)
  "If point is in element, return parsed element as a plist."
  (-any #'identity (mapcar
                    (lambda (pair)
                      (xenops-parse-element-at-point-matching-delimiters
                       type
                       pair
                       (point-min)
                       (point-max)))
                    (xenops-elements-get type :delimiters))))

(defun xenops-parse-element-at-point-matching-delimiters (type delimiters lim-up lim-down)
  "If point is between regexps, return plist describing
  match. Like `org-between-regexps-p', but modified to return
  more match data."
  (-if-let (element
            (save-excursion
              (when (looking-at (cdr delimiters))
                ;; This function will return nil if point is between delimiters separated by
                ;; zero characters.
                (left-char))
              (save-match-data ;; TODO: necessary?
                (let ((pos (point)))
                  (save-excursion
                    (and (or (org-in-regexp (car delimiters))
                             (re-search-backward (car delimiters) lim-up t))
                         (xenops-parse-match nil delimiters lim-down pos)))))))
      (append element `(:type ,type :delimiters ,delimiters))))

(defun xenops-parse-match (element delimiters limit pos)
  "Based on `org-between-regexps-p'."
  (let (beg-beg beg-end end-beg end-end)
    (and (setq beg-beg (match-beginning 0))
         (goto-char (match-end 0))
         (skip-chars-forward " \t\n")
         (setq beg-end (point))
         (re-search-forward (cdr delimiters) limit t)
         (> (setq end-end (match-end 0)) pos)
         (goto-char (match-beginning 0))
         (skip-chars-backward " \t\n")
         (setq end-beg (point))
         (not (re-search-backward (car delimiters) (1+ beg-beg) t))
         (append element
                 `(:begin ,beg-beg :begin-content ,beg-end :end-content ,end-beg :end ,end-end)))))

(provide 'xenops-parse)
