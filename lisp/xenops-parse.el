;;; xenops-parse.el --- Functions for parsing elements from their text representation -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:

(declare-function xenops-elements-get "xenops-elements")
(declare-function xenops-elements-get-all "xenops-elements")
(declare-function xenops-overlay-at-point "xenops-overlay")
(declare-function xenops-util-first-result "xenops-util")

(defun xenops-parse-any-element-at-point (&optional parse-at-point-fns no-recurse)
  "Return the element at point if there is one.

Try a list of parser functions until first success. If
PARSE-AT-POINT-FNS is non-nil, use this as the list of parser
functions. Otherwise, use the `parser` entries in
`xenops-elements'.

If NO-RECURSE is nil and there is an overlay at point, move to
the beginning of the overlay and attempt the parse there."
  ;; If there's a xenops overlay at point, then the user will expect that element to be returned,
  ;; even if point somehow isn't actually on the element.
  (-if-let* ((ov (and (not (or parse-at-point-fns no-recurse))
                      (xenops-overlay-at-point))))
      (save-excursion
        (goto-char (overlay-start ov))
        (xenops-parse-any-element-at-point nil t))
    (xenops-util-first-result
     #'funcall
     (or parse-at-point-fns
         (xenops-elements-get-all :parser)))))

(defun xenops-parse-element-at-point (type &optional lim-up lim-down delimiters)
  "Return the element at point if there is one and it is of type TYPE.

Optional arguments LIM-UP and LIM-DOWN bound the search. Optional
argument DELIMITERS specifies the delimiters sought."
  ;; This is a base `parse-at-point` implementation that is used by
  ;; some concrete element types. It is not expected to work for all
  ;; types.
  ;; TODO: make this more consistent.
  (xenops-util-first-result
   (lambda (pair)
     (xenops-parse-element-at-point-matching-delimiters
      type
      pair
      (or lim-up (point-min))
      (or lim-down (point-max))))
   (if delimiters (list delimiters)
     (xenops-elements-get type :delimiters))))

(defun xenops-parse-element-at-point-matching-delimiters (type delimiters lim-up lim-down)
  "If point is between regexps, return plist describing match.

TYPE is the element type being parsed. LIM-UP and LIM-DOWN bound
the search. DELIMITERS are the delimiters sought."
  ;; Based on `org-between-regexps-p'.
  (save-excursion
    (when (looking-at (car (last delimiters)))
      ;; This function will return nil if point is between delimiters separated by
      ;; zero characters.
      (unless (bobp) (left-char)))
    (let ((pos (point)) beg-beg beg-end end-beg end-end)
      (and (or (org-in-regexp (car delimiters))
               (and (< lim-up (point)) (re-search-backward (car delimiters) lim-up t)))
           (save-match-data
             (and
              (setq beg-beg (match-beginning 0))
              (goto-char (match-end 0))
              (setq beg-end (point))
              (skip-chars-forward "\t\n")
              (> lim-down (point))
              (re-search-forward (car (last delimiters)) lim-down t)
              (> (setq end-end (match-end 0)) pos)
              (goto-char (match-beginning 0))
              (setq end-beg (point))
              (skip-chars-backward "\t\n")
              (not (re-search-backward (car delimiters) (1+ beg-beg) t))
              (list :type type
                    :begin beg-beg
                    :begin-content beg-end
                    :end-content end-beg
                    :end end-end
                    :delimiters delimiters)))))))

(provide 'xenops-parse)

;;; xenops-parse.el ends here
