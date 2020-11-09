;;; xenops-apply.el --- Functions for applying operations to elements in a region -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:
(require 'dash)

(declare-function xenops-dispatch-handlers "xenops")
(declare-function xenops-ops-get-for-ops "xenops")
(declare-function xenops-elements-delimiter-start-regexp "xenops-elements")
(declare-function xenops-elements-get-all "xenops-elements")
(declare-function xenops-overlay-delete-overlays-in "xenops-overlay")
(declare-function xenops-parse-any-element-at-point "xenops-parse")

(defvar xenops-apply-pre-apply-hook nil)
(defvar xenops-apply-post-apply-hook nil)
(defvar xenops-apply-user-point nil) ; The user's value of (point) at the outset of an apply operation

(defun xenops-apply-operations (ops &optional pred)
  "Apply operation types OPS to any elements encountered.

The region operated on is either the active region, or the entire
buffer.

Optional argument PRED is a function taking an element plist as
its only argument. The element will be operated on iff PRED
returns non-nil."
  (let ((handlers (xenops-ops-get-for-ops ops :handlers))
        (xenops-apply-user-point (point)))
    (cl-destructuring-bind (beg end region-active)
        (if (region-active-p)
            `(,(region-beginning) ,(region-end) t)
          `(,(point-min) ,(point-max) nil))
      (run-hook-with-args 'xenops-apply-pre-apply-hook handlers beg end region-active)
      (xenops-apply-handlers-over-region handlers beg end region-active pred)
      (run-hook-with-args 'xenops-apply-post-apply-hook handlers beg end region-active))))

(defun xenops-apply-handlers-over-region (handlers beg end _ &optional pred)
  "Apply HANDLERS to any elements encountered between BEG and END.

The region operated on is either the active region, or the entire
buffer. Optional argument PRED is a predicate function taking an
element and returning a boolean, to be used to restrict the set
of elements operated on."
  (cl-flet ((handle (el) (save-excursion
                           (xenops-dispatch-handlers handlers el))))
    (save-excursion
      (goto-char beg)
      (let ((el)
            (parse-at-point-fns (xenops-elements-get-all :parser)))
        (while (setq el (xenops-apply-parse-next-element end parse-at-point-fns))
          (and el
               (not (eq el 'parse-failure))
               (or (null pred) (funcall pred el))
               (ignore-errors (handle el))))))))

(defun xenops-apply-operations-at-point (ops &optional args)
  "Apply operation types OPS to element at point, if there is one.

Optional argument ARGS are passed to `xenops-apply-handlers-at-point'."
  (let ((handlers (xenops-ops-get-for-ops ops :handlers)))
    (run-hook-with-args 'xenops-apply-pre-apply-hook handlers)
    (prog1 (xenops-apply-handlers-at-point handlers args)
      (run-hook-with-args 'xenops-apply-post-apply-hook handlers))))

(defun xenops-apply-handlers-at-point (handlers &optional _)
  "Apply HANDLERS to element at point if there is one."
  (-if-let* ((el (xenops-parse-any-element-at-point)))
      (xenops-dispatch-handlers handlers el)))

(defun xenops-apply-parse-next-element (&optional end parse-at-point-fns)
  "If there is another element, return it and leave point after it.

An element is a plist containing data about a regexp match for a
section of the buffer that Xenops can do something to. Optional
argument END limits the search. Optional PARSE-AT-POINT-FNS are a
subset of parsing functions to use."
  (let ((start-regexp (xenops-elements-delimiter-start-regexp))
        (end (or end (point-max)))
        (parse-at-point-fns (or parse-at-point-fns (xenops-elements-get-all :parser))))
    (-if-let* ((find (re-search-forward start-regexp end t))
               (visit-beg (goto-char (match-beginning 0)))
               (element (or (xenops-parse-any-element-at-point parse-at-point-fns)
                            'parse-failure))
               (visit-end (goto-char (if (eq element 'parse-failure) (1+ (point))
                                       (plist-get element :end)))))
        element)))

(defun xenops-apply-post-apply-deactivate-mark (handlers &optional _ __ region-active)
  "Deactivate mark when appropriate.

HANDLERS are the current handlers and are used to determine
whether deactivation is appropriate. REGION-ACTIVE is a boolean
indicating whether there is an active region.

`increase-size` and `decrease-size` are expected to be applied
multiple times, and we want to preserve the active region.
Otherwise, the region should be deactivated after operating on
it."
  (and region-active (not (-intersection handlers '(xenops-image-increase-size
                                                    xenops-image-decrease-size)))
       (deactivate-mark)))

(defun xenops-apply-post-reveal-delete-overlays (handlers &optional beg end _)
  "Delete any image overlays that may remain after a `reveal` operation.

HANDLERS are the current handlers and are used to determine
whether deletion is appropriate. BEG and END define the region
within which deletion occurs.

If the underlying textual representation of an element has become
malformed, `xenops-reveal' will fail to delete its overlay, which
makes this hook function necessary."
  ;; We require beg and end to be non-nil so that we only delete overlays in the appropriate
  ;; region. I.e. we will not delete overlays if the action was a reveal-at-point.
  (if (and beg end (-intersection handlers '(xenops-math-reveal
                                             xenops-element-reveal
                                             xenops-image-reveal)))
      (xenops-overlay-delete-overlays-in beg end)))

(add-hook 'xenops-apply-post-apply-hook #'xenops-apply-post-apply-deactivate-mark)
(add-hook 'xenops-apply-post-apply-hook #'xenops-apply-post-reveal-delete-overlays)

(provide 'xenops-apply)

;;; xenops-apply.el ends here
