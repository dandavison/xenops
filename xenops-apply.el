;; -*- lexical-binding: t -*-

(setq xenops-apply-post-apply-hook nil)

(defmacro xenops-define-apply-command (op docstring)
  `(defun ,(intern (concat "xenops-" (symbol-name op))) ()
     ,(concat docstring "\n\n"
              "The elements operated on are determined by trying the following:
1. The element at point, if any.
2. Elements in the active region, if there is an active region.
3. All elements in the buffer.")
     (interactive)
     (xenops-apply '(,op))))

(defmacro xenops-define-apply-at-point-command (op docstring)
  `(defun ,(intern (concat "xenops-" (symbol-name op) "-at-point")) ()
     ,docstring
     (interactive)
     (xenops-apply-at-point '(,op))))

(defun xenops-apply (ops &optional pred)
  "Apply operation types OPS to any elements encountered.

The region operated on is either the active region, or the entire
buffer.

Optional argument PRED is a function taking an element plist as
its only argument. The element will be operated on iff PRED
returns non-nil."
  (let ((handlers (xenops-ops-get-for-ops ops :handlers)))
    (cl-destructuring-bind (beg end region-active)
        (if (region-active-p)
            `(,(region-beginning) ,(region-end) t)
          `(,(point-min) ,(point-max) nil))
      (xenops-apply-handlers handlers beg end region-active pred)
      (run-hook-with-args 'xenops-apply-post-apply-hook handlers beg end region-active))))

(defun xenops-apply-handlers (handlers beg end region-active &optional pred)
  "Apply HANDLERS to any elements encountered.

The region operated on is either the active region, or the entire
buffer."
  (cl-flet ((handle (lambda (el) (save-excursion
                              (xenops-element-dispatch el handlers)))))
    (save-excursion
      (goto-char beg)
      (let ((parse-at-point-fns (xenops-elements-get-all :parser))
            (sem-start-value (aref xenops-math-latex-tasks-semaphore 1)))
        (while (setq el (xenops-apply-parse-next-element nil end parse-at-point-fns))
          (and el
               (or (null pred) (funcall pred el))
               (ignore-errors (handle el))))
        (if (-intersection handlers '(xenops-math-render))
            (message "Started %d latex processing tasks"
                     (- sem-start-value
                        (aref xenops-math-latex-tasks-semaphore 1))))))))

(defun xenops-apply-at-point (ops &optional pred)
  "Apply operation types OPS to element at point, if there is one."
  (let ((handlers (xenops-ops-get-for-ops ops :handlers)))
    (xenops-apply-handlers-at-point handlers pred)
    (run-hook-with-args 'xenops-apply-post-apply-hook handlers))
  t)

(defun xenops-apply-handlers-at-point (handlers &optional pred)
  "Apply HANDLERS to element at point if there is one."
  (-if-let* ((el (xenops-parse-any-element-at-point)))
      (xenops-element-dispatch el handlers)))

(defun xenops-apply-parse-next-element (&optional start-regexp end parse-at-point-fns)
  "If there is another element, return it and leave point after it.
An element is a plist containing data about a regexp match for a
section of the buffer that Xenops can do something to."
  (let ((start-regexp (or start-regexp (xenops-elements-delimiter-start-regexp)))
        (end (or end (point-max)))
        (parse-at-point-fns (or parse-at-point-fns (xenops-elements-get-all :parser))))
    (-if-let* ((_ (re-search-forward start-regexp end t))
               (_ (goto-char (match-beginning 0)))
               (element (xenops-parse-any-element-at-point parse-at-point-fns))
               (_ (goto-char (plist-get element :end))))
        element)))


(defun xenops-apply-post-apply-deactivate-mark (handlers &optional beg end region-active)
  "Deactivate mark when appropriate.

`increase-size` and `decrease-size` are expected to be applied
multiple times, and we want to preserve the active region.
Otherwise, the region should be deactivated after operating on
it."
  (and region-active (not (-intersection handlers '(xenops-image-increase-size
                                                    xenops-image-decrease-size)))
       (deactivate-mark)))

(add-hook 'xenops-apply-post-apply-hook #'xenops-apply-post-apply-deactivate-mark)

(provide 'xenops-apply)
