;; Generic operations on elements. Functions in this file should determine behavior based on the
;; type of the element and the `xenops-elements' data structure. They should not directly call
;; functions that are specific to element type (e.g. in xenops-math, xenops-image, xenops-text).

(defun xenops-apply (ops)
  "Apply operations OPS to any elements encountered. The region
operated on is either the element at point, the active region, or
the entire buffer."
  (cl-flet ((process (lambda (el)
                       (-if-let (op (xenops-element-get-matching-operation el ops))
                           (save-excursion (funcall op el))))))
    (-if-let (el (xenops-parse-element-at-point))
        (process el)
      (destructuring-bind (beg end region-active)
          (if (region-active-p)
              `(,(region-beginning) ,(region-end) t)
            `(,(point-min) ,(point-max) nil))
        (save-excursion
          (goto-char beg)
          (let (el)
            (while (setq el (xenops-element-get-next-element end))
              (process el))))
        ;; Hack: This should be abstracted.
        (and region-active (not (-intersection ops '(xenops-math-image-increase-size
                                                     xenops-math-image-decrease-size)))
             (deactivate-mark))))))

(defun xenops-element-get-next-element (end)
  "If there is another element, return it and leave point after it.
An element is a plist containing data about a regexp match for a
section of the buffer that xenops can do something to."
  (cl-flet ((next-match-pos (regexp)
                            (save-excursion
                              (if (re-search-forward regexp end t) (match-beginning 0) end))))
    (let ((element (-min-by (lambda (delims1 delims2)
                              (> (next-match-pos (car (plist-get delims1 :delimiters)))
                                 (next-match-pos (car (plist-get delims2 :delimiters)))))
                            (xenops-element-get-delimiters))))
      (when (re-search-forward (car (plist-get element :delimiters)) end t)
        (let* ((type (plist-get element :type))
               (parser (xenops-element-get type :parser))
               (element (funcall parser element)))
          ;; TODO: This shouldn't be necessary but it sometimes gets
          ;; stuck attempting to process the same block repeatedly.
          (goto-char (plist-get element :end))
          element)))))

(defun xenops-element-get-matching-operation (el ops)
  (car (-intersection ops (xenops-element-get (plist-get el :type) :ops))))

(defun xenops-element-get-delimiters ()
  (cl-flet ((get-delimiters (type)
                            (mapcar (lambda (delimiters)
                                      `(:type ,type :delimiters ,delimiters))
                                    (xenops-element-get type :delimiters))))
    (apply #'append (mapcar #'get-delimiters (mapcar #'car xenops-elements)))))

(defun xenops-element-get (type key)
  "Return the value associated with KEY for element type TYPE."
  (let ((value (plist-get (cdr (assq type xenops-elements)) key)))
    (if (and (symbolp value) (assq value xenops-elements))
        ;; Instead of a real entry, an element type may name another element type, meaning: use
        ;; that element type's entry.
        (xenops-element-get value key)
      value)))

(provide 'xenops-element)
