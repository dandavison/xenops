(defun xenops-elements-get (type key)
  "Return the value associated with KEY for element type TYPE."
  (let ((value (cdr (assq key (cdr (assq type xenops-elements))))))
    (if (and (symbolp value) (assq value xenops-elements))
        ;; Instead of a real entry, an element type may name another element type, meaning: use
        ;; that element type's entry.
        (xenops-elements-get value key)
      value)))

(defun xenops-elements-get-for-types (key types)
  "Concatenated list of all items under key KEY for any type in
TYPES. If TYPES is 'all, then all items under key KEY for any
type."
  (-uniq
   (apply #'append
          (loop for (type _) in xenops-elements
                collecting (and (or (eq types 'all) (memq type types))
                                (let ((val (xenops-elements-get type key)))
                                  (if (listp val) val (list val))))))))

(defun xenops-elements-get-all (key)
  "Concatenated list of all items under key KEY for any element type."
  (xenops-elements-get-for-types key 'all))

(defun xenops-elements-delimiter-start-regexp (&optional types)
  "A regexp matching the start of any element."
  (format "\\(%s\\)"
          (s-join "\\|"
                  (mapcar #'car (xenops-elements-get-for-types :delimiters (or types 'all))))))

(provide 'xenops-elements)
