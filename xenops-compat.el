;; This module contains copies of functions that Xenops uses from Emacs but that are not available
;; in all versions of Emacs that Xenops supports.

(unless (fboundp 'image-property)
  (defun image-property (image property)
    "Return the value of PROPERTY in IMAGE.
Properties can be set with

  (setf (image-property IMAGE PROPERTY) VALUE)
If VALUE is nil, PROPERTY is removed from IMAGE."
    (declare (gv-setter image--set-property))
    (plist-get (cdr image) property)))

(unless (fboundp 'org-element-property)
  (defsubst org-element-property (property element)
    "Extract the value from the PROPERTY of an ELEMENT."
    (if (stringp element) (get-text-property 0 property element)
      (plist-get (nth 1 element) property))))

(unless (fboundp 'org-element-type)
  (defsubst org-element-type (element)
    "Return type of ELEMENT.

The function returns the type of the element or object provided.
It can also return the following special value:
  `plain-text'       for a string
  `org-data'         for a complete document
  nil                in any other case."
    (cond
     ((not (consp element)) (and (stringp element) 'plain-text))
     ((symbolp (car element)) (car element)))))

(provide 'xenops-compat)
