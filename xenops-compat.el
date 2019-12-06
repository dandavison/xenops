(unless (fboundp 'image-property)
  (defun image-property (image property)
    "Return the value of PROPERTY in IMAGE.
Properties can be set with

  (setf (image-property IMAGE PROPERTY) VALUE)
If VALUE is nil, PROPERTY is removed from IMAGE."
    (declare (gv-setter image--set-property))
    (plist-get (cdr image) property)))

(provide 'xenops-compat)
