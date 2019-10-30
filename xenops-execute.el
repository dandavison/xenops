(defun xenops-org-babel-execute-src-block ()
  (interactive)
  (let ((mode major-mode)
        (heading "* \n"))
    (org-narrow-to-block)
    (org-mode)
    ;; hack: org requires a heading in file
    (save-excursion
      (goto-char (point-min))
      (insert heading))
    (forward-char (length heading))
    (org-babel-execute-src-block-maybe)
    (save-excursion
      (goto-char (point-min))
      (delete-char (length heading)))
    (widen)
    (funcall mode)))
