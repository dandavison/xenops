(defun xenops-minted-parse-at-point ()
  (if-let (element (xenops-parse-element-at-point 'minted))
      (let* ((language (xenops-minted-get-babel-language (match-string 2)))
             (body (buffer-substring (plist-get element :begin-content)
                                     (plist-get element :end-content)))
             (org-babel-info
              (list language body '((:results . "latex replace")))))
        (xenops-util-plist-update
         element
         :language language
         :org-babel-info org-babel-info))))

(defvar xenops-minted-language-to-babel-language
  '(("wolfram" . "mathematica")))

(defun xenops-minted-get-babel-language (language)
  (or (cdr (assoc language xenops-minted-language-to-babel-language))
      language))

(provide 'xenops-minted)
