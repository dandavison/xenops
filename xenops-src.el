(defun xenops-src-parse-at-point ()
  (if-let ((org-element (xenops-src-do-in-org-mode (org-element-context)))
           (org-babel-info (org-babel-get-src-block-info 'light org-element)))
      (list :type 'src
            :begin (plist-get org-element :begin)
            :end (plist-get org-element :end)
            :language (nth 0 org-babel-info)
            :org-babel-info org-babel-info)))

(defun xenops-src-execute (element)
  (let ((execute-src-block-fn (if (equal (plist-get element :language) "mathematica")
                                  #'xenops-src-execute-src-block:mathematica
                                #'xenops-src-execute-src-block)))
    (funcall execute-src-block-fn nil (plist-get element :org-babel-info))))

(defun xenops-src-execute-src-block (arg info)
  "Execute the src block in a temporary org-mode buffer and
insert the results in the LaTeX buffer."
  ;; TODO: `Use org-babel-insert-result'
  (let* ((case-fold-search t)
         (end-block-regexp "^[ \t]*#\\+end_.*")
         (result-text (xenops-src-do-in-org-mode
                       (org-babel-execute-src-block arg info)
                       (re-search-forward end-block-regexp nil t)
                       (buffer-substring (point) (point-max)))))
    (save-excursion
      (re-search-forward end-block-regexp)
      (insert result-text))))

(defvar xenops-src-mathematica-latex-results-command
  "
f () {
  echo \\\\begin{align*}
  MathematicaScript -noprompt -run < \"$1\"
  echo \\\\end{align*}
}
f")

(defun xenops-src-execute-src-block:mathematica (arg info)
  (let* ((body (nth 1 info))
         (params (nth 2 info))
         (result-params (split-string (downcase (cdr (assq :results params)))))
         (latex-results (member "latex" result-params)))
    (if latex-results
        (let ((org-babel-mathematica-command xenops-src-mathematica-latex-results-command))
          (setf (nth 1 info) (concat body " // TeXForm" ))
          (setf (cdr (assq :results (nth 2 info))) "raw")
          (xenops-src-execute-src-block arg info)
          (save-excursion
            (search-forward "#+RESULTS:\n")
            (-if-let (element (xenops-math-parse-block-element-at-point))
                (xenops-math-render element))))
      (xenops-src-execute-src-block arg info))))

(defmacro xenops-src-do-in-org-mode (&rest body)
  `(save-restriction
     (progn
       (condition-case nil
           (org-narrow-to-block)
         (user-error nil))
       (let ((region (buffer-substring (point-min) (point-max))))
         (with-temp-buffer
           (erase-buffer)
           (insert "* \n")
           (insert region)
           (org-mode)
           ,@body)))))

(provide 'xenops-src)
