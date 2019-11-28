(setq xenops-src-do-in-org-mode-header "* \n")

(defun xenops-src-activate ()
  (font-lock-add-keywords
   nil
   `((,(caar (xenops-elements-get 'src :delimiters))
      (0
       (xenops-src-apply-syntax-highlighting))))))

(defun xenops-src-parse-at-point ()
  (if-let ((element (xenops-parse-element-at-point 'src))
           (org-element (xenops-src-do-in-org-mode (org-element-context)))
           (org-babel-info (org-babel-get-src-block-info 'light org-element)))
      (xenops-util-plist-update
       element
       :type 'src
       :language (nth 0 org-babel-info)
       :org-babel-info org-babel-info)))

(defun xenops-src-execute (element)
  (let ((execute-src-block-fn (if (equal (plist-get element :language) "mathematica")
                                  #'xenops-src-execute-src-block:mathematica
                                #'xenops-src-execute-src-block)))
    (funcall execute-src-block-fn element)))

(defun xenops-src-execute-src-block (element)
  "Execute the src block in a temporary org-mode buffer and
insert the results in the LaTeX buffer."
  ;; TODO: `Use org-babel-insert-result'
  (let* ((case-fold-search t)
         (result (xenops-src-execute-parsed-src-block (plist-get element :org-babel-info))))
    (save-excursion
      (goto-char (plist-get element :end))
      (insert result))))

(defun xenops-src-execute-parsed-src-block (info)
  "Execute an org-babel src block from the parsed data structure
INFO. Return the results section that is written to the org-mode
buffer, as a string."
  (with-temp-buffer
    (org-mode)
    ;; TODO: Execute the block based on parsed `info' without writing it into the buffer.
    (insert (org-babel-exp-code info 'block))
    (org-babel-execute-src-block t info)
    (buffer-substring (point) (point-max))))

(defvar xenops-src-mathematica-latex-results-command
  "
f () {
  echo \\\\begin{align*}
  MathematicaScript -script \"$1\"
  echo \\\\end{align*}
}
f")

(defun xenops-src-execute-src-block:mathematica (element)
  (let* ((info (plist-get element :org-babel-info))
         (body (nth 1 info))
         (params (nth 2 info))
         (result-params (split-string (downcase (cdr (assq :results params)))))
         (latex-results (member "latex" result-params)))
    (if latex-results
        (let ((org-babel-mathematica-command xenops-src-mathematica-latex-results-command))
          (setf (nth 1 info) (concat body " // TeXForm // ToString" ))
          (setf (cdr (assq :results (nth 2 info))) "raw")
          (xenops-src-execute-src-block element)
          (save-excursion
            (search-forward "#+RESULTS:\n")
            (-if-let (element (xenops-math-parse-block-element-at-point))
                (xenops-math-render element))))
      (xenops-src-execute-src-block element))))

(defmacro xenops-src-do-in-org-mode (&rest body)
  `(save-restriction
     (progn
       (condition-case nil
           (org-narrow-to-block)
         (user-error nil))
       (let ((region (buffer-substring (point-min) (point-max))))
         (with-temp-buffer
           (erase-buffer)
           (insert xenops-src-do-in-org-mode-header)
           (insert region)
           (org-mode)
           ,@body)))))

(defun xenops-src-apply-syntax-highlighting ()
  (if-let* ((element (xenops-src-parse-at-point))
            (lang (plist-get element :language))
            (beg (plist-get element :begin))
            (end (plist-get element :end)))
      (org-src-font-lock-fontify-block lang beg end)))

(provide 'xenops-src)
