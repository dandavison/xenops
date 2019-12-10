(setq xenops-src-do-in-org-mode-header "* \n")

(defun xenops-src-parse-at-point ()
  (-if-let* ((element (xenops-parse-element-at-point 'src))
             (org-element (xenops-src-do-in-org-mode (org-element-context)))
             (org-babel-info (org-babel-get-src-block-info 'light org-element)))
      (xenops-util-plist-update
       element
       :type 'src
       :language (nth 0 org-babel-info)
       :org-babel-info org-babel-info)))

(defun xenops-src-execute (element)
  "ELEMENT is an org-babel src block. Execute it using
`org-babel-execute-src-block'."
  (let* ((language (plist-get element :language))
         (execute-language-fn
          (intern (concat "xenops-src-execute-src-block:" language))))
    (if (fboundp execute-language-fn)
        (funcall execute-language-fn element)
      (xenops-src-execute-src-block element))))

(defun xenops-src-execute-src-block:mathematica (element)
  "Execute mathematica src block. If `:results latex`, arrange
for mathematica to return the result as LaTeX."
  (if (xenops-src-latex-results? element)
      (let* ((info (plist-get element :org-babel-info))
             (body (nth 1 info)))
        (setf (nth 1 info) (concat body " // TeXForm // ToString" ))))
  (xenops-src-execute-src-block element))

(defun xenops-src-execute-src-block (element)
  "Execute the src block in a temporary org-mode buffer and
insert the results in the LaTeX buffer. If `:results latex`,
post-process by replacing the org-mode LaTeX export block (see
`org-babel-insert-result') with a LaTeX align environment."
  (let* ((case-fold-search t)
         (result (xenops-src-execute-parsed-src-block (plist-get element :org-babel-info))))
    (save-excursion
      ;; TODO: `Use org-babel-insert-result'
      (goto-char (plist-get element :end))
      (insert result)))
  (if (xenops-src-latex-results? element)
      (save-excursion
        (when (re-search-forward
               "#\\+RESULTS:\n#\\+BEGIN_EXPORT latex\\(\\(\n.*?\\)*\\)#\\+END_EXPORT\n" nil t)
          (replace-match "\\\\begin{align*}\\1\\\\end{align*}" t)
          (backward-char)
          (-if-let* ((element (xenops-math-parse-block-element-at-point)))
              (xenops-math-render element))))))

(defun xenops-src-latex-results? (element)
  (let* ((info (plist-get element :org-babel-info))
         (result-params (cdr (assq :results (nth 2 info)))))
    (member "latex" (split-string (downcase result-params)))))

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
  (-when-let* ((element (or (xenops-minted-parse-at-point)
                            (xenops-src-parse-at-point)))
               (lang (plist-get element :language))
               (beg (plist-get element :begin-content))
               (end (plist-get element :end-content)))
    (org-src-font-lock-fontify-block lang beg end)
    (add-face-text-property beg end 'fixed-pitch)))

(provide 'xenops-src)
