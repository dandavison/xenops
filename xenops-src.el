;;; xenops-src.el --- Utilities for working with elements of type 'src -*- lexical-binding: t; -*-

;;; Commentary:

;; Elements of type 'src represent alien org-mode 'src blocks in a LaTeX buffer. They contain code.
;; Xenops enables execution (via org-babel) and syntax highlighting of elements of type 'minted and
;; 'src.

;;; Code:

(defvar xenops-src-mathematica-use-wolframscript t
  "If non-nil then use the `wolframscript` executable to execute
mathematica code; otherwise use `MathematicaScript`. Note that
graphics cannot be saved to file reliably using MathematicaScript
under some OSs / Mathematica versions.")

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

(defun xenops-src-execute-src-block:python (element)
  "Execute python src block, with special setup if :sympy header argument is set."
  (let ((info (plist-get element :org-babel-info)))
    (if (cdr (assq :sympy (nth 2 info)))
        (xenops-src-execute-src-block:python-sympy element)
      (xenops-src-execute-src-block element))))

(defun xenops-src-execute-src-block:python-sympy (element)
  "Execute python sympy src block. Add an `import * from sympy` line and,
if `:results latex`, arrange for sympy to return the results as
LaTeX."
  (let ((org-babel-python-wrapper-method
         (concat "from sympy import *\n" org-babel-python-wrapper-method)))
    (if (xenops-src-latex-results? element)
        (let* ((info (plist-get element :org-babel-info))
               (body (substring-no-properties (nth 1 info)))
               (lines (s-split "\n" body t))
               (last-line (car (last lines))))
          (when (string-match "[ \t]*return[ \t]*\\(.+\\)" last-line)
            (setq lines (push "from sympy import latex as __sympy_latex__" lines))
            (setf (car (last lines))
                  (replace-match "return __sympy_latex__(\\1)" t nil last-line))
            (setf (nth 1 info) (s-join "\n" lines)))))
    (xenops-src-execute-src-block element)))

(defun xenops-src-execute-src-block:mathematica (element)
  "Execute mathematica src block. If `:results latex`, arrange
for mathematica to return the result as LaTeX."
  (let* ((info (plist-get element :org-babel-info))
         (body (nth 1 info)))
    (cond ((xenops-src-latex-results? element)
           (setf (nth 1 info) (concat body " // TeXForm // ToString" )))
          ((xenops-src-image-results? element)
           (let* ((file (or (cdr (assq :file (nth 2 info)))
                            (xenops-image-suggest-file-name ".png")))
                  (lines (s-split "\n" body t))
                  (last-line (car (last lines))))
             (setf (car (last lines))
                   (format "Export[\"%s\", %s, ImageResolution -> 144]" file last-line))
             (setf (nth 1 info) (s-join "\n" lines))))))
  (let ((org-babel-mathematica-command
         (if xenops-src-mathematica-use-wolframscript
             "wolframscript -file"
           org-babel-mathematica-command)))
    (xenops-src-execute-src-block element)))

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
  (cond ((xenops-src-latex-results? element)
         (xenops-src-post-process-latex-result element))
        ((xenops-src-image-results? element)
         (xenops-src-post-process-image-result element))))

(defun xenops-src-post-process-latex-result (element)
  (let* ((lang (downcase (plist-get element :language)))
         (wrap-in-align-environment (member lang '("python" "mathematica"))))
    (save-excursion
      (when (re-search-forward
             "#\\+RESULTS:\n#\\+BEGIN_EXPORT latex\\(\\(\n.*?\\)*\\)#\\+END_EXPORT\n" nil t)
        (replace-match
         (if wrap-in-align-environment "\\\\begin{align*}\\1\\\\end{align*}" "\\1") t)))
    (save-excursion
      (-if-let* ((element (xenops-apply-parse-next-element)))
          (xenops-element-do element 'render)))))

(defun xenops-src-post-process-image-result (element)
  (let* ((lang (downcase (plist-get element :language))))
    (save-excursion
      (when (re-search-forward
             "#\\+RESULTS:\n\\[\\[file:\\([^]\n]+\\)\\]\\]" nil t)
        (replace-match
         "\\\\includegraphics{\\1}" t)))
    (save-excursion
      (-if-let* ((element (xenops-apply-parse-next-element)))
          (xenops-element-do element 'render)))))

(defun xenops-src-org-babel-result-params (element)
  (let* ((info (plist-get element :org-babel-info))
         (result-params (cdr (assq :results (nth 2 info)))))
    (split-string (downcase result-params))))

(defun xenops-src-latex-results? (element)
  (member "latex" (xenops-src-org-babel-result-params element)))

(defun xenops-src-image-results? (element)
  (member "graphics" (xenops-src-org-babel-result-params element)))

(defun xenops-src-execute-parsed-src-block (info)
  "Execute an org-babel src block from the parsed data structure
INFO. Return the results section that is written to the org-mode
buffer, as a string."
  (with-temp-buffer
    (org-mode)
    ;; TODO: Execute the block based on parsed `info' without writing it into the buffer.
    (insert (org-babel-exp-code info 'block))
    (org-babel-execute-src-block 'ignore-cached info)
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

;;; xenops-src.el ends here
