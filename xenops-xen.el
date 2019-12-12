(require 'style)

(define-minor-mode xenops-xen-mode
  "Minimal visual appearance for a Xenops LaTeX buffer.

\\{xenops-xen-mode-map}"
  :lighter nil
  (cond
   (xenops-xen-mode
    (setq style-rules xenops-xen-style-rules
          style-regexp-rules-get-text-properties #'xenops-xen-style-regexp-rules-get-text-properties)
    (style-mode +1))
   (t
    (style-mode -1)
    (setq style-rules nil
          style-regexp-rules-get-text-properties nil))))

;; A brace-delimited multiline expression supporting 1 level of nesting: i.e. it can contain other
;; brace-delimited expressions, as long as these have no children.
(setq xenops-brace-delimited-multiline-expression-regexp
      (let ((brace-delimited-atom-regexp
             (concat
              "\\("
              "{"
              "[^{}]*"
              "}"
              "\\)")))
        (concat
         "{"
         "\\("
         "\\("
         "[^{}]*"
         brace-delimited-atom-regexp
         "\\)"
         "*"
         "[^{}]*"
         "\\)"
         "}")))

(defvar xenops-xen-style-rules
  '(("\\grad" . "∇")
    ("\\implies" . "⟹")
    ("\\iff" . "⟺")
    ("\\sqrt" . "√")
    ("^2" . "²")
    ("^3" . "³")
    ("^n" . "ⁿ")
    ("\\&" . "&")
    ("\\pm" . "±")
    ("\\mp" . "∓")
    ("\\to " . "⟶ ")
    ("\\in " . "∈ ")
    ("``" . "“")
    ("''" . "”")

    ("\\\\begin{\\([^}*\n]+\\)\\*?}" . xenops-xen-begin-latex-environment-formatter)
    ("\\\\end{\\([^}*\n]+\\)\\*?}" . xenops-xen-end-latex-environment-formatter)

    ("\\begin{quote}" . "“")
    ("\\end{quote}" . "”")

    "\\url{\\([^}*\n]+\\)}"

    ("\\item" . "⁃")
    ("\\includegraphics" . "img")

    "\\\\emph{\\([^\n}]+\\)}"
    "\\\\textbf{\\([^\n}]+\\)}"
    "\\\\textit{\\([^\n}]+\\)}"
    ("\\\\\\(?:sub\\)*section\\*?{\\([^\n}]+\\)}" . xenops-xen-section-title-formatter)
    "{\\\\bf +\\([^\n}]+\\)}"
    "{\\\\it +\\([^\n}]+\\)}"))

(defun xenops-xen-begin-latex-environment-formatter (env)
  (cond
   ((member env '("src" "minted" "align")) "⚡")
   ((member env '("enumerate" "itemize" "mdframed")) " ")
   (t (upcase-initials env))))

(defun xenops-xen-end-latex-environment-formatter (env)
  (cond
   ((member env '("align" "src" "minted")) "⚡")
   ((member env '("proof")) "□")
   (t " ")))

(defun xenops-xen-section-title-formatter (title)
  (let* ((match (match-string 0))
         (indent (cond
                  ((string-match "^\\\\subsubsection" match)
                   8)
                  ((string-match "^\\\\subsection" match)
                   4)
                  ((string-match "^\\\\section" match)
                   0))))
    (format "%s%s" (s-repeat indent " ") title)))

(defun xenops-xen-style-regexp-rules-get-text-properties (match)
  "An implementation of `style-regexp-rules-get-text-properties'."
  (cond
   ((string-match "\\(\\\\textbf{\\|{\\\\bf \\)" match)
    '(face bold))
   ((string-match "\\(\\\\textit{\\|\\\\emph{\\|{\\\\it \\)" match)
    '(face italic))
   ((string-match "\\\\subsubsection" match)
    '(face font-latex-sectioning-4-face))
   ((string-match "\\\\subsection" match)
    '(face font-latex-sectioning-3-face))
   ((string-match "\\\\section" match)
    '(face font-latex-sectioning-2-face))
   ((string-match "\\\\url" match)
    '(face underline))))

(provide 'xenops-xen)
