(require 'style)

(define-minor-mode xenops-xen-mode
  "Minimal visual appearance for a Xenops LaTeX buffer.

\\{xenops-xen-mode-map}"
  :lighter " xen"
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
    ("\\begin{abstract}" . "Abstract.")
    ("\\end{abstract}" . "┘")

    ("\\begin{definition}" . "Definition.")
    ("\\end{definition}" . "┘")

    ("\\begin{definition*}" . "Definition.")
    ("\\end{definition*}" . "┘")

    ("\\begin{theorem*}" . "Theorem.")
    ("\\end{theorem*}" . "┘")

    ("\\begin{theorem}" . "Theorem.")
    ("\\end{theorem}" . "┘")

    ("\\begin{lemma*}" . "Lemma.")
    ("\\end{lemma*}" . "┘")

    ("\\begin{lemma}" . "Lemma.")
    ("\\end{lemma}" . "┘")

    ("\\begin{proof}" . "Proof.")
    ("\\end{proof}" . "□")

    ("\\begin{align*}" . "⚡")
    ("\\end{align*}" . "⚡")

    ("\\begin{align}" . "⚡")
    ("\\end{align}" . "⚡")

    ("\\begin{minted}" . "⚡")
    ("\\end{minted}" . "⚡")

    ("#+begin_src" . "⚡")
    ("#+end_src" . "⚡")

    ("\\begin{cases}" . "cases:")
    ("\\end{cases}" . "┘")

    ("\\begin{enumerate}" . "┐")
    ("\\end{enumerate}" . "┘")

    ("\\begin{itemize}" . "┐")
    ("\\end{itemize}" . "┘")

    ("\\begin{mdframed}" . "┐")
    ("\\end{mdframed}" . "┘")

    ("\\end{verbatim}" . "┘")
    ("\\end{tabular}" . "┘")
    ("\\end{tabular*}" . "┘")

    ("\\begin{quote}" . "“")
    ("\\end{quote}" . "”")

    ("\\item" . "⁃")
    ("\\includegraphics" . "img")

    ;; post-spacing is incorrect for these when using
    ;; prettify-symbols-mode with the latex-unicode-math-mode
    ;; symbols. So using a string replacement with explicit
    ;; space.
    ;; TODO: combine string and single-character replacements cleanly.
    ("\\to " . "⟶ ")
    ("\\in " . "∈ ")

    ;; Computed display-strings
    "\\\\emph{\\([^\n}]+\\)}"
    "\\\\textbf{\\([^\n}]+\\)}"
    "\\\\textit{\\([^\n}]+\\)}"
    "\\\\\\(?:sub\\)*section\\*?{\\([^\n}]+\\)}"
    "{\\\\bf +\\([^\n}]+\\)}"
    "{\\\\it +\\([^\n}]+\\)}"))

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
    '(face font-latex-sectioning-2-face))))

(provide 'xenops-xen)
