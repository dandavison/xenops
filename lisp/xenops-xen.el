;;; xenops-xen.el --- A alternative cleaner view of a LaTeX buffer -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:

(require 'xenops-style)

;; A brace-delimited multiline expression supporting 1 level of nesting: i.e. it can contain other
;; brace-delimited expressions, as long as these have no children.
(defvar xenops-brace-delimited-multiline-expression-regexp
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
    ("\\%" . "%")
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
    "{\\\\it +\\([^\n}]+\\)}")
  "Style rules for xenops xen-mode.")

(defvar xenops-xen-style-regexp-rules-get-text-properties-function
  'xenops-xen-style-regexp-rules-get-text-properties
  "`xenops-style-regexp-rules-get-text-properties-function' for xenops-xen mode.")

(define-minor-mode xenops-xen-mode
  "Minimal visual appearance for a Xenops LaTeX buffer.

\\{xenops-xen-mode-map}"
  :lighter nil
  (cond
   (xenops-xen-mode
    (setq xenops-style-rules
          xenops-xen-style-rules
          xenops-style-regexp-rules-get-text-properties-function
          xenops-xen-style-regexp-rules-get-text-properties-function)
    (xenops-style-mode +1))
   (t
    (xenops-style-mode -1)
    (setq xenops-style-rules nil
          xenops-style-regexp-rules-get-text-properties-function nil))))

(defun xenops-xen-begin-latex-environment-formatter (env)
  "Return visual replacement for environment ENV begin token."
  (cond
   ((member env '("src" "minted" "align")) "⚡")
   ((member env '("enumerate" "itemize" "mdframed")) "↴")
   (t env)))

(defun xenops-xen-end-latex-environment-formatter (env)
  "Return visual replacement for environment ENV end token."
  (cond
   ((member env '("align" "src" "minted")) "⚡")
   ((member env '("proof")) "□")
   (t "↲")))

(defun xenops-xen-section-title-formatter (title)
  "Return visual replacement for section title TITLE."
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
  "An implementation of `xenops-style-regexp-rules-get-text-properties-function'.

MATCH is the current regular expression match."
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

;;; xenops-xen.el ends here
