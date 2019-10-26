(defvar xenops-latex-prettify-symbols-alist-extra
      '(("\\R" . "ℝ")
        ("\\N" . "ℕ")
        ("\\C" . "ℂ")
        ("\\Q" . "ℚ")
        ("\\grad" . "∇")
        ("\\sqrt" . "√")
        ;; https://unicode-table.com/en/0307/
        ;; U+0307 Combining Dot Above
        ("\\ddot{\\r}" . "r̈")
        ("\\dot{\\r}" . "ṙ")
        ("\\dot{\\v}" . "v̇")
        ("\\dot{x}" . "ẋ")
        ("\\dot{y}" . "ẏ")
        ("\\xdot" . "ẋ")
        ("\\ydot" . "ẏ")
        ("\\\\" . "")  ;; "⏎"
        ("``" . "\"")
        ("''" . "\"")
        ("^2" . "²")
        ("^3" . "³")
        ("$" . "​")  ;; zero-width space
        (" ~ " . " ")))


(defvar xenops-latex-prettify-symbols-string-replacements
      '(("\\begin{definition*}" . "Definition.")
        ("\\end{definition*}" . "┘")

        ("\\begin{theorem*}" . "Theorem.")
        ("\\end{theorem*}" . "┘")

        ("\\begin{theorem}" . "Theorem.")
        ("\\end{theorem}" . "┘")

        ("\\begin{claim*}" . "Claim.")
        ("\\end{claim*}" . "┘")

        ("\\begin{question*}" . "Question.")
        ("\\end{question*}" . "┘")

        ("\\begin{example*}" . "Example.")
        ("\\end{example*}" . "┘")

        ("\\begin{proof}" . "Proof.")
        ("\\end{proof}" . "□")

        ("\\begin{align*}" . "⚡")
        ("\\end{align*}" . "⚡")

        ("\\begin{align}" . "⚡")
        ("\\end{align}" . "⚡")

        ("#+begin_src" . "⚡")
        ("#+end_src" . "⚡")

        ("\\begin{enumerate}" . "┐")
        ("\\begin{enumerate}[label=(\alph*)]" . "┐")
        ("\\end{enumerate}" . "┘")

        ("\\begin{itemize}" . "┐")
        ("\\end{itemize}" . "┘")

        ("\\begin{mdframed}" . "┐")
        ("\\end{mdframed}" . "┘")

        ("\\end{minted}" . "┘")
        ("\\end{verbatim}" . "┘")
        ("\\end{tabular}" . "┘")
        ("\\end{tabular*}" . "┘")

        ("\\begin{comment}  % latex-focus" .
         "\\begin{comment}  % latex-focus ⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯")
        ("\\end{comment}  % latex-focus" .
         "\\end{comment}  % latex-focus ⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯")

        ("\\newpage" .
         "-------------------------------------------------------------------------------------------------------------------")

        ("\\begin{quote}" . "“")
        ("\\end{quote}" . "”")
        ("\\item" . "⁃")
        ("\\section" . "§")
        ("\\subsection" . "§§")
        ("\\subsubsection" . "§§§")

        ("\\correct" . "☑")
        ("\\todo" . "TODO")
        ("\\includegraphics" . "img")

        ("\\vecMMM" . "\\vec")
        ("\\bvecMMM" . "\\vec")

        ;; TODO: DNW?
        ("\\Bigg[" . "[")
        ("\\Bigg]" . "]")
        ("\\Bigg(" . "(")
        ("\\Bigg)" . ")")

        ;; post-spacing is incorrect for these when using
        ;; prettify-symbols-mode with the latex-unicode-math-mode
        ;; symbols. So using a string replacement with explicit
        ;; space.
        ;; TODO: combine string and single-character replacements cleanly.
        ("\\to " . "→ ")
        ("\\in " . "∈ ")

        ("&=" . "=")
        ("\\dt" . "dt")
        ("\\dx" . "dx")
        ("\\dy" . "dy")

        ("\\d\\r" . "dr")
        ;; TODO: dangerous?, will this clash with anything starting with \r?
        ("\\r" . "r")
        ("\\v" . "v")
        ("\\F" . "F")))


(defun xenops-latex-prettify-symbols-mode ()
  (interactive)

  ;; TODO
  ;;
  ;; - Can we make \Delta consume the post-space? So that "\Delta r"
  ;;   has no space between them?
  ;; - Order of precedence? Sort longest (most specific) first?

  (let ((-compare-fn (lambda (pair1 pair2) (equal (car pair1) (car pair2)))))

    ;; Add custom single-character entries to default latex-mode entries.
    (setq prettify-symbols-alist
          (-union prettify-symbols-alist xenops-latex-prettify-symbols-alist-extra))

    ;; Remove entries that will be overridden by string replacements
    (setq prettify-symbols-alist
          (-difference prettify-symbols-alist
                       xenops-latex-prettify-symbols-string-replacements)))

  ;; Add string replacements.
  (mapc #'xenops-prettify-symbols-add-string-replacement
        xenops-latex-prettify-symbols-string-replacements)

  ;; Activate.
  (prettify-symbols-mode))


;; https://emacs.stackexchange.com/a/34882/9007
(defun xenops-prettify-symbols-add-string-replacement (pair)
  "Make `prettify-symbols-mode' replace string FROM with string TO.

Updates `prettify-symbols-alist'.  You may need to toggle
`prettify-symbols-mode' to make the changes take effect.

Each character of TO is vertically aligned using the baseline,
such that base-left of the character is aligned with base-right
of the preceding character.  Refer to `reference-point-alist'
for more information."
  (push (cons (car pair)
              (let ((composition nil))
                (dolist (char (string-to-list (cdr pair))
                              (nreverse (cdr composition)))
                  (push char composition)
                  (push '(Br . Bl) composition))))
        prettify-symbols-alist))


(provide 'xenops-prettify-symbols)
