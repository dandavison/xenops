(defvar xenops-text-prettify-symbols
  '(("\\grad" . "∇")
    ("\\implies" . "⟹")
    ("\\sqrt" . "√")
    ("^2" . "²")
    ("^3" . "³")
    ("^n" . "ⁿ")))

(defvar xenops-text-prettify-symbols-string-replacements
  '(("\\begin{definition*}" . "Definition.")
    ("\\end{definition*}" . "┘")

    ("\\begin{theorem*}" . "Theorem.")
    ("\\end{theorem*}" . "┘")

    ("\\begin{theorem}" . "Theorem.")
    ("\\end{theorem}" . "┘")

    ("\\begin{proof}" . "Proof.")
    ("\\end{proof}" . "□")

    ("\\begin{align*}" . "⚡")
    ("\\end{align*}" . "⚡")

    ("\\begin{align}" . "⚡")
    ("\\end{align}" . "⚡")

    ("#+begin_src" . "⚡")
    ("#+end_src" . "⚡")

    ("\\begin{enumerate}" . "┐")
    ("\\end{enumerate}" . "┘")

    ("\\begin{itemize}" . "┐")
    ("\\end{itemize}" . "┘")

    ("\\begin{mdframed}" . "┐")
    ("\\end{mdframed}" . "┘")

    ("\\end{minted}" . "┘")
    ("\\end{verbatim}" . "┘")
    ("\\end{tabular}" . "┘")
    ("\\end{tabular*}" . "┘")

    ("\\newpage" .
     "-------------------------------------------------------------------------------------------------------------------")

    ("\\begin{quote}" . "“")
    ("\\end{quote}" . "”")

    ("\\item" . "⁃")
    ("\\section" . "§")
    ("\\subsection" . "§§")
    ("\\subsubsection" . "§§§")

    ("\\includegraphics" . "img")

    ;; post-spacing is incorrect for these when using
    ;; prettify-symbols-mode with the latex-unicode-math-mode
    ;; symbols. So using a string replacement with explicit
    ;; space.
    ;; TODO: combine string and single-character replacements cleanly.
    ("\\to " . "⟶ ")
    ("\\in " . "∈ ")))


(defun xenops-text-activate ()
  ;; TODO
  ;;
  ;; - Can we make \Delta consume the post-space? So that "\Delta r"
  ;;   has no space between them?
  ;; - Order of precedence? Sort longest (most specific) first?

  (let ((-compare-fn (lambda (pair1 pair2) (equal (car pair1) (car pair2)))))

    ;; Add custom single-character entries to default latex-mode entries.
    (setq prettify-symbols-alist
          (-union prettify-symbols-alist xenops-text-prettify-symbols))

    ;; Remove entries that will be overridden by string replacements
    (setq prettify-symbols-alist
          (-difference prettify-symbols-alist
                       xenops-text-prettify-symbols-string-replacements)))

  ;; Add string replacements.
  (mapc #'xenops-text-prettify-symbols-add-string-replacement
        xenops-text-prettify-symbols-string-replacements)

  ;; Activate.
  (prettify-symbols-mode))


;; https://emacs.stackexchange.com/a/34882/9007
(defun xenops-text-prettify-symbols-add-string-replacement (pair)
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


(provide 'xenops-text)
