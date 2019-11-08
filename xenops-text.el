(defvar xenops-text-prettify-symbols
  '(("\\grad" . "∇")
    ("\\implies" . "⟹")
    ("\\iff" . "⟺")
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


(defvar xenops-text-prettify-symbols-regexp-replacements
  ;; TODO Use an alist like the others.
  (format
   "\\(%s\\)"
   (s-join
    "\\|"
    '("\\\\emph{\\([^}]+\\)}"))))

(defun xenops-text-activate ()
  ;; TODO
  ;;
  ;; - Can we make \Delta consume the post-space? So that "\Delta r"
  ;;   has no space between them?
  ;; - Order of precedence? Sort longest (most specific) first?

  (let ((-compare-fn (lambda (pair1 pair2) (equal (car pair1) (car pair2)))))

    ;; Add custom single-character entries to default latex-mode entries.
    (setq prettify-symbols-alist
          (-union xenops-text-prettify-symbols prettify-symbols-alist))

    ;; Remove entries that will be overridden by string replacements
    (setq prettify-symbols-alist
          (-difference prettify-symbols-alist
                       xenops-text-prettify-symbols-string-replacements)))

  ;; Add string replacements.
  (mapc #'xenops-text-prettify-symbols-add-string-replacement
        xenops-text-prettify-symbols-string-replacements)

  (prettify-symbols-mode)

  ;; Prettify-symbols replacements using captured text
  (font-lock-add-keywords
   nil
   `((,xenops-text-prettify-symbols-regexp-replacements
      (0
       (xenops-text-prettify-regexp-replacement)))))

  ;; Add tooltips to prettify replacements
  ;; TODO: I think this is causing the very long regexp to be matched twice during fontification.
  ;; Can this be done by modifying the existing prettify-symbols entry?
  (xenops-text-add-tooltips (caar prettify-symbols--keywords))
  (xenops-text-add-tooltips xenops-text-prettify-symbols-regexp-replacements))


(defun xenops-text-add-tooltips (regexp)
  (font-lock-add-keywords
   nil
   `((,regexp
      0 `(face font-lock-keyword-face
               help-echo ,(match-string 0))))))

(defun xenops-text-prettify-regexp-replacement ()
  (let ((string (save-match-data (s-join " " (split-string (match-string 2))))))
    (xenops-text-prettify-symbols-compose
     (xenops-text-make-composition string)))
  nil)

(defun xenops-text-prettify-symbols-compose (components)
  "Taken from `prettify-symbols--compose-symbol'"
  ;; TODO: look at defensive measures in that function.
  (let ((start (match-beginning 0))
        (end (match-end 0)))
    (with-silent-modifications
      (compose-region start end components)
      (add-text-properties
       start end
       `(prettify-symbols-start ,start prettify-symbols-end ,end)))))

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
              (xenops-text-make-composition (cdr pair)))
        prettify-symbols-alist))

(defun xenops-text-make-composition (string)
  (let ((composition nil))
    (dolist (char (string-to-list string)
                  (nreverse (cdr composition)))
      (push char composition)
      (push '(Br . Bl) composition))))

(provide 'xenops-text)
