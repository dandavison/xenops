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

    ("\\begin{cases}" . "cases:")
    ("\\end{cases}" . "┘")

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
    '("\\\\emph{\\([^}]+\\)}"
      "\\\\textbf{\\([^}]+\\)}"
      "\\\\textit{\\([^}]+\\)}"
      "{\\\\bf +\\([^}]+\\)}"
      "{\\\\it +\\([^}]+\\)}"))))

(setq xenops-text-tooltip-delay-orig nil)

;; Prettify-symbols replacements using captured text
(setq xenops-text-prettify-symbols-font-lock-keywords
      `((,xenops-text-prettify-symbols-regexp-replacements
         (0
          (xenops-text-prettify-regexp-replacement)))))

(defun xenops-text-activate ()
  (font-lock-add-keywords nil xenops-text-prettify-symbols-font-lock-keywords)
  (xenops-text-prettify-symbols-mode)
  (xenops-text-configure-tooltips))

(defun xenops-text-deactivate ()
  (font-lock-remove-keywords nil xenops-text-prettify-symbols-font-lock-keywords)
  (xenops-text-configure-tooltips 'deactivate)
  (xenops-text-prettify-symbols-mode 'deactivate)
  (org-restart-font-lock))

(defun xenops-text-prettify-symbols-mode (&optional deactivate)
  (if deactivate (prettify-symbols-mode -1)
    (let ((prettify-symbols-alist prettify-symbols-alist)
          (-compare-fn (lambda (pair1 pair2) (equal (car pair1) (car pair2)))))

      ;; Add custom single-character entries to default latex-mode entries.
      (setq prettify-symbols-alist
            (-union xenops-text-prettify-symbols prettify-symbols-alist))

      ;; Remove entries that will be overridden by string replacements
      (setq prettify-symbols-alist
            (-difference prettify-symbols-alist
                         xenops-text-prettify-symbols-string-replacements))

      ;; Add string replacements.
      (mapc #'xenops-text-prettify-symbols-add-string-replacement
            xenops-text-prettify-symbols-string-replacements)

      (prettify-symbols-mode))))

(defun xenops-text-configure-tooltips (&optional deactivate)
  ;; Add tooltips to prettify replacements
  ;; TODO: I think this is causing the very long regexp to be matched twice during fontification.
  ;; Can this be done by modifying the existing prettify-symbols entry?
  (dolist (regexp (list (caar prettify-symbols--keywords)
                        xenops-text-prettify-symbols-regexp-replacements))
    (funcall
     (if deactivate 'font-lock-remove-keywords 'font-lock-add-keywords)
     nil
     `((,regexp
        0 `(face font-lock-keyword-face
                 help-echo ,(match-string 0))))))
  (if deactivate
      (setq tooltip-delay xenops-text-tooltip-delay-orig)
    (setq xenops-text-tooltip-delay-orig tooltip-delay)))

(defun xenops-text-prettify-regexp-replacement ()
  "A match for a regexp capture replacement has just been made.
Return the replacement text to be displayed, with any text properties."
  (let* ((beg (match-beginning 0))
         (end (match-end 0))
         (match (match-string 0))
         (capture (s-join " " (split-string (xenops-text-prettify-regexp-get-match-capture))))
         (composition (xenops-text-make-composition capture))
         (properties (xenops-text-prettify-regexp-get-text-properties match)))
    (xenops-text-prettify-symbols-compose composition properties beg end))
  nil)

(defun xenops-text-prettify-regexp-get-match-capture ()
  "A match for a regexp capture replacement has just been made.
Return the captured text."
  ;; The regexp is like (option_1(captured)|option_2(captured)|...).
  ;; Note that there is an "outer union expression" followed by N "options", each of which has its
  ;; own capture group.
  ;; So the indices of match-data contain the following:
  ;;
  ;; 0 . whole string beg (match-string 0)
  ;; 1 . whole string end
  ;; 2 . outer union beg (match-string 1)
  ;; 3 . outer union end
  ;; 4 0 option_1 beg (match-string 2)
  ;; 5 1 option_1 end
  ;; 6 2 option_2 beg (match-string 3)
  ;; 7 3 option_2 end
  ;;
  ;; We ignore the first 4 indices and start counting at index 4.  Then, we find the first `beg`
  ;; index that is non-nil. Suppose this is i. Then (i + 4)/2 is the corresponding match-string
  ;; index.
  (match-string (/ (+ (xenops-util-first-index (-drop 4 (match-data 'integers))) 4) 2)))

(defun xenops-text-prettify-regexp-get-text-properties (match)
  "Return plist of text properties for match.
E.g. if `match' looks like \textbf{something}, then return text
properties that will apply a bold face to the replacement
text."
  (cond
   ((string-match "\\(\\\\textbf{\\|{\\\\bf \\)" match)
    '(face bold))
   ((string-match "\\(\\\\textit{\\|\\\\emph{\\|{\\\\it \\)" match)
    '(face italic))))

(defun xenops-text-prettify-symbols-compose (composition properties beg end)
  "Taken from `prettify-symbols--compose-symbol'"
  ;; TODO: look at defensive measures in that function.
  (with-silent-modifications
    (compose-region beg end composition)
    (add-text-properties
     beg end
     (append properties
             `(prettify-symbols-start ,beg prettify-symbols-end ,end)))))

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
