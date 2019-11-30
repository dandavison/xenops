(ert-deftest xenops-text-prettify-symbols ()
  (let ((xenops-text-prettify-symbols
         '(("\\grad" . "∇"))))
    (with-temp-buffer
      (save-excursion (insert "\\grad"))
      (xenops-mode)
      (font-lock-fontify-buffer)
      (should (plist-get (text-properties-at (point)) 'prettify-symbols-start)))))

(ert-deftest xenops-text-prettify-symbols-string-replacement ()
  (let* ((replacement `("\\begin{abstract}" . "Abstract."))
         (xenops-text-prettify-symbols-string-replacements
          `(,replacement)))
    (with-temp-buffer
      (save-excursion (insert (car replacement)))
      (xenops-mode)
      (font-lock-fontify-buffer)
      (should (plist-get (text-properties-at (point)) 'prettify-symbols-start))
      (should (equal
               (xenops-text-composition-to-string
                (plist-get (text-properties-at (point)) 'composition))
               (cdr replacement))))))

(ert-deftest xenops-text-prettify-symbols-regexp-replacement-1 ()
  (let* ((xenops-text-prettify-symbols-regexp-replacements
          '("\\\\textit{\\([^\n}]+\\)}")))
    (with-temp-buffer
      (save-excursion (insert "\\textit{To be italicised}"))
      (xenops-mode)
      (font-lock-fontify-buffer)
      (should (plist-get (text-properties-at (point)) 'prettify-symbols-start))
      (should (equal
               (xenops-text-composition-to-string
                (plist-get (text-properties-at (point)) 'composition))
               "To be italicised")))))

(ert-deftest xenops-text-prettify-symbols-regexp-replacement-2 ()
  (let* ((xenops-text-prettify-symbols-regexp-replacements
          '("\\\\\\(?:sub\\)*section\\*?{\\([^\n}]+\\)}")))
    (with-temp-buffer
      (save-excursion (insert "\\subsubsection*{Section title}"))
      (xenops-mode)
      (font-lock-fontify-buffer)
      (should (plist-get (text-properties-at (point)) 'prettify-symbols-start))
      (should (equal
               (xenops-text-composition-to-string
                (plist-get (text-properties-at (point)) 'composition))
               "Section title")))))

(defun xenops-text-composition-to-string (composition)
  "Return string corresponding to composition text property. When
  the text \"Abstract.\" is displayed, the composition text
  property has a value like

\(...
      [16 65 63 98 63 115 63 116 63 114 63 97 63 99 63 116 63 46]\)

After stripping out the interleaved character 63 this is the
string \"Abstract.\".  I'm not sure what the 63 is and I'm not
sure what the leading 16 is and sometimes it's a list and
sometimes a vector etc; this may be very fragile."
  (let ((list-of-chars (append (car (last composition)) nil)))
    (concat
     (loop for (- char) in (-partition 2 list-of-chars)
           collecting char))))
