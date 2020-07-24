;; -*- lexical-binding: t -*-

(ert-deftest xenops-test-xen--test-prettify-symbols-replacement ()
  "An entry in  `prettify-symbols-alist' set outside xenops should be honored."
  (with-temp-buffer
    (let ((prettify-symbols-alist '(("replacement-specified-outside-xenops" . ?⚡))))
      (save-excursion (insert "replacement-specified-outside-xenops"))
      (xenops-mode)
      (call-interactively #'xenops-xen-mode)
      (font-lock-fontify-buffer)
      (should (plist-get (text-properties-at (point)) 'prettify-symbols-start))
      (should (equal
               (style-composition-to-displayed-string
                (plist-get (text-properties-at (point)) 'composition))
               "⚡")))))

(ert-deftest xenops-test-xen--test-character-replacement ()
  "Test replacement by a single character."
  (with-temp-buffer
    (save-excursion (insert "\\grad"))
    (xenops-mode)
    (call-interactively #'xenops-xen-mode)
    (font-lock-fontify-buffer)
    (should (plist-get (text-properties-at (point)) 'prettify-symbols-start))
    (should (equal
             (style-composition-to-displayed-string
              (plist-get (text-properties-at (point)) 'composition))
             "∇"))))

(ert-deftest xenops-test-xen--test-string-replacement ()
  "Test replacement by a multi-character string."
  (with-temp-buffer
    (save-excursion (insert "\\begin{abstract}"))
    (xenops-mode)
    (call-interactively #'xenops-xen-mode)
    (font-lock-fontify-buffer)
    (should (plist-get (text-properties-at (point)) 'prettify-symbols-start))
    (should (equal
             (style-composition-to-displayed-string
              (plist-get (text-properties-at (point)) 'composition))
             "abstract"))))

(ert-deftest xenops-test-xen--test-regexp-replacement-1 ()
  "Test replacement by regexp captured text."
  (with-temp-buffer
    (save-excursion (insert "\\textit{To be italicised}"))
    (xenops-mode)
    (call-interactively #'xenops-xen-mode)
    (font-lock-fontify-buffer)
    (should (plist-get (text-properties-at (point)) 'prettify-symbols-start))
    (should (equal
             (style-composition-to-displayed-string
              (plist-get (text-properties-at (point)) 'composition))
             "To be italicised"))))

(ert-deftest xenops-test-xen--test-regexp-replacement-2 ()
  "Test replacement by regexp captured text employing a formatting function."
  (with-temp-buffer
    (save-excursion (insert "\\subsection*{Section title}"))
    (xenops-mode)
    (call-interactively #'xenops-xen-mode)
    (font-lock-fontify-buffer)
    (should (plist-get (text-properties-at (point)) 'prettify-symbols-start))
    (should (equal
             (style-composition-to-displayed-string
              (plist-get (text-properties-at (point)) 'composition))
             "    Section title"))))

(defun style-composition-to-displayed-string (composition)
  "Return string corresponding to composition text property.

When the displayed text is the string \"Abstract.\", the
composition text property has a value like

\(...
      [16 65 63 98 63 115 63 116 63 114 63 97 63 99 63 116 63 46]\).

After stripping out the interleaved characters 16 and 63 this is
the string \"Abstract.\".

When the character ⚡ is displayed, the composition text property
  has a value like

\(\(36 . 9889\)\).

This may be fragile; this data structure is documented to not be
part of the public API."
  (let* ((displayed-text (car (last composition))))
    (if (vectorp displayed-text)
        (setq displayed-text (append displayed-text nil)))
    (cond
     ((listp (cdr displayed-text))
      (concat
       (cl-loop for (_ char) in (-partition 2 displayed-text)
                collecting char)))
     ((stringp (cdr displayed-text))
      (cdr displayed-text))
     (t
      (char-to-string (cdr displayed-text))))))
