;;; style.el --- A library for visual styling of text in a buffer, inspired by `prettify-symbols-mode' -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:

;; This module implements a self-contained minor mode `style-mode' which is essentially an
;; extension of `prettify-symbols-mode'.

(defvar-local style-rules nil
  "List of display strings used to change the visual appearance of text in a buffer.

An item in the list must have one of the following forms:

\(STRING . CHARACTER\) An entry of this form has the same meaning
                       as an entry in `prettify-symbols-alist':
                       occurrences of STRING (which is not a
                       regular expression) are displayed as
                       CHARACTER. If CHARACTER is a vector or list,
                       then this is treated as a sequence of
                       characters to be composed. See `prettify-symbols-alist'.

\(STRING . STRING\)    In this case, occurrences of the first string
                       (not a regular expression) are displayed as
                       the second string.

REGEXP                 In this case, REGEXP must be a regular expression with
                       exactly one capture group. Text matching the
                       regular expression will be displayed using
                       the text captured by the capture group. For
                       example, the entry
                       \"\\\\\\\\textit{\\\\([^\\n}]+\\\\)}\"
                       causes occurrences of the LaTeX italic
                       markup \"\\textit{some text}\" to be
                       displayed visually as \"some text\". See
                       `style-regexp-rules-get-text-properties'
                       for how an italic font is then applied to
                       \"some text\".

\(REGEXP . FUNCTION\) In this case, REGEXP must be a regular
                      expression with exactly one capture group and
                      FUNCTION must be a function that takes one
                      string argument and returns a string.  The
                      displayed text is computed by taking the text
                      captured by the regular expression and
                      passing it to FUNCTION.")

(define-minor-mode style-mode
  "A minor mode changing the visual appearance of the buffer according to `style-rules'.

\\{style-mode-map}"
  :lighter " style"
  (cond
   (style-mode
    (let ((prettify-symbols-alist prettify-symbols-alist))
      (mapc #'style-process-string-rule
            (style-get-string-rules))

      (setq prettify-symbols-alist
            (sort prettify-symbols-alist (lambda (x y) (> (length (car x)) (length (car y))))))

      (prettify-symbols-mode +1))
    (font-lock-add-keywords nil (style-extra-font-lock-keywords))
    (style-configure-tooltips))
   (t
    ;; Deactivate
    (font-lock-remove-keywords nil (style-extra-font-lock-keywords))
    (style-configure-tooltips 'deactivate)
    (prettify-symbols-mode -1)
    (font-lock-mode -1)
    (font-lock-mode 1))))

(defun style-extra-font-lock-keywords ()
  `((,(style-regexp-rules-make-regexp)
     (0
      (style-regexp-rule-apply)))))

(setq style-tooltip-delay-orig nil)

(defun style-configure-tooltips (&optional deactivate)
  ;; Add tooltips to prettify replacements
  ;; TODO: I think this is causing the very long regexp to be matched twice during fontification.
  ;; Can this be done by modifying the existing prettify-symbols entry?
  (dolist (regexp (list (caar prettify-symbols--keywords)
                        (style-regexp-rules-make-regexp)))
    (funcall
     (if deactivate 'font-lock-remove-keywords 'font-lock-add-keywords)
     nil
     `((,regexp
        0 `(face font-lock-keyword-face
                 help-echo ,(match-string 0))))))
  (if deactivate
      (setq tooltip-delay style-tooltip-delay-orig)
    (setq style-tooltip-delay-orig tooltip-delay)))

(defun style-regexp-rule-apply ()
  "A match for a regexp capture replacement has just been made.
Compute the replacement text to be displayed, with its text
properties, and display it."
  (let* ((beg (match-beginning 0))
         (end (match-end 0))
         (match (match-string 0))
         (match-string-index (style-regexp-rules-get-match-string-index))
         (display-string (style-regexp-rules-make-display-string match-string-index))
         (composition (style-make-composition display-string))
         (properties (if style-regexp-rules-get-text-properties
                         (funcall style-regexp-rules-get-text-properties match))))
    (style-compose composition properties beg end))
  nil)

(defun style-regexp-rules-make-regexp ()
  "Return a regular expression matching text corresponding to any
regular expression rule in `style-rules'."
  (format "\\(%s\\)"
          (s-join "\\|" (style-regexp-rules-get-regexps))))

(defun style-regexp-rule-get-canonicalized-rule (spec)
  "Return cons cell (REGEXP . FORMATTER) where FORMATTER may be nil."
  (pcase spec
    (`(,(and (pred stringp) regexp) . ,(and (pred functionp) formatter))
     `(,regexp . ,formatter))
    ((and (pred stringp) regexp)
     `(,regexp))))

(defun style-regexp-rules-get-regexps ()
  (-remove
   #'null
   (mapcar (-compose #'car #'style-regexp-rule-get-canonicalized-rule)
           style-rules)))

(defun style-get-regexp-rules ()
  (-filter
   #'style-regexp-rule-get-canonicalized-rule
   style-rules))

(defun style-get-string-rules ()
  (-remove
   #'style-regexp-rule-get-canonicalized-rule
   style-rules))

(defun style-regexp-rules-get-match-string-index ()
  "A match for a regexp capture replacement has just been made.
Return the to be supplied to `match-string' to obtain the caotured text."
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
  ;; index that is non-nil. Suppose this is i. Then (i + 4)/2 is the corresponding index.
  (/ (+ (xenops-util-first-index (-drop 4 (match-data 'integers))) 4) 2))

(defvar style-regexp-rules-get-text-properties nil
  "A function of one argument (MATCH, i.e. the original matched text)
returning a plist of text properties to be applied to the displayed text.
E.g. if MATCH looks like \textbf{something}, then the function
might return text properties that will apply a bold face to the
replacement text.")

(defun style-regexp-rules-make-display-string (match-string-index)
  (let ((spec (nth (- match-string-index 2) (style-get-regexp-rules)))
        (capture (save-match-data
                   (s-join " " (split-string (match-string match-string-index))))))
    (pcase spec
      (`(,regexp . ,formatter) (funcall formatter capture))
      (_ capture))))

(defun style-compose (composition properties beg end)
  "Taken from `prettify-symbols--compose-symbol'"
  ;; TODO: look at defensive measures in that function.
  (with-silent-modifications
    (compose-region beg end composition)
    (add-text-properties
     beg end
     (append properties
             `(prettify-symbols-start ,beg prettify-symbols-end ,end)))))

;; https://emacs.stackexchange.com/a/34882/9007
(defun style-process-string-rule (pair)
  "Make `prettify-symbols-mode' replace string FROM with string TO.

Updates `prettify-symbols-alist'.  You may need to toggle
`prettify-symbols-mode' to make the changes take effect.

Each character of TO is vertically aligned using the baseline,
such that base-left of the character is aligned with base-right
of the preceding character.  Refer to `reference-point-alist'
for more information."
  (push (cons (car pair)
              (style-make-composition (cdr pair)))
        prettify-symbols-alist))

(defun style-make-composition (string)
  (let ((composition nil))
    (dolist (char (string-to-list string)
                  (nreverse (cdr composition)))
      (push char composition)
      (push '(Br . Bl) composition))))

(provide 'style)

;;; style.el ends here
