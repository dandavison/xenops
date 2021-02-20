;;; xenops-style.el --- A library for visual styling of text in a buffer -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module implements a self-contained minor mode `xenops-style-mode' which is essentially an
;; extension of `prettify-symbols-mode'.

;;; Code:
(require 'dash)
(require 's)

(declare-function xenops-util-first-index "xenops-util")

(defvar-local xenops-style-rules nil
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
                       `xenops-style-regexp-rules-get-text-properties-function'
                       for how an italic font is then applied to
                       \"some text\".

\(REGEXP . FUNCTION\) In this case, REGEXP must be a regular
                      expression with exactly one capture group and
                      FUNCTION must be a function that takes one
                      string argument and returns a string.  The
                      displayed text is computed by taking the text
                      captured by the regular expression and
                      passing it to FUNCTION.")

(defvar xenops-style-regexp-rules-get-text-properties-function nil
  "A function of one argument (MATCH, i.e. the original matched text)
returning a plist of text properties to be applied to the displayed text.
E.g. if MATCH looks like \\textbf{something}, then the function
might return text properties that will apply a bold face to the
replacement text.")

(defvar xenops-style-tooltip-delay-orig nil)

(define-minor-mode xenops-style-mode
  "A minor mode changing the visual appearance of the buffer according to `xenops-style-rules'.

\\{xenops-style-mode-map}"
  :lighter " style"
  (cond
   (xenops-style-mode
    (let ((prettify-symbols-alist prettify-symbols-alist))
      (mapc #'xenops-style-process-string-rule
            (xenops-style-get-string-rules))

      (setq prettify-symbols-alist
            (sort prettify-symbols-alist (lambda (x y) (> (length (car x)) (length (car y))))))

      (prettify-symbols-mode +1))
    (font-lock-add-keywords nil (xenops-style-extra-font-lock-keywords))
    (xenops-style-configure-tooltips))
   (t
    ;; Deactivate
    (font-lock-remove-keywords nil (xenops-style-extra-font-lock-keywords))
    (xenops-style-configure-tooltips 'deactivate)
    (prettify-symbols-mode -1)
    (font-lock-mode -1)
    (font-lock-mode 1))))

(defun xenops-style-extra-font-lock-keywords ()
  "Create the font-lock keyword style rules."
  `((,(xenops-style-regexp-rules-make-regexp)
     (0
      (xenops-style-regexp-rule-apply)))))

(defun xenops-style-configure-tooltips (&optional deactivate)
  "Add tooltips to prettify replacements.

Optional argument DEACTIVATE removes tooltips."
  ;; TODO: I think this is causing the very long regexp to be matched twice during fontification.
  ;; Can this be done by modifying the existing prettify-symbols entry?
  (dolist (regexp (list (caar prettify-symbols--keywords)
                        (xenops-style-regexp-rules-make-regexp)))
    (funcall
     (if deactivate 'font-lock-remove-keywords 'font-lock-add-keywords)
     nil
     `((,regexp
        0 `(face font-lock-keyword-face
                 help-echo ,(match-string 0))))))
  (if deactivate
      (setq tooltip-delay xenops-style-tooltip-delay-orig)
    (setq xenops-style-tooltip-delay-orig tooltip-delay)))

(defun xenops-style-regexp-rule-apply ()
  "A match for a regexp capture replacement has just been made.
Compute the replacement text to be displayed, with its text
properties, and display it."
  (let* ((beg (match-beginning 0))
         (end (match-end 0))
         (match (match-string 0))
         (match-string-index (xenops-style-regexp-rules-get-match-string-index))
         (display-string (xenops-style-regexp-rules-make-display-string match-string-index))
         (composition (xenops-style-make-composition display-string))
         (properties (and (functionp xenops-style-regexp-rules-get-text-properties-function)
                          (funcall xenops-style-regexp-rules-get-text-properties-function match))))
    (xenops-style-compose composition properties beg end))
  nil)

(defun xenops-style-regexp-rules-make-regexp ()
  "Return a regular expression matching text corresponding to any
regular expression rule in `xenops-style-rules'."
  (format "\\(%s\\)"
          (s-join "\\|" (xenops-style-regexp-rules-get-regexps))))

(defun xenops-style-regexp-rule-get-canonicalized-rule (spec)
  "Return cons cell (REGEXP . FORMATTER) for SPEC where FORMATTER may be nil."
  (pcase spec
    (`(,(and (pred stringp) regexp) . ,(and (pred functionp) formatter))
     `(,regexp . ,formatter))
    ((and (pred stringp) regexp)
     `(,regexp))))

(defun xenops-style-regexp-rules-get-regexps ()
  "Return the regexps used by style rules."
  (-remove
   #'null
   (mapcar (-compose #'car #'xenops-style-regexp-rule-get-canonicalized-rule)
           xenops-style-rules)))

(defun xenops-style-get-regexp-rules ()
  "Return the suset of rules that are regexp rules."
  (-filter
   #'xenops-style-regexp-rule-get-canonicalized-rule
   xenops-style-rules))

(defun xenops-style-get-string-rules ()
  "Return the suset of rules that are string rules."
  (-remove
   #'xenops-style-regexp-rule-get-canonicalized-rule
   xenops-style-rules))

(defun xenops-style-regexp-rules-get-match-string-index ()
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

(defun xenops-style-regexp-rules-make-display-string (match-string-index)
  "Return the string to be displayed for the regexp rule with index MATCH-STRING-INDEX."
  (let ((spec (nth (- match-string-index 2) (xenops-style-get-regexp-rules)))
        (capture (save-match-data
                   (s-join " " (split-string (match-string match-string-index))))))
    (pcase spec
      (`(,_ . ,formatter) (funcall formatter capture))
      (_ capture))))

(defun xenops-style-compose (composition properties beg end)
  "Compose symbols for COMPOSITION.

Adds to PROPERTIES between BEG and END."
  ;; Taken from `prettify-symbols--compose-symbol'
  ;; TODO: look at defensive measures in that function.
  (with-silent-modifications
    (compose-region beg end composition)
    (add-text-properties
     beg end
     (append properties
             `(prettify-symbols-start ,beg prettify-symbols-end ,end)))))

;; https://emacs.stackexchange.com/a/34882/9007
(defun xenops-style-process-string-rule (pair)
  "Register a style string replacekent rule.

PAIR is a cons (FROM, TO). Make function `prettify-symbols-mode'
replace FROM with TO.

Updates `prettify-symbols-alist'. You may need to call toggle
command `prettify-symbols-mode' to make the changes take effect.

Each character of TO is vertically aligned using the baseline,
such that base-left of the character is aligned with base-right
of the preceding character.  Refer to `reference-point-alist'
for more information."
  (push (cons (car pair)
              (xenops-style-make-composition (cdr pair)))
        prettify-symbols-alist))

(defun xenops-style-make-composition (string)
  "Return composition for STRING."
  (let ((composition nil))
    (dolist (char (string-to-list string)
                  (nreverse (cdr composition)))
      (push char composition)
      (push '(Br . Bl) composition))))

(provide 'xenops-style)

;;; xenops-style.el ends here
