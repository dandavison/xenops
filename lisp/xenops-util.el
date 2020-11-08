;;; xenops-util.el --- Miscellaneous utility functions  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:
(declare-function xenops-overlay-create "xenops-overlay")

(defmacro xenops-util-define-key-with-fallback (key handler &optional fallback-key)
  "Bind HANDLER to KEY in `xenops-mode-map'.

The binding is such that if HANDLER returns nil, then the
function is called that would have been bound to KEY were
variable `xenops-mode' not active."
  `(define-key xenops-mode-map ,key
     (lambda ()
       (interactive)
       (unless (funcall ,handler)
         (let (xenops-mode)
           (execute-kbd-macro ,(or fallback-key key)))))))

(defun xenops-util-goto-line (line)
  "Move point to the first column of LINE, efficiently."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun xenops-util-first-index (list)
  "Return smallest index for which the corresponding element in LIST is non-nil.

Return nil if no such index exists."
  (catch :index
    (let ((i 0))
      (dolist (el list)
        (and el (throw :index i))
        (setq i (1+ i))))))

(defun xenops-util-first-result (fn list)
  "Call FN on each element of LIST until a non-nil return value is encountered.

Return this value without further evaluations."
  (catch :result
    (dolist (el list)
      (-if-let* ((result (funcall fn el)))
          (throw :result result)))))

(defun xenops-util-plist-update (plist &rest args)
  "Update PLIST according to ARGS.

ARGS will typically look like :k1 v1 :k2 v2 ..."
  (dolist (pair (-partition 2 args))
    (setq plist (apply #'plist-put plist pair)))
  plist)

(defun xenops-util-parse-image-at (pos)
  "Parse image at POS."
  (let ((display (get-char-property pos 'display )))
    (and (listp display) (eq (car display) 'image) display)))

(defun xenops-util-svg-resize (svg scale)
  "Return SVG data with height and width scaled by SCALE."
  (cl-flet ((resize (match)
                    (cl-destructuring-bind (size . units)
                        (xenops-util-svg-parse-length-or-percent (match-string 1 match))
                      (format "%f%s" (* scale size) units))))
    ;; This regexp captures a single-quoted string, and then consumes to the end of the input,
    ;; across newlines. I.e. we replace the first match only in a multiline string. See
    ;; `replace-regexp-in-string'.
    (let ((quoted-string-regexp "'\\([^']+\\)'\\(.\\|\n\\)*"))
      (setq svg (replace-regexp-in-string  (concat "width=" quoted-string-regexp) #'resize svg nil nil 1))
      (setq svg (replace-regexp-in-string (concat "height=" quoted-string-regexp) #'resize svg nil nil 1)))
    svg))

(defun xenops-util-svg-parse-length-or-percent (string)
  "Parse STRING, returning the numerical coefficient and the unit specifier.

For example, STRING might look like '1.5pt' or '50.5%'"
  (let* ((n (length string))
         (unit-specifier (if (s-ends-with? "%" string)
                             "%"
                           (substring string (- n 2) n))))
    (cons (string-to-number (substring string 0 (- n (length unit-specifier))))
          unit-specifier)))

(defun xenops-util-highlight-current-line ()
  "Highlight the current line."
  (let ((ov (xenops-overlay-create (point-at-bol) (point-at-eol))))
    (overlay-put ov 'face 'highlight)))

(provide 'xenops-util)

;; xenops-util.el ends here

(provide 'xenops-util)

;;; xenops-util.el ends here
