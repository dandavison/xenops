;;; xenops-footnote.el --- Functions for working with elements of type 'footnote -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:

(defun xenops-footnote-render (element)
  (let ((ov (xenops-overlay-create (plist-get element :begin)
                                   (plist-get element :end))))
    (overlay-put ov 'display "[footnote]")
    (overlay-put ov 'help-echo
                 (s-replace-regexp "[ \n]+" " "
                                   (buffer-substring
                                    (plist-get element :begin-content)
                                    (plist-get element :end-content))))
    ov))

(defun xenops-footnote-parse-at-point ()
  (if (looking-at (caar (xenops-elements-get 'footnote :delimiters)))
      `(:type footnote :begin ,(match-beginning 0) :end ,(match-end 0)
        :begin-content ,(match-beginning 1) :end-content ,(match-end 1))))


(provide 'xenops-footnote)

;;; xenops-footnote.el ends here
