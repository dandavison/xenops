;;; xenops-elements.el --- API for working with `xenops-elements' -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:
(require 's)

(declare-function xenops-get "xenops")
(declare-function xenops-get-for-types "xenops")

;; Silence compiler: defined elsewhere
(defvar xenops-elements)

(defun xenops-elements-get (type key)
  "The value associated with KEY for element type TYPE."
  (xenops-get xenops-elements type key))

(defun xenops-elements-get-for-types (types key)
  "Concatenated list of values associated with KEY for elements of types TYPES."
  (xenops-get-for-types xenops-elements types key))

(defun xenops-elements-get-all (key)
  "Concatenated list of all items under key KEY for any element type."
  (xenops-elements-get-for-types 'all key))

(defun xenops-elements-delimiter-start-regexp (&optional types)
  "A regexp matching the start of any element.

Optional argument TYPES limits the elements considered."
  (format "\\(%s\\)"
          (s-join "\\|"
                  (mapcar #'car (xenops-elements-get-for-types (or types 'all) :delimiters)))))

(provide 'xenops-elements)

;;; xenops-elements.el ends here
