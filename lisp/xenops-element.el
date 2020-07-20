;;; xenops-element.el --- Functions for operating generically on an element -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Generic operations on an element. Functions in this file should determine behavior based on the
;; type of the element and requested operations, by consulting the `xenops-elements' and
;; `xenops-ops' data structures. They should not directly call functions that are specific to
;; element type (e.g. in xenops-math, xenops-image, xenops-text).

;;; Code:
(require 'subr-x)

(require 'dash)

(declare-function xenops-util-parse-image-at "xenops-util")

(defun xenops-element-overlays-get (element)
  "Return overlays overlapping ELEMENT."
  (let ((beg (plist-get element :begin))
        (end (plist-get element :end)))
    (overlays-in beg end)))

(defun xenops-element-overlays-delete (element)
  "Delete xenops overlays overlapping ELEMENT."
  (dolist (ov (xenops-element-overlays-get element))
    (when (overlay-get ov 'xenops-overlay-type)
      (delete-overlay ov))))

(defun xenops-element-overlay-get (element type)
  "Return first xenops overlay of type TYPE overlapping ELEMENT."
  (--first (eq (overlay-get it 'xenops-overlay-type) type)
           (xenops-element-overlays-get element)))

(defalias 'xenops-element-reveal #'xenops-element-overlays-delete)

(defun xenops-element-copy (element)
  "Copy ELEMENT text to kill ring."
  (copy-region-as-kill (plist-get element :begin)
                       (plist-get element :end)))

(defun xenops-element-delete (element)
  "Delete ELEMENT."
  (kill-region (plist-get element :begin)
               (plist-get element :end))
  t)

(defun xenops-element-get-image (element)
  "Return the display property of ELEMENT if it is of type 'image."
  (xenops-util-parse-image-at (plist-get element :begin)))

(defun xenops-element-create-marker (element)
  "Create a marker pointing at the current :begin position of ELEMENT."
  (plist-put element :begin-marker (set-marker (make-marker) (plist-get element :begin))))

(defun xenops-element-deactivate-marker (element)
  "Delete the marker for ELEMENT created by `xenops-element-create-marker'."
  (if-let* ((marker (plist-get element :begin-marker)))
      (set-marker marker nil)))

(provide 'xenops-element)

;;; xenops-element.el ends here
