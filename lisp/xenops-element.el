;;; xenops-element.el --- Functions for operating generically on an element -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Generic operations on an element. Functions in this file should determine behavior based on the
;; type of the element and requested operations, by consulting the `xenops-elements' and
;; `xenops-ops' data structures. They should not directly call functions that are specific to
;; element type (e.g. in xenops-math, xenops-image, xenops-text).

;;; Code:

(defun xenops-element-overlays-get (element)
  (let ((beg (plist-get element :begin))
        (end (plist-get element :end)))
    (overlays-in beg end)))

(defun xenops-element-overlays-delete (element)
  (dolist (ov (xenops-element-overlays-get element))
    (when (overlay-get ov 'xenops-overlay-type)
      (delete-overlay ov))))

(defun xenops-element-overlay-get (element type)
  (--first (eq (overlay-get it 'xenops-overlay-type) type)
           (xenops-element-overlays-get element)))

(defalias 'xenops-element-reveal #'xenops-element-overlays-delete)

(defun xenops-element-copy (element)
  (copy-region-as-kill (plist-get element :begin)
                       (plist-get element :end)))

(defun xenops-element-delete (element)
  (kill-region (plist-get element :begin)
               (plist-get element :end))
  t)

(defun xenops-element-get-image (element)
  (xenops-util-parse-image-at (plist-get element :begin)))

(defun xenops-element-create-marker (el)
  "Create a marker pointing at the current :begin position of ELEMENT."
  (plist-put el :begin-marker (set-marker (make-marker) (plist-get el :begin))))

(defun xenops-element-deactivate-marker (el)
  "Delete the marker created by `xenops-element-create-marker'."
  (if-let* ((marker (plist-get el :begin-marker)))
      (set-marker marker nil)))

(provide 'xenops-element)

;;; xenops-element.el ends here
