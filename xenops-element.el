;; -*- lexical-binding: t -*-

;; Generic operations on an element. Functions in this file should determine behavior based on the
;; type of the element and requested operations, by consulting the `xenops-elements' and
;; `xenops-ops' data structures. They should not directly call functions that are specific to
;; element type (e.g. in xenops-math, xenops-image, xenops-text).

(defun xenops-element-delete-overlays (element)
  (let ((beg (plist-get element :begin))
        (end (plist-get element :end)))
    (dolist (ov (overlays-in beg end))
      (when (overlay-get ov 'xenops-overlay-type)
        (delete-overlay ov)))))

(defalias 'xenops-element-reveal #'xenops-element-delete-overlays)

(defun xenops-element-copy (element)
  (copy-region-as-kill (plist-get element :begin)
                       (plist-get element :end)))

(defun xenops-element-delete (element)
  (kill-region (plist-get element :begin)
               (plist-get element :end))
  t)

(defun xenops-element-dispatch (el handlers)
  "Call the first handler in HANDLERS that is valid for an element of this type."
  (-if-let* ((handler (car (-intersection handlers (xenops-element-handlers-for-el el)))))
      (funcall handler el)))

(defun xenops-element-do (element op)
  "Do operation OP on ELEMENT."
  (xenops-element-dispatch element (xenops-ops-get op :handlers)))

(defun xenops-element-handlers-for-el (el)
  "The valid operations for an element of this type."
  (xenops-elements-get (plist-get el :type) :handlers))

(defun xenops-element-get-image (element)
  (xenops-parse-image-at (plist-get element :begin)))

(defun xenops-element-create-marker (el)
  "Create a marker pointing at the current :begin position of ELEMENT."
  (plist-put el :begin-marker (set-marker (make-marker) (plist-get el :begin))))

(defun xenops-element-deactivate-marker (el)
  "Delete the marker created by `xenops-element-create-marker'."
  (if-let* ((marker (plist-get el :begin-marker)))
      (set-marker marker nil)))

(provide 'xenops-element)
