;; Generic operations on elements. Functions in this file should determine behavior based on the
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

(defun xenops-element-element? (el)
  (plist-get el :type))

(defun xenops-element-op-for-el (el ops)
  "The first operation in OPS that is valid for an element of this type."
  (car (-intersection ops (xenops-element-ops-for-el el))))

(defun xenops-element-op-of-type-for-el (el op-type)
  "Does an element of this type have an operation of type OP-TYPE?"
  (xenops-element-op-for-el el (xenops-ops-for-op-type op-type)))

(defun xenops-element-ops-for-el (el)
  "The valid operations for an element of this type."
  (xenops-elements-get (plist-get el :type) :ops))

(defun xenops-element-get-image-at-point ()
  (let ((display (get-char-property (point) 'display )))
    (and (eq (car display) 'image) display)))

(defun xenops-element-get-image (element)
  (save-excursion
    (goto-char (plist-get element :begin))
    (xenops-element-get-image-at-point)))

(defun xenops-element-make-overlay (element)
  (let* ((beg (plist-get element :begin))
         (end (plist-get element :end))
         (ov (make-overlay beg end))
         (keymap (make-sparse-keymap)))
    (overlay-put ov 'xenops-overlay-type 'xenops-overlay)
    (overlay-put ov 'evaporate t)
    (overlay-put ov
                 'modification-hooks
                 (list (lambda (o _flag _beg _end &optional _l)
                         (delete-overlay o))))
    (overlay-put ov 'help-echo (buffer-substring beg end))

    (set-keymap-parent keymap xenops-rendered-element-keymap)
    (define-key keymap [mouse-3] `(lambda (event) (interactive "e") (xenops-element-menu ',element event)))
    (overlay-put ov 'keymap keymap)

    ov))

(defun xenops-element-menu (element event)
  (popup-menu
   `("Xenops"
     ["Edit" (xenops-element-reveal ',element)])
   event))

(provide 'xenops-element)
