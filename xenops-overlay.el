;; -*- lexical-binding: t -*-

(defun xenops-overlay-create (beg end)
  (let* ((ov (make-overlay beg end))
         (keymap (make-sparse-keymap)))
    (overlay-put ov 'xenops-overlay-type 'xenops-overlay)
    (overlay-put ov 'evaporate t)
    (overlay-put ov
                 'modification-hooks
                 (list (lambda (o _flag _beg _end &optional _l)
                         (delete-overlay o))))

    (set-keymap-parent keymap xenops-rendered-element-keymap)
    (define-key keymap [mouse-3] #'xenops-overlay-menu)
    (overlay-put ov 'keymap keymap)

    ov))

(defun xenops-overlay-menu (event)
  (interactive "e")
  (popup-menu
   `("Xenops"
     ["Edit" (xenops-reveal)])
   event))

(defun xenops-overlay-at-point ()
  (--first (overlay-get it 'xenops-overlay-type) (overlays-at (point))))

(defun xenops-overlay-delete-overlays (&optional beg end)
  (interactive "r")
  (dolist (ov (overlays-in (or beg (point-min)) (or end (point-max))))
    (delete-overlay ov)))

(provide 'xenops-overlay)
