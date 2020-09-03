;;; xenops-overlay.el --- Create/Read/Delete API for xenops overlays used to render Xenops elements -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:

(defun xenops-overlay-create (beg end)
  "Create a Xenops overlay between BEG and END."
  (let* ((ov (make-overlay beg end))
         (keymap (make-sparse-keymap)))
    (overlay-put ov 'xenops-overlay-type 'xenops-overlay)
    (overlay-put ov 'evaporate t)
    (overlay-put ov
                 'modification-hooks
                 (list (lambda (o _flag _beg _end &optional _l)
                         (delete-overlay o))))
    (overlay-put ov 'keymap keymap)
    ov))

(defun xenops-overlay-at-point ()
  "Return a Xenops overlay at point, if there is one."
  (--first (overlay-get it 'xenops-overlay-type) (overlays-at (point))))

(defun xenops-overlay-delete-overlays-in (&optional beg end)
  "Delete Xenops overlays between BEG and END."
  (interactive "r")
  (dolist (ov (overlays-in (or beg (point-min)) (or end (point-max))))
    (when (overlay-get ov 'xenops-overlay-type)
      (delete-overlay ov))))

(provide 'xenops-overlay)

(provide 'xenops-overlay)

;;; xenops-overlay.el ends here
