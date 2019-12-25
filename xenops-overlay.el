;; -*- lexical-binding: t -*-

(defun xenops-overlay-at-point ()
  (--first (overlay-get it 'xenops-overlay-type) (overlays-at (point))))

(defun xenops-overlay-delete-overlays (&optional beg end)
  (interactive "r")
  (dolist (ov (overlays-in (or beg (point-min)) (or end (point-max))))
    (delete-overlay ov)))

(provide 'xenops-overlay)
