(defun xenops-overlay-at-point ()
  (--first (overlay-get it 'xenops-overlay-type) (overlays-at (point))))

(provide 'xenops-overlay)
