(ert-deftest xenops-text-prettify-symbols ()
  (let ((xenops-text-prettify-symbols
         '(("\\grad" . "∇"))))
    (with-temp-buffer
      (save-excursion (insert "\\grad"))
      (xenops-mode)
      (font-lock-fontify-buffer)
      (should (plist-get (text-properties-at (point)) 'prettify-symbols-start)))))
