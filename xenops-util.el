(defmacro xenops-util-define-key-with-fallback (key handler &optional fallback-key)
  "Bind `handler' to `key' in `xenops-mode-map' such that if
`handler' returns `nil', then the function is called that would
have been bound to `key' were `xenops-mode' not active."
  `(define-key xenops-mode-map ,key
     (lambda ()
       (interactive)
       (unless (funcall ,handler)
         (let (xenops-mode)
           (execute-kbd-macro ,(or fallback-key key)))))))

(defun xenops-util-first-index (list)
  "Return smallest index for which the corresponding element is
non-nil, or nil if no such index exists."
  (catch :index
    (let ((i 0))
      (dolist (el list)
        (and el (throw :index i))
        (setq i (1+ i))))))

(provide 'xenops-util)
