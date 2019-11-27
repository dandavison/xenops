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

(defun xenops-util-first-result (fn list)
  "Call FN on each element of LIST until a non-nil return value
is encountered. Return this value without further evaluations."
  (catch :result
    (dolist (el list)
      (-if-let (result (funcall fn el))
          (throw :result result)))))

(defun xenops-util-plist-update (plist &rest args)
  (dolist (pair (-partition 2 args))
    (setq plist (apply #'plist-put plist pair)))
  plist)

(defun xenops-util-svg-resize (svg scale)
  "Return SVG data with height and width scaled by `scale'"
  (cl-flet ((resize (match)
                    (destructuring-bind (size . units)
                        (xenops-util-svg-parse-length-or-percent (match-string 1 match))
                      (format "%f%s" (* scale size) units))))
    ;; We could parse as XML, but this is tempting.
    (setq svg (replace-regexp-in-string "width='\\([^']+\\)'.*" #'resize svg nil nil 1))
    (setq svg (replace-regexp-in-string "height='\\([^']+\\)'.*" #'resize svg nil nil 1))
    svg))

(defun xenops-util-svg-parse-length-or-percent (string)
  "Parse a string like '1.5pt' or '50.5%'"
  (let* ((n (length string))
         (unit-specifier (if (s-ends-with? "%" string)
                             "%"
                           (substring string (- n 2) n))))
    (cons (string-to-number (substring string 0 (- n (length unit-specifier))))
          unit-specifier)))

(provide 'xenops-util)


(replace-regexp-in-string "width='\\([^']+\\)'.*" "XXXXpt" "32.409891' width='2.5pt' width='2.5pt' xmlns='http" nil nil 1)
