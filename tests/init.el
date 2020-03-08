;; Override minor-mode activation function to not make `xenops-doctor' checks.
(setq xenops-mode-orig (symbol-function 'xenops-mode))
(defun xenops-mode (&rest args)
  (cl-letf (((symbol-function 'xenops-doctor)
             (lambda (&rest args) (interactive) nil)))
    (latex-mode)
    (funcall xenops-mode-orig args)))


;; Mock this function since these face attributes are not set when the tests are running.
(defun org-latex-color (attr)
  (case attr
    (:foreground "0,0,0")
    (:background "1,1,1")
    (t (error "Unexpected input: %s" attr))))
