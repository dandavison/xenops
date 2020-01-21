;; Override minor-mode activation function to not make `xenops-doctor' checks.
(setq xenops-mode-orig (symbol-function 'xenops-mode))
(defun xenops-mode (&rest args)
  (cl-letf (((symbol-function 'xenops-doctor)
             (lambda (&rest args) (interactive) nil)))
    (funcall xenops-mode-orig args)))
