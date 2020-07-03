;; -*- lexical-binding: t -*-

(ert-deftest xenops-parse-inline-math-element ()
  (with-temp-buffer
    (xenops-mode)
    (insert "$x$")
    (goto-char 3)
    (should (xenops-math-parse-element-at-point))))
