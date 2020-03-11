;; -*- lexical-binding: t -*-

(ert-deftest xenops-test-elements--test-get--handlers ()
  (let ((inline-math-handlers (xenops-elements-get 'inline-math :handlers))
        (block-math-handlers (xenops-elements-get 'block-math :handlers)))
    (should (and
             inline-math-handlers
             block-math-handlers
             (equal inline-math-handlers block-math-handlers)))))

(ert-deftest xenops-test-elements--test-get--delimiters ()
  (let ((inline-math-delimiters (xenops-elements-get 'inline-math :delimiters))
        (block-math-delimiters (xenops-elements-get 'block-math :delimiters)))
    (should (and
             inline-math-delimiters
             block-math-delimiters
             (not (equal inline-math-delimiters block-math-delimiters))))))
