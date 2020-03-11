;; -*- lexical-binding: t -*-

(setq xenops-test-math--block-math-example "Before.
\\begin{align*}
e^{2i\pi}
\\end{align*}
After.")

(setq xenops-test-math--inline-math-example--dollar-delimited "Before. $e^{2i\pi}$ After.")

(defun xenops-test-math--assert-image-is-displayed (element)
  "Perform a `render` operation, and assert that the image is present."
  (save-excursion
    (goto-char (plist-get element :begin-marker))
    (let ((ov (xenops-overlay-at-point)))
      (should ov)
      (let ((image (overlay-get ov 'display)))
        (should image)
        (should (equal (image-property image :type) 'svg))
        (should (equal (image-property image :file) xenops-test--example-svg--cache-file))))))

(defun xenops-test-math--assert-image-is-not-displayed (element)
  "Perform a `reveal` operation, and assert that the image has gone."
  (save-excursion
    (goto-char (plist-get element :begin-marker))
    (should (not (xenops-overlay-at-point)))))

(ert-deftest xenops-test-math--test-apply-parse-next-element--block-math ()
  (xenops-test--do-apply-parse-next-element-test
   xenops-test-math--block-math-example :type 'block-math))

(ert-deftest xenops-test-math--test-apply-parse-next-element--inline-math ()
  (xenops-test--do-apply-parse-next-element-test
   xenops-test-math--inline-math-example--dollar-delimited :type 'inline-math))

(ert-deftest xenops-test-math--test-apply-parse-next-element--inline-math-with-newline ()
  (should-error
   (xenops-test--do-apply-parse-next-element-test
    "Since $\partial{\dx}{\d\dot{x}} =
  \frac{2x}{t^3}$, we have that $x(t)$ satisfies
"
    :type 'inline-math)
   :type 'ert-test-failed))

(ert-deftest xenops-test-math--parse-inline-math-from-string ()
  (should (equal (xenops-math-parse-element-from-string "$x$")
                 '(:begin 1 :begin-content 2 :end-content 3 :end 4 :type inline-math
                   :delimiters ("\\$" "\\$")))))

(ert-deftest xenops-test-math--parse-block-math-from-string ()
  (should (equal (xenops-math-parse-element-from-string
                  (s-trim
                   "
\\begin{align*}
  x
\\end{align*}
"))
                 `(:begin 1 :begin-content 15 :end-content 20 :end 32 :type block-math
                   :delimiters ,(car (xenops-elements-get 'block-math :delimiters))))))


(defun xenops-test-math--do-render-and-reveal-test (text)
  "Render a math image overlay and use `xenops-reveal' to remove it."
  (with-temp-buffer
    (xenops-mode)
    (insert text)
    (goto-char (point-min))
    (let ((element (xenops-apply-parse-next-element)))
      (xenops-element-create-marker element)

      ;; render
      (save-excursion
        (goto-char (point-min))
        (xenops-render))
      (xenops-test-math--assert-image-is-displayed element)

      ;; reveal
      (save-excursion
        (goto-char (point-min))
        (xenops-reveal))
      (xenops-test-math--assert-image-is-not-displayed element))))

(ert-deftest xenops-test-math--test-render-and-reveal--inline-math--dollar-delimited ()
  "Test render and reveal for an inline math element."
  (xenops-test-math--do-render-and-reveal-test
   xenops-test-math--inline-math-example--dollar-delimited))

(ert-deftest xenops-test-math--test-render-and-reveal--block-math ()
  "Test render and reveal for a block math element."
  (xenops-test-math--do-render-and-reveal-test
   xenops-test-math--block-math-example))
(defun xenops-test-math--do-add-cursor-sensor-property-test (insert-1$345$7-fn)
  "See the docstring for `xenops-math-add-cursor-sensor-property'."
  (with-temp-buffer
    (xenops-mode)
    (funcall insert-1$345$7-fn)
    (should (equal (buffer-string) "1$345$7"))
    (font-lock-fontify-buffer)
    (cl-loop for (pos expected-properties) in
             '((1 nil)
               (2 nil)
               (3 (cursor-sensor-functions (xenops-math-handle-element-transgression)))
               (4 (cursor-sensor-functions (xenops-math-handle-element-transgression)))
               (5 (cursor-sensor-functions (xenops-math-handle-element-transgression)))
               (6 (cursor-sensor-functions (xenops-math-handle-element-transgression)
                                           rear-nonsticky (cursor-sensor-functions)))
               (7 nil))
             do (cl-loop for (key expected) in (-partition 2 expected-properties)
                         do
                         (should (equal (get-text-property pos key) expected))))))

(defun xenops-test-math--insert-1$345$7 ()
  "This simulates e.g. the case where auctex is not being used,
and hence $ is bound to `self-insert-command'"
  (insert "1$345$7"))

(defun xenops-test-math--insert-1$345$7-without-TeX-electric-math ()
  "This simulates e.g. the case where auctex is being used, but
the user does not have `TeX-electric-math' set, and therefore
they type the characters in the straightforward sequence."
  (insert "1")
  (TeX-insert-dollar)
  (insert "345")
  (TeX-insert-dollar)
  (insert "7"))

(defun xenops-test-math--insert-1$345$7-with-TeX-electric-math ()
  "This simulates e.g. the case where auctex is being used, but
the user does not have `TeX-electric-math' set, and therefore
they type the characters in the straightforward sequence."
  (insert "1")
  (TeX-insert-dollar)
  (insert "345")
  (forward-char)
  (insert "7"))

(ert-deftest xenops-test-math--test-add-cursor-sensor-property-as-if-no-auctex ()
  (xenops-test-math--do-add-cursor-sensor-property-test #'xenops-test-math--insert-1$345$7))

(ert-deftest xenops-test-math--test-add-cursor-sensor-property-as-if-auctex-without-TeX-electric-math ()
  (let ((TeX-electric-math nil))
    (xenops-test-math--do-add-cursor-sensor-property-test #'xenops-test-math--insert-1$345$7-without-TeX-electric-math)))

(ert-deftest xenops-test-math--test-add-cursor-sensor-property-as-if-auctex-with-TeX-electric-math ()
  (let ((TeX-electric-math '("$" . "$")))
    (xenops-test-math--do-add-cursor-sensor-property-test #'xenops-test-math--insert-1$345$7)
    (xenops-test-math--do-add-cursor-sensor-property-test #'xenops-test-math--insert-1$345$7-with-TeX-electric-math)))

