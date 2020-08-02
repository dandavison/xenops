;; -*- lexical-binding: t -*-

(setq xenops-test-math--block-math-example "Before.
\\begin{align*}
e^{2i\pi}
\\end{align*}
After.")

(setq xenops-test-math--inline-math-example--dollar-delimited "Before. $e^{2i\pi}$ After.")

(defun xenops-test-math--assert-image-is-displayed (element)
  "Perform a `render` operation, and assert that the image is present."
  (let ((ov (xenops-element-overlay-get element 'xenops-overlay)))
    (should ov)
    (let ((image (overlay-get ov 'display)))
      (should image)
      (should (equal (image-property image :type) 'svg))
      (should (equal (image-property image :file) xenops-test--example-svg--cache-file)))))

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

(ert-deftest xenops-test-math--parse-inline-math--dollar-delimited ()
  (should (equal (xenops-math-parse-element-from-string "$x$")
                 '(:type inline-math
                   :begin 1 :begin-content 2 :end-content 3 :end 4
                   :delimiters ("\\$" "\\$")))))

(ert-deftest xenops-test-math--parse-inline-math--paren-delimited ()
  (should (equal (xenops-math-parse-element-from-string "\\(x\\)")
                 `(:type inline-math
                   :begin 1 :begin-content 3 :end-content 4 :end 6
                   :delimiters ,xenops-math-paren-delimited-inline-math-delimiters))))

(ert-deftest xenops-test-math--parse-block-math--align ()
  (should (equal (xenops-math-parse-element-from-string
                  (s-trim
                   "
\\begin{align}
  x
\\end{align}
"))
                 `(:type block-math
                   :begin 1 :begin-content 14 :end-content 19 :end 30
                   :delimiters ,(car (xenops-elements-get 'block-math :delimiters))))))

(ert-deftest xenops-test-math--parse-block-math--align* ()
  (should (equal (xenops-math-parse-element-from-string
                  (s-trim
                   "
\\begin{align*}
  x
\\end{align*}
"))
                 `(:type block-math
                   :begin 1 :begin-content 15 :end-content 20 :end 32
                   :delimiters ,(car (xenops-elements-get 'block-math :delimiters))))))

(ert-deftest xenops-test-math--parse-block-math--equation ()
  (should (equal (xenops-math-parse-element-from-string
                  (s-trim
                   "
\\begin{equation}
  x
\\end{equation}
"))
                 `(:type block-math
                   :begin 1 :begin-content 17 :end-content 22 :end 36
                   :delimiters ,(car (xenops-elements-get 'block-math :delimiters))))))

(ert-deftest xenops-test-math--parse-block-math--equation* ()
  (should (equal (xenops-math-parse-element-from-string
                  (s-trim
                   "
\\begin{equation*}
  x
\\end{equation*}
"))
                 `(:type block-math
                   :begin 1 :begin-content 18 :end-content 23 :end 38
                   :delimiters ,(car (xenops-elements-get 'block-math :delimiters))))))

(ert-deftest xenops-test-math--parse-block-math--square-bracket ()
  (should (equal (xenops-math-parse-element-from-string
                  (s-trim
                   "
\\[
  x
\\]
"))
                 `(:type block-math
                   :begin 1 :begin-content 3 :end-content 8 :end 10
                   :delimiters ,(cadr (xenops-elements-get 'block-math :delimiters))))))

(ert-deftest xenops-test-math--parse-block-math--align--inline ()
  (should (equal (xenops-math-parse-element-from-string
                  (s-trim
                   "
\\begin{align}x\\end{align}
"))
                 `(:type inline-math
                   :begin 1 :begin-content 14 :end-content 15 :end 26
                   :delimiters ,xenops-math-environment-delimited-inline-math-delimiters))))

(ert-deftest xenops-test-math--parse-block-math--align*--inline ()
  (should (equal (xenops-math-parse-element-from-string
                  (s-trim
                   "
\\begin{align*}x\\end{align*}
"))
                 `(:type inline-math
                   :begin 1 :begin-content 15 :end-content 16 :end 28
                   :delimiters ,xenops-math-environment-delimited-inline-math-delimiters))))

(ert-deftest xenops-test-math--parse-block-math--equation--inline ()
  (should (equal (xenops-math-parse-element-from-string
                  (s-trim
                   "
\\begin{equation}x\\end{equation}
"))
                 `(:type inline-math
                   :begin 1 :begin-content 17 :end-content 18 :end 32
                   :delimiters ,xenops-math-environment-delimited-inline-math-delimiters))))

(ert-deftest xenops-test-math--parse-block-math--equation*--inline ()
  (should (equal (xenops-math-parse-element-from-string
                  (s-trim
                   "
\\begin{equation*}x\\end{equation*}
"))
                 `(:type inline-math
                   :begin 1 :begin-content 18 :end-content 19 :end 34
                   :delimiters ,xenops-math-environment-delimited-inline-math-delimiters))))

(ert-deftest xenops-test-math--parse-block-math--square-bracket--inline ()
  (should (equal (xenops-math-parse-element-from-string
                  (s-trim
                   "
\\[x\\]
"))
                 `(:type inline-math
                   :begin 1 :begin-content 3 :end-content 4 :end 6
                   :delimiters ,xenops-math-square-bracket-delimited-inline-math-delimiters))))

(ert-deftest xenops-test-math--parse-block-math--tikz--inline ()
  (should (equal (xenops-math-parse-element-from-string
                  (s-trim
                   "
\\tikz \draw (0,0) rectangle (1,1) (0,0) parabola (1,1);
"))
                 `(:type inline-math
                   :begin 1 :begin-content 6 :end-content 54 :end 55
                   :delimiters ,xenops-math-tikz-inline-math-delimiters))))

(defun xenops-test-math--do-render-and-reveal-test (text &optional command-type)
  "Render a math image overlay and use `xenops-reveal' to remove it."
  (with-temp-buffer
    (xenops-mode)
    (insert text)
    (goto-char (point-min))
    (let ((element (xenops-apply-parse-next-element)))
      (xenops-math-set-marker-on-element element)

      ;; render
      (save-excursion

        (cond
         ((eq 'dwim command-type)
          (goto-char (point-min))
          (call-interactively #'xenops-dwim))
         ((eq 'at-point command-type)
          (goto-char (plist-get element :begin))
          (call-interactively #'xenops-render-at-point))
         (t
          (goto-char (point-min))
          (call-interactively #'xenops-render)))

        (xenops-test-math--assert-image-is-displayed element))

      ;; reveal
      (save-excursion
        (cond
         ((eq 'dwim command-type)
          (goto-char (point-min))
          (let ((current-prefix-arg '(4)))
            (call-interactively #'xenops-dwim)))
         ((eq 'at-point command-type)
          (goto-char (plist-get element :begin))
          (call-interactively #'xenops-reveal-at-point))
         (t
          (goto-char (point-min))
          (call-interactively #'xenops-reveal))))

      (xenops-test-math--assert-image-is-not-displayed element))))

(defun xenops-test-math--do-render-and-reveal-test--malformed-element (text)
  "Test that reveal works when the element has become malformed."
  (with-temp-buffer
    (xenops-mode)
    (insert text)
    (goto-char (point-min))
    (let ((element (xenops-apply-parse-next-element)))
      (xenops-math-set-marker-on-element element)

      ;; render
      (save-excursion
        (goto-char (point-min))
        (call-interactively #'xenops-render))
      (xenops-test-math--assert-image-is-displayed element)

      ;; Alter buffer text so that element is malformed.
      (goto-char (point-min))
      (message "buffer-string is %s" (buffer-string))
      (message "point is %s" (point))
      (should (save-excursion (xenops-apply-parse-next-element)))
      (save-excursion
        (re-search-forward (xenops-elements-delimiter-start-regexp))
        (goto-char (match-beginning 0))
        (insert "X")
        (message "buffer-string is %s" (buffer-string)))
      (xenops-test-math--assert-image-is-displayed element)
      (should (not (save-excursion (xenops-apply-parse-next-element))))

      ;; reveal
      (save-excursion
        (goto-char (point-min))
        (call-interactively #'xenops-reveal))
      (xenops-test-math--assert-image-is-not-displayed element))))

(ert-deftest xenops-test-math--test-render-and-reveal--inline-math--dollar-delimited ()
  "Test render and reveal for an inline math element."
  (xenops-test-math--do-render-and-reveal-test
   xenops-test-math--inline-math-example--dollar-delimited))

(ert-deftest xenops-test-math--test-render-and-reveal--block-math ()
  "Test render and reveal for a block math element."
  (xenops-test-math--do-render-and-reveal-test
   xenops-test-math--block-math-example))

(ert-deftest xenops-test-math--test-render-and-reveal--inline-math--dollar-delimited--at-point ()
  "Test render and reveal for an inline math element."
  (xenops-test-math--do-render-and-reveal-test
   xenops-test-math--inline-math-example--dollar-delimited
   'at-point))

(ert-deftest xenops-test-math--test-render-and-reveal--block-math--at-point ()
  "Test render and reveal for a block math element."
  (xenops-test-math--do-render-and-reveal-test
   xenops-test-math--block-math-example
   'at-point))

(ert-deftest xenops-test-math--test-render-and-reveal--inline-math--dollar-delimited--dwim ()
  "Test render and reveal for an inline math element."
  (xenops-test-math--do-render-and-reveal-test
   xenops-test-math--inline-math-example--dollar-delimited
   'dwim))

(ert-deftest xenops-test-math--test-render-and-reveal--block-math--dwim ()
  "Test render and reveal for a block math element."
  (xenops-test-math--do-render-and-reveal-test
   xenops-test-math--block-math-example
   'dwim))

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
  (call-interactively #'TeX-insert-dollar)
  (insert "345")
  (call-interactively #'TeX-insert-dollar)
  (insert "7"))

(defun xenops-test-math--insert-1$345$7-with-TeX-electric-math ()
  "This simulates e.g. the case where auctex is being used, but
the user does not have `TeX-electric-math' set, and therefore
they type the characters in the straightforward sequence."
  (insert "1")
  (call-interactively #'TeX-insert-dollar)
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

(ert-deftest xenops-test-math--test-render-and-reveal--block-math--malformed-element ()
  "Test render and reveal for a block math element."
  (xenops-test-math--do-render-and-reveal-test--malformed-element
   xenops-test-math--block-math-example))
