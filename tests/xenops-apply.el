(defun xenops-apply-get-next-element--do-test (input-text &rest expected-properties)
  (with-temp-buffer
    (save-excursion (insert input-text))
    (let ((element (xenops-apply-get-next-element
                    (xenops-elements-delimiter-start-regexp) (point-max))))
      (cl-loop for (k v) in (-partition 2 expected-properties)
               do (should (equal (plist-get element k) v))))))

(ert-deftest xenops-apply-get-next-element--block-math ()
  (xenops-apply-get-next-element--do-test
   "Before.
\\begin{align*}
e^{2i\pi}
\\end{align*}
After." :type 'block-math))

(ert-deftest xenops-apply-get-next-element--inline-math ()
  (xenops-apply-get-next-element--do-test
   "Before. $e^{2i\pi}$ After." :type 'inline-math))

(ert-deftest xenops-apply-get-next-element--inline-math-with-newline ()
  (xenops-apply-get-next-element--do-test
   "Since $\partial{\dx}{\d\dot{x}} =
  \frac{2x}{t^3}$, we have that $x(t)$ satisfies
"
   :type 'inline-math))

(ert-deftest xenops-apply-get-next-element--src ()
  (xenops-apply-get-next-element--do-test
   "Before.
#+begin_src python
def f():
    pass
#+end_src
After." :type 'src :language "python"))

(ert-deftest xenops-apply-get-next-element--minted ()
  (xenops-apply-get-next-element--do-test
   "Before.
\\begin{minted}{python}
def f():
    pass
\\end{minted}
After." :type 'minted :language "python"))

(ert-deftest xenops-apply-get-next-element--image ()
  (xenops-apply-get-next-element--do-test
   "Before.
\\includegraphics[width=400pt]{img/zz.png}
After." :type 'image :path (expand-file-name "img/zz.png")))

(ert-deftest xenops-apply-get-next-element--footnote-1 ()
  (xenops-apply-get-next-element--do-test
   "Before\\footnote{Simple note contents}. After." :type 'footnote))

(ert-deftest xenops-apply-get-next-element--footnote-2 ()
  (xenops-apply-get-next-element--do-test
   "Before\\footnote{Note contents containing \textit{italicised} brace-delimited expression}. After."
   :type 'footnote))

(ert-deftest xenops-apply-get-next-element--footnote-2 ()
  (xenops-apply-get-next-element--do-test
   "Before\\footnote{Note contents containing
newline}. After." :type 'footnote))
