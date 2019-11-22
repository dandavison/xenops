(ert-deftest xenops-math-parse-element-from-string ()
  (should (equal (xenops-math-parse-element-from-string "$x$")
                 '(:begin 1 :begin-content 2 :end-content 3 :end 4 :type inline-math
                          :delimiters ("\\$" . "\\$"))))
  (should (equal (xenops-math-parse-element-from-string
                  (s-trim
                   "
\\begin{align*}
  x
\\end{align*}
"))
                 '(:begin 1 :begin-content 18 :end-content 19 :end 32 :type block-math
                          :delimiters ("^[ \t]*\\\\begin{align\\*?}" .
                                       "^[ \t]*\\\\end{align\\*?}")))))
