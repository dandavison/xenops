(ert-deftest xenops-image-get-file-name-suggestion ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'buffer-file-name) (lambda () "my-file")))
      (insert "
\\section{sec1}

\\subsection{subsec1}

\\subsection{subsec2}

\\subsubsection{subsubsec1}

\\subsubsection{subsubsec2}
hello
")
      (should (equal (xenops-image-get-file-name-suggestion "png")
                     "my-file--sec1--subsec2--subsubsec2.png")))))
