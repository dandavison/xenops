(ert-deftest xenops-image-get-file-name-suggestion ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'buffer-file-name) (lambda () "my-file")))
      (insert "
\\section{sec1}

\\subsection{sub sec 1}

\\subsection{subsec: 2}

\\subsubsection{SubSubsec1}

\\subsubsection{SubSubsec2}
hello
")
      (should (equal (xenops-image-get-file-name-suggestion "hash" "png")
                     "my-file--sec1--subsec-2--subsubsec2--hash.png")))))
