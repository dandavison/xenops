;; -*- lexical-binding: t -*-

(ert-deftest xenops-test-image--test-apply-parse-next-element--image ()
  (xenops-test--do-apply-parse-next-element-test
   "Before.
\\includegraphics[width=400pt]{img/zz.png}
After." :type 'image :path (expand-file-name "img/zz.png")))

(ert-deftest xenops-test-image--suggest-file-name ()
  (with-temp-buffer
    (cl-letf* (((symbol-function 'buffer-name) (lambda () "my-file.tex"))
               ((symbol-function 'buffer-file-name) (lambda () (concat "/a/b/c/" (buffer-name)))))
      (insert "
\\section{sec1}

\\subsection{sub sec 1}

\\subsection{subsec: 2}

\\subsubsection{SubSubsec1}

\\subsubsection{SubSubsec2}
hello
")
      (should (equal (xenops-image-suggest-file-name "--hash.png")
                     "my-file--sec1--subsec-2--subsubsec2--hash.png")))))
