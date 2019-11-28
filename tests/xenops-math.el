(ert-deftest xenops-math-parse-inline-math-from-string ()
  (should (equal (xenops-math-parse-element-from-string "$x$")
                 '(:begin 1 :begin-content 2 :end-content 3 :end 4 :type inline-math
                          :delimiters ("\\$" "\\$")))))

(ert-deftest xenops-math-parse-block-math-from-string ()
  (should (equal (xenops-math-parse-element-from-string
                  (s-trim
                   "
\\begin{align*}
  x
\\end{align*}
"))
                 '(:begin 1 :begin-content 18 :end-content 19 :end 32 :type block-math
                          :delimiters ("^[ \t]*\\\\begin{align\\*?}"
                                       "^[ \t]*\\\\end{align\\*?}")))))


(ert-deftest xenops-math-create-latex-document-for-fragment--single-file ()
  (let ((file (make-temp-file "xenops-test" nil ".tex")))
    (f-write-text xenops-test-math-single-file-contents 'utf-8 file)
    (find-file file)
    (xenops-mode)
    (let ((preamble (xenops-math-get-latex-preamble-lines)))
      (should
       (member "\\newcommand{\\CommandInFile}{\\text{CommandInFileOutput}}" preamble))
      (should
       (member "\\usepackage{amsmath}" preamble)))))

(ert-deftest xenops-math-create-latex-document-for-fragment--multi-file ()
  (let ((child-file (make-temp-file "xenops-test-child" nil ".tex"))
        (master-file (make-temp-file "xenops-test-master" nil ".tex")))
    (f-write-text xenops-test-math-child-file-contents 'utf-8 child-file)
    (f-write-text xenops-test-math-master-file-contents 'utf-8 master-file)
    (find-file child-file)
    (xenops-mode)
    (let* ((TeX-master master-file)
           (preamble (xenops-math-get-latex-preamble-lines)))
      (should
       (member "\\newcommand{\\CommandInMasterFile}{\\text{CommandInMasterFileOutput}}" preamble))
      (should
       (member "\\usepackage{amsmath}" preamble)))))

(setq xenops-test-math-single-file-contents "\\documentclass{article}
\\usepackage{amsmath}
\\newcommand{\\CommandInFile}{\\text{CommandInFileOutput}}
\\begin{document}
Before block.
\\begin{align*}
  block 1 \\\\
  \\CommandInFile
\\end{align*}
After block.
\\end{document}
")

(setq xenops-test-math-master-file-contents "\\documentclass{article}
\\usepackage{amsmath}
\\newcommand{\\CommandInMasterFile}{\\text{CommandInMasterFileOutput}}
\\begin{document}
\\include{child}
\\end{document}
")

(setq xenops-test-math-child-file-contents "Before blocks.
\\begin{align*}
  block 1 \\\\
  \\CommandInMasterFile
\\end{align*}

\\begin{align*}
  block 2 \\\\
  \\CommandInChildFile
\\end{align*}

After blocks.
")
