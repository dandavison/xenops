(setq xenops-test-math-latex--single-file-contents "\\documentclass{article}
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

(setq xenops-test-math-latex--master-file-contents "\\documentclass{article}
\\usepackage{amsmath}
\\newcommand{\\CommandInMasterFile}{\\text{CommandInMasterFileOutput}}
\\begin{document}
\\include{child}
\\end{document}
")

(setq xenops-test-math-latex--child-file-contents "Before blocks.
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

(ert-deftest xenops-test-math-latex--create-latex-document-for-fragment--single-file ()
  (let ((file (make-temp-file "xenops-test" nil ".tex")))
    (f-write-text xenops-test-math-latex--single-file-contents 'utf-8 file)
    (find-file file)
    (xenops-mode)
    (let ((preamble (xenops-math-latex-get-preamble-lines)))
      (should
       (member "\\newcommand{\\CommandInFile}{\\text{CommandInFileOutput}}" preamble))
      (should
       (member "\\usepackage{amsmath}" preamble)))
    (f-delete file)))

(ert-deftest xenops-test-math-latex--create-latex-document-for-fragment--multi-file ()
  (let ((child-file (make-temp-file "xenops-test-child" nil ".tex"))
        (master-file (make-temp-file "xenops-test-master" nil ".tex")))
    (f-write-text xenops-test-math-latex--child-file-contents 'utf-8 child-file)
    (f-write-text xenops-test-math-latex--master-file-contents 'utf-8 master-file)
    (find-file child-file)
    (xenops-mode)
    (let* ((TeX-master master-file)
           (preamble (xenops-math-latex-get-preamble-lines)))
      (should
       (member "\\newcommand{\\CommandInMasterFile}{\\text{CommandInMasterFileOutput}}" preamble))
      (should
       (member "\\usepackage{amsmath}" preamble)))
    (f-delete child-file)
    (f-delete master-file)))

;;; TODO: The following tests are all skipped: code paths that use aio cannot be tested under ert.
(defun xenops-test-math-latex--do-image-test (buffer-contents element-begin expected-type)
  (ert-skip "Test disabled because LaTeX rendering is now asynchronous.")
  (xenops-test--with-xenops-render
   buffer-contents
   (forward-char element-begin)
   (let ((element (xenops-parse-any-element-at-point)))
     (should (equal (plist-get element :type) expected-type)))
   (let ((image (xenops-util-parse-image-at (point))))
     (should (equal (image-property image :type) 'svg)))))

(ert-deftest xenops-test-math-latex--test-render--inline-math ()
  (xenops-test-math-latex--do-image-test "123$e^{2i\\pi}$maths." 4 'inline-math))

(ert-deftest xenops-test-math-latex--test-render--block-math ()
  (xenops-test-math-latex--do-image-test
   "Hello.
\\begin{align*}
  e^{2i\\pi}
\\end{align*}
" 8 'block-math))

(ert-deftest xenops-test-math-latex--test-render--table ()
  (xenops-test-math-latex--do-image-test
   "This is a table.
\\begin{tabular}{c||c|c|c|c|c|}
    & e & a & b & c & d\\\\
  \\hline
  \\hline
  e & e & a & b & c & d\\\\
  a & a & b & c & d & e\\\\
  b & b & c & d & e & a\\\\
  c & c & d & e & a & b\\\\
  d & d & e & a & b & c
\\end{tabular}\\\\
" 18 'table))

(ert-deftest xenops-test-math-latex--test-render--table-R ()
  (xenops-test-math-latex--do-image-test
   "% latex table generated in R 3.6.1 by xtable 1.8-4 package
% Tue Dec 10 23:53:04 2019
\\begin{table}[ht]
\\centering
\\begin{tabular}{rrrrr}
  \\hline
 & Estimate & Std. Error & t value & Pr($>$$|$t$|$) \\\\
  \\hline
(Intercept) & -0.2814 & 0.1055 & -2.67 & 0.0090 \\\\
  x & 0.9928 & 0.1014 & 9.79 & 0.0000 \\\\
   \\hline
\\end{tabular}
\\end{table}
" 87 'table))
