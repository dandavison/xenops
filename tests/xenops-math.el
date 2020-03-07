;; -*- lexical-binding: t -*-

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
                 `(:begin 1 :begin-content 15 :end-content 20 :end 32 :type block-math
                   :delimiters ,(car (xenops-elements-get 'block-math :delimiters))))))


(ert-deftest xenops-math-create-latex-document-for-fragment--single-file ()
  (let ((file (make-temp-file "xenops-test" nil ".tex")))
    (f-write-text xenops-test-math-single-file-contents 'utf-8 file)
    (find-file file)
    (xenops-mode)
    (let ((preamble (xenops-math-latex-get-preamble-lines)))
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
           (preamble (xenops-math-latex-get-preamble-lines)))
      (should
       (member "\\newcommand{\\CommandInMasterFile}{\\text{CommandInMasterFileOutput}}" preamble))
      (should
       (member "\\usepackage{amsmath}" preamble)))))

(defun xenops-math--do-add-cursor-sensor-property-test (insert-1$345$7-fn)
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

(defun xenops-test--math--insert-1$345$7 ()
  "This simulates e.g. the case where auctex is not being used,
and hence $ is bound to `self-insert-command'"
  (insert "1$345$7"))

(defun xenops-test--math--insert-1$345$7-without-TeX-electric-math ()
  "This simulates e.g. the case where auctex is being used, but
the user does not have `TeX-electric-math' set, and therefore
they type the characters in the straightforward sequence."
  (insert "1")
  (TeX-insert-dollar)
  (insert "345")
  (TeX-insert-dollar)
  (insert "7"))

(defun xenops-test--math--insert-1$345$7-with-TeX-electric-math ()
  "This simulates e.g. the case where auctex is being used, but
the user does not have `TeX-electric-math' set, and therefore
they type the characters in the straightforward sequence."
  (insert "1")
  (TeX-insert-dollar)
  (insert "345")
  (forward-char)
  (insert "7"))

(ert-deftest xenops-math-add-cursor-sensor-property-as-if-no-auctex ()
  (xenops-math--do-add-cursor-sensor-property-test #'xenops-test--math--insert-1$345$7))

(ert-deftest xenops-math-add-cursor-sensor-property-as-if-auctex-without-TeX-electric-math ()
  (let ((TeX-electric-math nil))
    (xenops-math--do-add-cursor-sensor-property-test #'xenops-test--math--insert-1$345$7-without-TeX-electric-math)))

(ert-deftest xenops-math-add-cursor-sensor-property-as-if-auctex-with-TeX-electric-math ()
  (let ((TeX-electric-math '("$" . "$")))
    (xenops-math--do-add-cursor-sensor-property-test #'xenops-test--math--insert-1$345$7)
    (xenops-math--do-add-cursor-sensor-property-test #'xenops-test--math--insert-1$345$7-with-TeX-electric-math)))

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
