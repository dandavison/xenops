(defmacro xenops-src-execute-and-do (src-block &rest body)
  `(with-temp-buffer
     (save-excursion (insert ,src-block))
     (let (org-confirm-babel-evaluate)
       (xenops-execute-at-point))
     ,@body))

(ert-deftest xenops-src-execute ()
  (xenops-src-execute-and-do
   "\
#+begin_src emacs-lisp
\(+ 1 1\)
#+end_src
"
   (search-forward "#+RESULTS:\n")
   (should (looking-at ": 2"))))

(ert-deftest xenops-src-execute--raw ()
  (xenops-src-execute-and-do
   "\
#+begin_src emacs-lisp :results raw
\(+ 1 1\)
#+end_src
"
   (search-forward "#+RESULTS:\n")
   (should (looking-at "2"))))

(ert-deftest xenops-minted-execute ()
  (xenops-src-execute-and-do
   "\
\\begin{minted}{emacs-lisp}
\(+ 1 2\)
\\end{minted}
"
   (search-forward "#+RESULTS:\n")
   (should (looking-at ": 3"))))

(ert-deftest xenops-minted-execute--raw ()
  (xenops-src-execute-and-do
   "\
\\begin{minted}{emacs-lisp} :results raw
\(+ 1 2\)
\\end{minted}
"
   (search-forward "#+RESULTS:\n")
   (should (looking-at "3"))))
