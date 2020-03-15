;; -*- lexical-binding: t -*-

(ert-deftest xenops-test-src--test-apply-parse-next-element--src ()
  (xenops-test--do-apply-parse-next-element-test
   "Before.
#+begin_src python
def f():
    pass
#+end_src
After." :type 'src :language "python"))

(ert-deftest xenops-test-src--test-apply-parse-next-element--minted ()
  (xenops-test--do-apply-parse-next-element-test
   "Before.
\\begin{minted}{python}
def f():
    pass
\\end{minted}
After." :type 'minted :language "python"))

(defun xenops-test-src--test-parse (buffer-contents element-begin expected-type keyword)
  (xenops-test--with-xenops-render
   buffer-contents
   (forward-char element-begin)
   (should (or (looking-at (caar (xenops-elements-get 'src :delimiters)))
               (looking-at (caar (xenops-elements-get 'minted :delimiters)))))
   (let ((element (xenops-parse-any-element-at-point)))
     (should (equal (plist-get element :type) expected-type)))
   (should (not (xenops-util-parse-image-at (point))))
   (search-forward keyword)
   (goto-char (match-beginning 0))
   ;; (font-lock-fontify-buffer)
   ;; (should (equal (face-at-point) 'font-lock-keyword-face))
   ))

(ert-deftest xenops-test-src--test-parse-minted ()
  (xenops-test-src--test-parse
   "Hello.
\\begin{minted}{emacs-lisp}
 (defun f ())
\\end{minted}
 " 8 'minted "defun"))

(ert-deftest xenops-test-src--test-parse-src ()
  (xenops-test-src--test-parse
   "Hello.
 #+begin_src emacs-lisp
 (defun f ())
 #+end_src
 " 8 'src "defun"))

(defmacro xenops-test-src--with-execute (src-block &rest body)
  `(with-temp-buffer
     (save-excursion (insert ,src-block))
     (let (org-confirm-babel-evaluate)
       (xenops-execute-at-point))
     ,@body))

(ert-deftest xenops-test-src--test-execute ()
  (xenops-test-src--with-execute
   "\
#+begin_src emacs-lisp
\(+ 1 1\)
#+end_src
"
   (search-forward "#+RESULTS:\n")
   (should (looking-at ": 2"))))

(ert-deftest xenops-test-src--test-execute--raw ()
  (xenops-test-src--with-execute
   "\
#+begin_src emacs-lisp :results raw
\(+ 1 1\)
#+end_src
"
   (search-forward "#+RESULTS:\n")
   (should (looking-at "2"))))

(ert-deftest xenops-test-src--test-minted-execute ()
  (xenops-test-src--with-execute
   "\
\\begin{minted}{emacs-lisp}
\(+ 1 2\)
\\end{minted}
"
   (search-forward "#+RESULTS:\n")
   (should (looking-at ": 3"))))

(ert-deftest xenops-test-src--test-minted-execute--raw ()
  (xenops-test-src--with-execute
   "\
\\begin{minted}{emacs-lisp} :results raw
\(+ 1 2\)
\\end{minted}
"
   (search-forward "#+RESULTS:\n")
   (should (looking-at "3"))))
