(defun xenops-apply-get-next-element--do-test (input-text expected-type)
  (with-temp-buffer
    (save-excursion
      (insert input-text))
    (should (equal (plist-get (xenops-apply-get-next-element (point-max)) :type)
                   expected-type))))

(ert-deftest xenops-apply-get-next-element--footnote-1 ()
  (xenops-apply-get-next-element--do-test
   "Before\\footnote{Simple note contents}. After." 'footnote))

(ert-deftest xenops-apply-get-next-element--footnote-2 ()
  (xenops-apply-get-next-element--do-test
   "Before\\footnote{Note contents containing \textit{italicised} brace-delimited expression}. After."
   'footnote))

(ert-deftest xenops-apply-get-next-element--footnote-2 ()
  (xenops-apply-get-next-element--do-test
   "Before\\footnote{Note contents containing
newline}. After." 'footnote))
