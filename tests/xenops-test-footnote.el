(ert-deftest xenops-test-footnote--test-apply-parse-next-element--footnote-1 ()
  (xenops-test--do-apply-parse-next-element-test
   "Before\\footnote{Simple note contents}. After." :type 'footnote))

(ert-deftest xenops-test-footnote--test-apply-parse-next-element--footnote-2 ()
  (xenops-test--do-apply-parse-next-element-test
   "Before\\footnote{Note contents containing \textit{italicised} brace-delimited expression}. After."
   :type 'footnote))

(ert-deftest xenops-test-footnote--test-apply-parse-next-element--footnote-2 ()
  (xenops-test--do-apply-parse-next-element-test
   "Before\\footnote{Note contents containing
newline}. After." :type 'footnote))
